
#include <string.h>
#include "qterl.h"
#include "qterl_commands.h"


extern "C" {
  // need to wrap erl_driver.h as that is not properly handled on windows
  // for the DRIVER_INIT macro
  #include <erl_driver.h>
}

static struct {
  int argc;
  char *argv[10];
  ErlDrvTid main_thread;
  QtErl *instance;
  ErlDrvCond *cond;
  ErlDrvMutex *mutex;
} qte;

struct qte_state_s {
  ErlDrvPort port;
  ei_x_buff ext;
  qte_ref_t *ref;
};

static int qte_init(void);
static ErlDrvData qte_start(ErlDrvPort port, char *command);
static void qte_stop(ErlDrvData drv_data);
static void qte_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);
static void qte_ready_input(ErlDrvData drv_data, ErlDrvEvent event);
static void qte_ready_output(ErlDrvData drv_data, ErlDrvEvent event);
static void qte_finish(void);
static ErlDrvSSizeT qte_control(ErlDrvData drv_data,
                                unsigned int command,
                                char *buf, ErlDrvSizeT len,
                                char **rbuf, ErlDrvSizeT rlen);
static void qte_ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data);
static void qte_flush(ErlDrvData drv_data);
static ErlDrvSSizeT qte_call(ErlDrvData drv_data,
                             unsigned int command,
                             char *buf, ErlDrvSizeT len,
                             char **rbuf, ErlDrvSizeT rlen,
                             unsigned int *flags);

static ErlDrvEntry qterl_drv_entry = {
  qte_init,
                              /* called at system start up for statically
                                 linked drivers, and after loading for
                                 dynamically loaded drivers */
  qte_start,
                              /* called when open_port/2 is invoked.
                                 return value -1 means failure. */
  qte_stop,
                              /* called when port is closed, and when the
                                 emulator is halted. */
  qte_output,
                              /* called when we have output from erlang to
                                 the port */
  qte_ready_input,
                              /* called when we have input from one of
                                 the driver's handles */
  qte_ready_output,
                              /* called when output is possible to one of
                                 the driver's handles */
  (char *)"QtErl",            /* name supplied as command
                                 in open_port XXX ? */
  qte_finish,
                              /* called before unloading the driver -
                                 DYNAMIC DRIVERS ONLY */
  NULL,                       /* Reserved -- Used by emulator internally */
  qte_control,
                              /* "ioctl" for drivers - invoked by
                                                port_control/3 */
  NULL, //void (*timeout)(ErlDrvData drv_data);	/* Handling of timeout in driver */
  NULL, //void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
                              /* called when we have output from erlang
                                 to the port */
  qte_ready_async,
  qte_flush,
                              /* called when the port is about to be
                                 closed, and there is data in the
                                 driver queue that needs to be flushed
                                 before 'stop' can be called */
  qte_call,
                              /* Works mostly like 'control',
                                                a synchronous
                                                call into the driver. */
  NULL, /*void (*event)(ErlDrvData drv_data, ErlDrvEvent event,
                ErlDrvEventData event_data);*/
                              /* Called when an event selected by
                                 driver_event() has occurred */
  (int)ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  0,                          /* ERL_DRV_FLAGs */
  NULL,                       /* Reserved -- Used by emulator internally */
  NULL, //void (*process_exit)(ErlDrvData drv_data, ErlDrvMonitor *monitor);
                              /* Called when a process monitor fires */
  NULL //void (*stop_select)(ErlDrvEvent event, void* reserved);
                              /* Called on behalf of driver_select when
                                 it is safe to release 'event'. A typical
                                 unix driver would call close(event) */
};


extern "C" {
  DRIVER_INIT(QtErl)
  {
    return &qterl_drv_entry;
  }
}

static QtErl *QtErl_Instance()
{
  while (!qte.instance)
  {
    if (!(qte.cond && qte.mutex))
      return NULL;

    erl_drv_cond_wait(qte.cond, qte.mutex);
  }

  return qte.instance;
}

static void *qte_main_thread(void *)
{
  QApplication app(qte.argc, qte.argv);
  app.setQuitOnLastWindowClosed(false);

  qte.instance = new QtErl(&app);
  erl_drv_cond_broadcast(qte.cond);

  return (void *)app.exec();
}

static int qte_init(void)
{
  // TODO: check that we have SMP support, fail if not.

  qte.cond = erl_drv_cond_create((char *)"QtErl.cond");
  qte.mutex = erl_drv_mutex_create((char *)"Qterl.mutex");

  qte.argc = 1;
  qte.argv[0] = (char *)"QtErl";

  return erl_drv_thread_create(qte.argv[0],
                               &qte.main_thread,
                               qte_main_thread,
                               NULL, NULL);
}

static ErlDrvData qte_start(ErlDrvPort port, char *command)
{
  QtErl *q = QtErl_Instance();

  if (!q)
    return ERL_DRV_ERROR_GENERAL;

  qte_state_t state = (qte_state_t)driver_alloc(sizeof(struct qte_state_s));
  if (!state)
  {
    erl_errno = ENOMEM;
    return ERL_DRV_ERROR_ERRNO;
  }

  state->port = port;
  state->ref = NULL;
  QTE_OPEN_REF(state, "start");

  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  char *p = strchr(command, ' ');
  if (p++ && *p)
    q->postLoadUI(state, p);
  else
    QTE_REF_SEND(state, state->ref, "ok");

  QTE_CLOSE_REF(state);
  return (ErlDrvData)state;
}

static void qte_stop(ErlDrvData drv_data)
{
  qte_state_t state = (qte_state_t) drv_data;
  QtErl *q = QtErl_Instance();
  if (q)
    q->clear(state);
  qte_close_ref(state);
  driver_free(drv_data);
}

static void qte_output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
  (void)drv_data; (void)buf; (void)len;
}

static void qte_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
  (void)drv_data; (void)event;
}

static void qte_ready_output(ErlDrvData drv_data, ErlDrvEvent event)
{
  (void)drv_data; (void)event;
}

static void qte_finish(void)
{
  QApplication::exit(0);
  erl_drv_thread_join(qte.main_thread, NULL);
  erl_drv_cond_destroy(qte.cond);
  erl_drv_mutex_destroy(qte.mutex);
}

static int qte_decode_string(char *buf, int *index, char *dst, int len)
{
  int type, size;
  ei_get_type(buf, index, &type, &size);
  if (size >= len)
    return size;

  return ei_decode_string(buf, index, dst);
}

static ErlDrvSSizeT qte_control(ErlDrvData drv_data,
                                unsigned int command,
                                char *buf, ErlDrvSizeT len,
                                char **rbuf, ErlDrvSizeT rlen)
{
  (void)len; (void)rlen;
  qte_state_t state = (qte_state_t) drv_data;

  int ret = 1;
  (*rbuf)[ret] = '0';

  switch (command)
  {
    case QTERL_LOAD_UI:
      (*rbuf)[ret]++;
      break;

    case QTERL_CONNECT:
      {
        int pos = 0;
        int arity;
        char name[128];
        char signal[64];
        erlang_ref ref;

        // {#Ref, {Name, Signal}}

        (*rbuf)[ret]++;
        if (ei_decode_version(buf, &pos, NULL))
          break;

        (*rbuf)[ret]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;

        (*rbuf)[ret]++;
        if (ei_decode_ref(buf, &pos, &ref))
          break;

        QTE_OPEN_EREF(state, &ref);

        (*rbuf)[ret]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;

        (*rbuf)[ret]++;
        if (qte_decode_string(buf, &pos, name, sizeof(name)))
          break;

        (*rbuf)[ret]++;
        if (qte_decode_string(buf, &pos, signal, sizeof(signal)))
          break;

        (*rbuf)[ret]++;
        QtErl *q = QtErl_Instance();
        if (!q)
          break;

        q->postConnect(state, name, signal);
        ret = 0;
        break;
      }

    default:
      (*rbuf)[++ret] = (char)command;
      break;
  }

  QTE_CLOSE_REF(state);
  return ret;
}

static void qte_ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data)
{
  (void)drv_data; (void)thread_data;
}

static void qte_flush(ErlDrvData drv_data)
{
  (void)drv_data;
}

static ErlDrvSSizeT qte_call(ErlDrvData drv_data,
                             unsigned int command,
                             char *buf, ErlDrvSizeT len,
                             char **rbuf, ErlDrvSizeT rlen,
                             unsigned int *flags)
{
  (void)drv_data; (void)command; (void)buf; (void)len;
  (void)rbuf; (void)rlen; (void)flags;
  return 0;
}

ei_x_buff *qte_prepare_ext(qte_state_t state)
{
  ei_x_new(&state->ext);
  return &state->ext;
}

qte_ref_t *qte_prepare_ref(qte_state_t state, qte_ref_t *in)
{
  qte_free_ref(state->ref);
  ei_x_new(in);
  state->ref = in;
  return state->ref;
}

qte_ref_t *qte_copy_ref(qte_state_t state, qte_ref_t *dst)
{
  return qte_copy_ref(state->ref, dst);
}

qte_ref_t *qte_copy_ref(qte_ref_t *src, qte_ref_t *dst)
{
  ei_x_new(dst);

  if (!src)
    return NULL;

  ei_x_append(dst, src);
  return dst;
}

void qte_close_ref(qte_state_t state)
{
  qte_free_ref(state->ref);
  state->ref = NULL;
}

void qte_free_ref(qte_ref_t *ref)
{
  if (ref)
    ei_x_free(ref);
}

int qte_send_ext(qte_state_t state, ei_x_buff *ext, qte_ref_t *ref)
{
  ErlDrvTermData term[] = {
    ERL_DRV_EXT2TERM,
    (ErlDrvTermData) (ref ? ref->buff : 0),
    (ErlDrvTermData) (ref ? ref->buffsz : 0),
    ERL_DRV_EXT2TERM,
    (ErlDrvTermData) ext->buff,
    (ErlDrvTermData) ext->buffsz,
    ERL_DRV_TUPLE, 2
  };

  return driver_send_term(
        state->port, driver_connected(state->port),
        &term[ref ? 0 : 3], ref ? 8 : 3
      );
}
