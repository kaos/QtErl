/**
 * Copyright 2013 Andreas Stenius
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include <string.h>
#include "qterl.h"

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
  (char *)"qterl",            /* name supplied as command
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


static int qte_init(void)
{
  static int argc = 1;
  static char *argv[] = { (char *)"QtErl" };

  // TODO: check that we have SMP support, fail if not.

  return QtErl::init(argc, argv);
}

static ErlDrvData qte_start(ErlDrvPort port, char *command)
{
  QtErl *q = new (std::nothrow) QtErl(port);
  if (!q)
  {
    erl_errno = ENOMEM;
    return ERL_DRV_ERROR_ERRNO;
  }

  q->open_ref("start");

  char *p = strchr(command, ' ');
  if (p++ && *p)
    q->LoadUI(p);
  else
  {
    QtErlState *s = q->new_state();
    s->notifyOK();
    delete s;
  }

  q->close_ref();

  return (ErlDrvData)q;
}

static void qte_stop(ErlDrvData drv_data)
{
  QtErl *q = (QtErl *) drv_data;
  if (q)
    delete q;
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
  QtErl::close();
}

static int qte_decode_string(char *buf, int *index, char **dst)
{
  int type, size;

  if (*dst)
  {
    driver_free(*dst);
    *dst = NULL;
  }

  ei_get_type(buf, index, &type, &size);

  if (!size)
    return ei_skip_term(buf, index);

  if (type != ERL_STRING_EXT)
    return -1;

  *dst = (char *)driver_alloc(size + 1);
  return ei_decode_string(buf, index, *dst);
}

// TODO: the qte_control function is in dire need of refactoring and overall improvement
static ErlDrvSSizeT qte_control(ErlDrvData drv_data,
                                unsigned int command,
                                char *buf, ErlDrvSizeT len,
                                char **rbuf, ErlDrvSizeT rlen)
{
  (void)len; (void)rlen;
  int ret, pos;
  int arity;
  char *name, *signal, *data;

  ei_term t;
  erlang_ref ref;
  QtErl *q = (QtErl *) drv_data;
  QtEArgumentList *args;

  args = NULL;
  name = signal = data = NULL;
  ret = pos = 0;
  (*rbuf)[ret++] = '0';

  switch (command)
  {
    case QTERL_LOAD_UI:
      {
        // {#Ref, {ParentName, Data}}

        // version
        (*rbuf)[ret - 1]++;
        if (ei_decode_version(buf, &pos, NULL))
          break;
        // {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;
        // #Ref
        (*rbuf)[ret - 1]++;
        if (ei_decode_ref(buf, &pos, &ref))
          break;

        q->open_ref(&ref);
        // {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;
        // ParentName
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &name))
          break;
        // Data
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &data) || !data)
          break;
        // } }

        (*rbuf)[ret - 1]++;
        // post load ui event to main Qt thread
        q->LoadUI(data, name);
        ret = 0;
        break;
      }

    case QTERL_CONNECT:
      {
        // {#Ref, {Name, Signal}}

        // version
        (*rbuf)[ret - 1]++;
        if (ei_decode_version(buf, &pos, NULL))
          break;
        // {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;
        // #Ref
        (*rbuf)[ret - 1]++;
        if (ei_decode_ref(buf, &pos, &ref))
          break;

        q->open_ref(&ref);
        // {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;
        // Name
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &name))
          break;
        // Signal
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &signal))
          break;
        // } }

        (*rbuf)[ret - 1]++;
        // post connect event to main Qt thread
        q->Connect(name, signal);
        ret = 0;
        break;
      }

    case QTERL_INVOKE:
      {
        // {#Ref, {Name::string(), Method::string(), Args::[term()] }}

        // (1) version
        (*rbuf)[ret - 1]++;
        if (ei_decode_version(buf, &pos, NULL))
          break;
        // (2) {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 2)
          break;
        // (3) #Ref
        (*rbuf)[ret - 1]++;
        if (ei_decode_ref(buf, &pos, &ref))
          break;

        q->open_ref(&ref);
        // (4) {
        (*rbuf)[ret - 1]++;
        if (ei_decode_tuple_header(buf, &pos, &arity) || arity != 3)
          break;
        // (5) Name
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &name))
          break;
        // (6) Method
        (*rbuf)[ret - 1]++;
        if (qte_decode_string(buf, &pos, &signal))
          break;
        // (7) Args
        (*rbuf)[ret - 1]++;
        if (0 > ei_decode_ei_term(buf, &pos, &t))
            break;

        // (7)(type)
        (*rbuf)[ret++] = t.ei_type;
        if (ERL_STRING_EXT == t.ei_type)
        {
          // (7)('k')(0)
          (*rbuf)[ret++] = '0';
          if (t.size >= (int)sizeof(t.value.atom_name))
            break;

          // (7)('k')(1)
          (*rbuf)[ret - 1]++;
          if (ei_decode_string(buf, &pos, t.value.atom_name))
            break;

          args = new QtEArgumentList;
          for (int i = 0; i < t.size; i++)
          {
            // (7)('k')(n)
            (*rbuf)[ret - 1]++;
            args->append(QTE_ARG(int, (int)t.value.atom_name[i]));
          }
        }
        else if (ERL_LIST_EXT == t.ei_type)
        {
          arity = t.arity;
          args = new QtEArgumentList;

          // (7)('l')(arity)(0)
          (*rbuf)[ret++] = '0' + arity;
          (*rbuf)[ret++] = '0';
          for (int i = 0; i < arity; i++)
          {
            // (7)('l')(arity)(n)
            (*rbuf)[ret - 1]++;
            switch (ei_decode_ei_term(buf, &pos, &t))
            {
              case 0:
                switch (t.ei_type)
                {
                  case ERL_STRING_EXT:
                    if (((t.size + 1) >= (int)sizeof(t.value.atom_name)) ||
                        ei_decode_string(buf, &pos, t.value.atom_name))
                      break;

                    args->append(QTE_ARG_NEW(QString, t.value.atom_name));
                    break;
                }
                break;

              case 1:
                switch (t.ei_type)
                {
                  case ERL_SMALL_INTEGER_EXT:
                  case ERL_INTEGER_EXT:
                    args->append(QTE_ARG(long, t.value.i_val));
                    break;
                  case ERL_FLOAT_EXT:
                  case NEW_FLOAT_EXT:
                    args->append(QTE_ARG(double, t.value.d_val));
                    break;
                }
                break;

              case -1:
                break;
            }
          }
        }
        else if (ERL_NIL_EXT != t.ei_type)
          break;

        // } }

        // (7)(type)...(1)
        (*rbuf)[ret++] = '1';

        // post invoke event to main Qt thread
        q->Invoke(name, signal, args);

        args = NULL;
        ret = 0;
        break;
      }

    default:
      (*rbuf)[ret++] = (char)command;
      break;
  }

  q->close_ref();

  if (name) driver_free(name);
  if (signal) driver_free(signal);
  if (data) driver_free(data);
  if (args) delete args;

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
