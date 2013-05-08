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

#include <qapplication.h>
#include "qterl.h"
#include "qteeventloadui.h"
#include "qteeventconnect.h"
#include "qteeventinvoke.h"

struct start_args
{
  int argc;
  char **argv;
  ErlDrvCond *cond;
  QtE **qte;
};

static void *qterl_main_thread(void *);

ErlDrvTid QtErl::main_thread;
ErlDrvCond *QtErl::cond;
ErlDrvMutex *QtErl::mutex;
QtE *QtErl::qte;
unsigned long QtErl::__counter;

QtErl::QtErl(ErlDrvPort port) :
  dp(port), id(__counter++)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  while (!qte)
  {
    if (!(cond && mutex))
      return;

    erl_drv_cond_wait(cond, mutex);
  }

  qte->init(id);
}

QtErl::~QtErl()
{
  qte->clear(id);
}

void QtErl::open_ref(const char *ref)
{
  ei_x_new_with_version(&r);
  ei_x_encode_atom(&r, ref);
}

void QtErl::open_ref(erlang_ref *ref)
{
  ei_x_new_with_version(&r);
  ei_x_encode_ref(&r, ref);
}

void QtErl::close_ref()
{
  ei_x_free(&r);
}

void *QtErl::operator new(size_t size, const std::nothrow_t &) throw()
{
  return driver_alloc(size);
}

void QtErl::operator delete(void *ptr) throw()
{
  driver_free(ptr);
}

QtErlState *QtErl::new_state()
{
  ei_x_buff *ref = r.buff ? &r : 0;
  return new QtErlState(id, dp, ref);
}

void QtErl::LoadUI(const char *src, const char *parent)
{
  QApplication::postEvent(
        qte,
        new QtEEventLoadUI(
          new_state(),
          src,
          parent ? qte->findWidget(id, parent) : 0));
}

void QtErl::Connect(const char *name, const char *signal)
{
  QApplication::postEvent(
        qte,
        new QtEEventConnect(
          new_state(),
          name, signal));
}

void QtErl::Invoke(const char *name, const char *method, QtEArgumentList *args)
{
  QApplication::postEvent(
        qte,
        new QtEEventInvoke(
          new_state(),
          name, method, args));
}

void *QtErl::operator new(size_t) throw(std::bad_alloc)
{
  throw std::bad_alloc();
}

void *QtErl::operator new[](size_t) throw(std::bad_alloc)
{
  throw std::bad_alloc();
}

void *QtErl::operator new[](size_t, const std::nothrow_t &) throw()
{
  return NULL;
}

void QtErl::operator delete[](void *) throw()
{
}

int QtErl::init(int argc, char **argv)
{
  static struct start_args a;

  cond = erl_drv_cond_create((char *)"QtErl.cond");
  mutex = erl_drv_mutex_create((char *)"QtErl.mutex");

  a.argc = argc;
  a.argv = argv;
  a.qte = &qte;
  a.cond = cond;

  return erl_drv_thread_create(
        argv[0],
        &main_thread,
        qterl_main_thread,
        &a,
      NULL);
}

void QtErl::close()
{
  QApplication::exit(0);

  erl_drv_thread_join(main_thread, NULL);
  erl_drv_cond_destroy(cond);
  erl_drv_mutex_destroy(mutex);
}

static void *qterl_main_thread(void *args)
{
  struct start_args *a = (struct start_args *)args;

  QApplication app(a->argc, a->argv);
  app.setQuitOnLastWindowClosed(false);

  *(a->qte) = new QtE(&app);
  erl_drv_cond_broadcast(a->cond);

  return (void *)app.exec();
}
