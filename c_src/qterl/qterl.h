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

#ifndef QTERL_H
#define QTERL_H

#include <new>
#include "qte.h"
#include "qterl_drv.h"
#include "qterlstate.h"
#include "qteargument.h"
#include "qteobjectproxy.h"

class QtErl
{
  friend class QtErlState;
  friend void *qterl_main_thread(void *);

public:
  QtErl(ErlDrvPort port);
  virtual ~QtErl();

  void open_ref(const char *ref);
  void open_ref(erlang_ref *ref);
  void close_ref();
  QtErlState *new_state();

  void LoadUI(const char *src, const char *parent = 0);
  void Connect(const char *name, const char *signal);
  void Invoke(const char *name, const char *method, QtEArgumentList *args);

  static int init(int argc, char ** argv);
  static void close();

  static void *operator new(size_t size, const std::nothrow_t &) throw();
  static void operator delete(void *ptr) throw();

private:
  ErlDrvPort dp;
  ei_x_buff r;
  QtEStateId id;
  QteObjectProxy op;

  static ErlDrvTid main_thread;
  static ErlDrvCond *cond;
  static ErlDrvMutex *mutex;
  static QtE *qte;
  static QtEStateId __counter;

  // hide these versions
  static void *operator new(size_t size) throw(std::bad_alloc);
  static void *operator new[](size_t size) throw(std::bad_alloc);
  static void *operator new[](size_t size, const std::nothrow_t &) throw();
  static void operator delete[](void *ptr) throw();
};

#endif // QTERL_H
