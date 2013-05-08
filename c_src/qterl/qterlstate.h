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

#ifndef QTERLSTATE_H
#define QTERLSTATE_H

#include "qteabstractstate.h"
#include "qterl_drv.h"

class QtErlState : public QtEAbstractState
{
public:
  QtErlState(QtEStateId state_id, ErlDrvPort port, ei_x_buff *ref);

  void notify(const char *event, const char *tag = 0, const char *key = 0, const char *value = 0);
  void notify(const char *event, const char *tag, const char *key, QStringList const &value);

private:
  ErlDrvPort dp;
  ei_x_buff r;

  int send(ei_x_buff *x);
};

#endif // QTERLSTATE_H
