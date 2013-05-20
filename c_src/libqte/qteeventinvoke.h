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

#ifndef QTEEVENTINVOKE_H
#define QTEEVENTINVOKE_H

#include "qteevent.h"
#include "qteargument.h"

class QtEEventInvoke : public QtEEvent
{
  Q_GADGET

public:
  explicit QtEEventInvoke(QtEAbstractState *state,
                          const char *name,
                          const char *method,
                          QtEArgumentList *args);
  virtual ~QtEEventInvoke();
  void execute(QtE *qte);

protected:
  bool executeOn(QtE *qte, QObject *o, bool notify);
  template<typename T>
  bool invokeOnObjWithArgs(QMetaMethod &method, QObject *obj, QtEArgument *args[10], QVariant &result, const char *resultType);

private:
  QString n;
  QString m;
  QtEArgumentList *a;
};

#define ARGS(_a_) \
  QTE_Q_ARG(_a_[0]), QTE_Q_ARG(_a_[1]), QTE_Q_ARG(_a_[2]), QTE_Q_ARG(_a_[3]), QTE_Q_ARG(_a_[4]), \
  QTE_Q_ARG(_a_[5]), QTE_Q_ARG(_a_[6]), QTE_Q_ARG(_a_[7]), QTE_Q_ARG(_a_[8]), QTE_Q_ARG(_a_[9])

#define INVOKE(_t_, _m_, _o_, _a_, _r_) invokeOnObjWithArgs<_t_>(_m_,_o_,_a_,_r_,#_t_)

#endif // QTEEVENTINVOKE_H
