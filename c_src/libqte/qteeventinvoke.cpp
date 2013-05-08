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

#include <qmetaobject.h>
//#include <qmetatype.h>
#include "qteeventinvoke.h"

QtEEventInvoke::QtEEventInvoke(QtEAbstractState *state, const char *name, const char *method, QtEArgumentList *args)
  : QtEEvent(state), n(name), m(method), a(args)
{
}

QtEEventInvoke::~QtEEventInvoke()
{
  if (a)
    delete a;
}

bool QtEEventInvoke::execute(QtE *qte)
{
  QObject *o = qte->findObject(getState(), n);

  if (!o)
  {
    getState()->notifyError("object_not_found", n.toLocal8Bit().constData());
    return true;
  }

  const QMetaObject *mo = o->metaObject();
  int idx = mo->indexOfMethod(m.toLocal8Bit().constData());
  if (-1 == idx)
  {
    getState()->notifyError("method_not_found",
                            mo->className(),
                            m.toLocal8Bit().constData());
    return true;
  }

  QMetaMethod mm = mo->method(idx);
  QtEArgument *qa[10] = { NULL };

  if (a)
    for (int i = 0; i < 10; i++)
      qa[i] = a->value(i, NULL);

  if (mm.invoke(o,
        QTE_Q_ARG(qa[0]), QTE_Q_ARG(qa[1]), QTE_Q_ARG(qa[2]), QTE_Q_ARG(qa[3]), QTE_Q_ARG(qa[4]),
        QTE_Q_ARG(qa[5]), QTE_Q_ARG(qa[6]), QTE_Q_ARG(qa[7]), QTE_Q_ARG(qa[8]), QTE_Q_ARG(qa[9])))
  {
    getState()->notifyOK();
  }
  else
  {
    getState()->notifyError("invoke_failed",
                            mo->className(),
                            mm.typeName());
  }

  return true;
}
