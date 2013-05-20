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
#include "qteeventinvoke.h"
#include "qteobjectproxy.h"

QtEEventInvoke::QtEEventInvoke(QtEAbstractState *state, const char *name, const char *method, QtEArgumentList *args)
  : QtEEvent(state), n(name), m(method), a(args)
{
}

QtEEventInvoke::~QtEEventInvoke()
{
  if (a)
    delete a;
}

void QtEEventInvoke::execute(QtE *qte)
{
  QObject *o = qte->findObject(getState(), n);

  if (!o)
  {
    getState()->notifyError("object_not_found", n.toLocal8Bit().constData());
    return;
  }

  if (!executeOn(o, false))
  {
    QObject *p = qte->getProxy()->newProxyObject(o);
    if (p)
    {
      executeOn(p, true);
      delete p;
    }
    else
      getState()->notifyError("no_proxy_for", o->metaObject()->className());
  }
}

bool QtEEventInvoke::executeOn(QObject *o, bool notify)
{
  const QMetaObject *mo = o->metaObject();
  int idx = mo->indexOfMethod(m.toLocal8Bit().constData());
  if (-1 == idx)
  {
    if (notify)
      getState()->notifyError("method_not_found",
                              mo->className(),
                              m.toLocal8Bit().constData());
    return false;
  }

  QMetaMethod mm = mo->method(idx);
  QtEArgument *qa[10] = { NULL };

  if (a)
    for (int i = 0; i < 10; i++)
      qa[i] = a->value(i, NULL);

  bool invoked = false;
  bool got_res = true;
  QString res;

  switch(mm.returnType())
  {
    case QMetaType::Int:
      int i;
      invoked = mm.invoke(o, Q_RETURN_ARG(int, i), ARGS(qa));
      // FIXME: should support returning more than only strings
      res.setNum(i);
      break;

    case QMetaType::QString:
      invoked = mm.invoke(o, Q_RETURN_ARG(QString, res), ARGS(qa));
      break;

    case QMetaType::Void:
      // drop to default, i.e. ignore result
    default:
      invoked = mm.invoke(o, ARGS(qa));
      got_res = false;
      break;
  }

  if (invoked)
  {
    if (got_res)
      getState()->notifyOK(res.toLocal8Bit().constData());
    else
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
