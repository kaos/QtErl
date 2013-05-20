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

  if (!executeOn(qte, o, false))
  {
    QObject *p = qte->getProxy()->newProxyObject(o);
    if (p)
    {
      executeOn(qte, p, true);
      delete p;
    }
    else
      getState()->notifyError("no_proxy_for", o->metaObject()->className());
  }
}

bool QtEEventInvoke::executeOn(QtE *qte, QObject *o, bool notify)
{
  (void) qte;

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
  QVariant res;

  switch(mm.returnType())
  {
    case QMetaType::Bool: invoked = INVOKE(bool, mm, o, qa, res); break;
    case QMetaType::Int: invoked = INVOKE(int, mm, o, qa, res); break;
    case QMetaType::Short: invoked = INVOKE(short, mm, o, qa, res); break;
    case QMetaType::Long: invoked = INVOKE(long, mm, o, qa, res); break;
    case QMetaType::UInt: invoked = INVOKE(unsigned int, mm, o, qa, res); break;
    case QMetaType::UShort: invoked = INVOKE(unsigned short, mm, o, qa, res); break;
    case QMetaType::ULong: invoked = INVOKE(unsigned long, mm, o, qa, res); break;
    case QMetaType::LongLong: invoked = INVOKE(long long, mm, o, qa, res); break;
    case QMetaType::ULongLong: invoked = INVOKE(unsigned long long, mm, o, qa, res); break;
    case QMetaType::Float: invoked = INVOKE(float, mm, o, qa, res); break;
    case QMetaType::Double: invoked = INVOKE(double, mm, o, qa, res); break;
    case QMetaType::Char: invoked = INVOKE(char, mm, o, qa, res); break;
    case QMetaType::QString: invoked = INVOKE(QString, mm, o, qa, res); break;

    case QMetaType::UnknownType:
      {
        if (!QString(mm.typeName()).endsWith('*'))
        {
          // ignore return value
          invoked = mm.invoke(o, ARGS(qa));
          break;
        }
      }
    case QMetaType::VoidStar:
      /*void *ptr;
      invoked = mm.invoke(o, Q_RETURN_ARG(void *, ptr), ARGS(qa));
      // need to cast void * to proper class (or any class, really)
      res.setValue(qte->getProxy()->addObject(ptr, o));
      break;*/
    case QMetaType::Void:
      // drop to default, i.e. ignore result
    default:
      invoked = mm.invoke(o, ARGS(qa));
      break;
  }

  if (invoked)
  {
    if (res.isNull())
      getState()->notifyOK();
    else
      getState()->notify("ok", res);
  }
  else
  {
    getState()->notifyError("invoke_failed",
                            mo->className(),
                            mm.name().constData());
  }

  return true;
}

template<typename T>
bool QtEEventInvoke::invokeOnObjWithArgs(QMetaMethod &method, QObject *obj, QtEArgument *args[10], QVariant &result, const char *resultType)
{
  T ret;
  if (!method.invoke(obj, QReturnArgument<T>(resultType, ret), ARGS(args)))
    return false;

  result.setValue(ret);
  return true;
}
