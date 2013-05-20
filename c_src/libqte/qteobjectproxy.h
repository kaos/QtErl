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

#ifndef QTEOBJECTPROXY_H
#define QTEOBJECTPROXY_H

#include <qobject.h>
#include <qlist.h>

struct QtErlProxyFactory
{
  friend class QteObjectProxy;

  QtErlProxyFactory() { factories << this; }
  ~QtErlProxyFactory() { factories.removeAll(this); }

  template<typename T>
  QObject *tryNewProxyObject(QObject *obj);

  virtual QObject *newProxyObject(QObject *obj) = 0;

private:
  static QList<QtErlProxyFactory *> factories;
};

#ifdef PROXY_IMPLEMENTATION
QList<QtErlProxyFactory *> QtErlProxyFactory::factories;
#endif

#include <qpointer.h>
#include "proxy/qterlproxy.h"

template<typename T>
QObject *QtErlProxyFactory::tryNewProxyObject(QObject *obj)
{
  T *t = dynamic_cast<T *>(obj);
  if (!t)
    return 0;

  return ::newProxyObject(t);
}

typedef unsigned long proxy_id_t;

class QteObjectProxy
{
public:
  QteObjectProxy();

  template<typename T>
  QObject *newProxyObject(T *obj)
  {
    return ::newProxyObject(obj);
  }

/*
  template<typename T>
  proxy_id_t addObject(T *obj, QObject *owner = 0);
  template<typename T>
  T *getObject(proxy_id_t id);
*/

private:
  proxy_id_t __next_id;

  struct ProxyObject {
    proxy_id_t id;
    QPointer<QObject> obj;
  };

  QHash<void *, ProxyObject> oh;
  QHash<proxy_id_t, ProxyObject> ih;
};

template<>
QObject *QteObjectProxy::newProxyObject<QObject>(QObject *obj);

#endif // QTEOBJECTPROXY_H
