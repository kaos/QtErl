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
  QObject *tryNewProxyObject(QObject *obj, QObject *owner = 0);

  virtual QObject *newProxyObject(QObject *obj, QObject *owner = 0) = 0;

private:
  static QList<QtErlProxyFactory *> factories;
};

#ifdef PROXY_IMPLEMENTATION
QList<QtErlProxyFactory *> QtErlProxyFactory::factories;
#endif

#include <qpointer.h>
#include "proxy/qterlproxy.h"

template<typename T>
QObject *QtErlProxyFactory::tryNewProxyObject(QObject *obj, QObject *owner)
{
  T *t = dynamic_cast<T *>(obj);
  if (!t)
    return 0;

  return ::newProxyObject(t, owner);
}

typedef unsigned long proxy_id_t;

class QteObjectProxy
{
public:
  QteObjectProxy();

  template<typename T>
  QObject *newProxyObject(T *obj, QObject *owner = 0)
  {
    return ::newProxyObject(obj, owner);
  }

  template<typename T>
  proxy_id_t addObject(T *obj, QObject *owner = 0);

  template<typename T>
  T *getObject(proxy_id_t id);

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
QObject *QteObjectProxy::newProxyObject<QObject>(QObject *obj, QObject *owner);

template<typename T>
proxy_id_t QteObjectProxy::addObject(T *obj, QObject *owner)
{
  ProxyObject p = oh[obj];

  if (p.obj != obj)
  {
    p.id = __next_id++;
    p.obj = newProxyObject(obj, owner);
    oh[obj] = ih[p.id] = p;
  }

  return p.id;
}

template<typename T>
T *QteObjectProxy::getObject(proxy_id_t id)
{
  if (!ih.contains(id))
    return 0;

  QObject *obj = ih[id].obj;
  if (!obj)
  {
    ih.remove(id);
    // TODO: clear value from oh hash too..
    return 0;
  }

  return dynamic_cast<T *>(obj);
}

#endif // QTEOBJECTPROXY_H
