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

#define PROXY_IMPLEMENTATION
#include "qteobjectproxy.h"
#undef PROXY_IMPLEMENTATION

QteObjectProxy::QteObjectProxy()
{
}

template<>
QObject *QteObjectProxy::newProxyObject<QObject>(QObject *obj, QObject *owner)
{
  foreach (QtErlProxyFactory *f, QtErlProxyFactory::factories)
  {
    QObject *p = f->newProxyObject(obj, owner);
    if (p)
      return p;
  }

  return 0;
}
