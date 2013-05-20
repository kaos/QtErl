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

#ifndef QTE_H
#define QTE_H

#include <qobject.h>
#include <qhash.h>
#include "qteabstractstate.h"
#include "qteobjectproxy.h"

class QtE : public QObject
{
  Q_OBJECT

public:
  explicit QtE(QObject *parent = 0);


  void init(QtEStateId id);
  void init(QtEAbstractState *state)
    { init(state->getId()); }

  void clear(QtEStateId id);
  void clear(QtEAbstractState *state)
    { clear(state->getId()); }

  void insertObject(QtEStateId id, QObject *object);
  void insertObject(QtEAbstractState *state, QObject *object)
    { insertObject(state->getId(), object); }

  QList<QObject *> getObjects(QtEStateId id);
  QList<QObject *> getObjects(QtEAbstractState *state)
    { return getObjects(state->getId()); }

  template<typename T>
  T find(QtEStateId id, const QString &name);
  template<typename T>
  T find(QtEAbstractState *state, const QString &name)
  { return find<T>(state->getId(), name); }

  QObject *findObject(QtEStateId id, const QString &name);
  QObject *findObject(QtEAbstractState *state, const QString &name)
    { return findObject(state->getId(), name); }

  QWidget *findWidget(QtEStateId id, const QString &name);
  QWidget *findWidget(QtEAbstractState *state, const QString &name)
    { return findWidget(state->getId(), name); }

  void customEvent(QEvent *event);
  void loaded(QWidget *widget, QtEAbstractState *state);

  QteObjectProxy *getProxy() { return &proxy; }

private:
  QMultiHash<QtEStateId, QObject *> root;
  QteObjectProxy proxy;
};

#endif // QTE_H
