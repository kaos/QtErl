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

#include "qte.h"
#include "qteevent.h"

QtE::QtE(QObject *parent)
  : QObject(parent)
{
  if (parent)
    parent->setObjectName("::root");
}

void QtE::init(QtEStateId id)
{
  insertObject(id, parent());
}

void QtE::clear(QtEStateId id)
{
  root.remove(id, parent());
  foreach(QObject *o, getObjects(id))
    o->deleteLater();
  root.remove(id);
}

void QtE::insertObject(QtEStateId id, QObject *object)
{
  root.insert(id, object);
}

QList<QObject *> QtE::getObjects(QtEStateId id)
{
  return root.values(id);
}

template<typename T>
T QtE::find(QtEStateId id, const QString &name)
{
  foreach (QObject *o, getObjects(id))
  {
    T t = dynamic_cast<T>(o);
    if (t && (name == o->objectName()))
      return t;

    t = o->findChild<T>(name);
    if (t)
      return t;
  }

  return NULL;
}

QObject *QtE::findObject(QtEStateId id, const QString &name)
{
  return find<QObject *>(id, name);
}

QWidget *QtE::findWidget(QtEStateId id, const QString &name)
{
  return find<QWidget *>(id, name);
}

bool QtE::event(QEvent *event)
{
  if (event->type() == QtEEvent::EventType())
  {
    QtEEvent *e = dynamic_cast<QtEEvent *>(event);
    return e && e->execute(this);
  }

  return QObject::event(event);
}

void QtE::loaded(QWidget *widget, QtEAbstractState *state)
{
  (void) state;

  // drop this? might wanna create it hidden, and show it later
  widget->show();

  // top-level widget?
  if (!widget->parent())
    insertObject(state, widget);
}

