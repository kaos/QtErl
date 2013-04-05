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


#include "qterl.h"
#include "qteconnection.h"

#include <QUiLoader>
#include <QMessageBox>
#include <QPushButton>
#include <QMetaEnum>
#include <QMetaType>

QtErl::QtErl(QObject *parent)
  : QObject(parent)
{
  parent->setObjectName("::root");
}

void QtErl::init(qte_state_t state)
{
  root.insert(state, parent());
}

void QtErl::clear(qte_state_t state)
{
  foreach (QObject *o, root.values(state))
  {
    if (parent() != o)
      o->deleteLater();
  }

  root.remove(state);
}

bool QtErl::valid(qte_state_t state)
{
  return 0 < root.values(state).count();
}

bool QtErl::event(QEvent *event)
{
  if (event->type() != QteEvent::QteEventType())
    return QObject::event(event);

  QteEvent *qte_event = (QteEvent *)event;
  switch(qte_event->getQteType())
  {
    case QteEvent::LoadUI:
      loadUI((QteLoadUIEvent *)qte_event);
      break;

    case QteEvent::Connect:
      connect((QteConnectEvent *)qte_event);
      break;

    case QteEvent::Invoke:
      invoke((QteInvokeEvent *)qte_event);
      break;

    default:
      const QMetaObject *m = &qte_event->staticMetaObject;
      QTE_SR_SEND(
            qte_event->getQteStateRef(),
            "{unknown_event,~a}",
            m->enumerator(
              m->indexOfEnumerator("QteType"))
            .valueToKey(
              qte_event->getQteType())
            );
      return false;
  }

  return true;
}

void QtErl::postLoadUI(qte_state_t state, const char *fileName, QWidget *parent)
{
  QApplication::postEvent(
        this, new QteLoadUIEvent(
          state, fileName, parent));
}

void QtErl::postConnect(qte_state_t state, const char *name, const char *signal)
{
  QApplication::postEvent(
        this, new QteConnectEvent(
          state, name, signal));
}

void QtErl::postInvoke(qte_state_t state, const char *name, const char *signal, QteArgumentList *args) //qte_args_t args)
{
  QApplication::postEvent(
        this, new QteInvokeEvent(
          state, name, signal, args));
}

template<typename T>
T QtErl::find(qte_state_t state, const QString &name)
{
  foreach (QObject *o, root.values(state))
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

QObject *QtErl::findObject(qte_state_t state, const QString &name)
{
  return find<QObject *>(state, name);
}

QWidget *QtErl::findWidget(qte_state_t state, const QString &name)
{
  return find<QWidget *>(state, name);
}

void QtErl::loadUI(QteLoadUIEvent *event)
{
  QWidget *w;
  QUiLoader loader;
  qte_state_t state = event->getQteStateRef().getQteState();

  w = loader.load(event->getIODevice(), event->getParent());
  if (!w)
  {
    QTE_SR_SEND(
          event->getQteStateRef(),
          "{error,~s}",
          loader.errorString().toLocal8Bit().constData()
          );
    return;
  }

  // drop this? might wanna create it hidden, and show it later
  w->show();

  // top-level widget?
  if (!event->getParent())
    root.insert(state, w);

  QTE_SR_SEND(
        event->getQteStateRef(),
        "{ok,~s}",
        w->objectName().toLocal8Bit().constData());
}

void QtErl::connect(QteConnectEvent *event)
{
  QString name = event->getName();
  QString signal = QT_STRINGIFY(QSIGNAL_CODE) + event->getSignal();
  QString slot = SLOT(send_signal) + signal.right(signal.length() - signal.indexOf('('));
  QObject *o = findObject(event->getQteStateRef().getQteState(), name);

  if (o)
  {
    QteConnection *c = new QteConnection(
                         event, o,
                         signal.toLocal8Bit().constData(),
                         slot.toLocal8Bit().constData());

    // connected message sent by qte connection when ok
    if (c->getOk())
      return;

    // delete qte connection object if the connect failed
    delete c;
  }

  QTE_SR_SEND(
        event->getQteStateRef(),
        "{error,~s,~s}",
        name.toLocal8Bit().constData(),
        event->getSignal().toLocal8Bit().constData());
}

void QtErl::invoke(QteInvokeEvent *event)
{
  QObject *o = findObject(event->getQteStateRef().getQteState(), event->getName());

  if (!o)
  {
    QTE_SR_SEND(
          event->getQteStateRef(),
          "{error,{object_not_found,~s}}",
          event->getName().toLocal8Bit().constData());
    return;
  }

  const QMetaObject *mo = o->metaObject();
  int idx = mo->indexOfMethod(event->getMethod().toLocal8Bit().constData());
  if (-1 == idx)
  {
    QTE_SR_SEND(
          event->getQteStateRef(),
          "{error,{method_not_found,~s,~s}}",
          mo->className(),
          event->getMethod().toLocal8Bit().constData());
    return;
  }

  QMetaMethod mm = mo->method(idx);
  QteArgumentList *args = event->getArguments();  
  QteArgument *qa[10] = { NULL };

  if (args)
    for (int i = 0; i < 10; i++)
      qa[i] = args->value(i, NULL);

  if (mm.invoke(o,
        QTE_Q_ARG(qa[0]), QTE_Q_ARG(qa[1]), QTE_Q_ARG(qa[2]), QTE_Q_ARG(qa[3]), QTE_Q_ARG(qa[4]),
        QTE_Q_ARG(qa[5]), QTE_Q_ARG(qa[6]), QTE_Q_ARG(qa[7]), QTE_Q_ARG(qa[8]), QTE_Q_ARG(qa[9])))
    QTE_SR_SEND(event->getQteStateRef(), "ok");
  else
    QTE_SR_SEND(event->getQteStateRef(), "{error, {invoke_failed,~s,~s}}",
                mo->className(),
                mm.typeName());
}

