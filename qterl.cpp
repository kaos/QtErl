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

QtErl::QtErl(QObject *parent)
  : QObject(parent)
{
}

void QtErl::clear(qte_state_t state)
{
  foreach (QWidget *w, root.values(state))
    w->deleteLater();

  root.remove(state);
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

void QtErl::postLoadUI(qte_state_t state, const char *FileName, QWidget *parent)
{
  QApplication::instance()->postEvent(
        this, new QteLoadUIEvent(
          state, FileName, parent));
}

void QtErl::postConnect(qte_state_t state, const char *name, const char *signal)
{
  QApplication::instance()->postEvent(
        this, new QteConnectEvent(
          state, name, signal));
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

  foreach (QWidget *w, root.values(event->getQteStateRef().getQteState()))
  {
    QWidget *o = w->findChild<QWidget *>(name);
    if (o)
    {
      QteConnection *c = new QteConnection(
                           event, o,
                           signal.toLocal8Bit().constData(),
                           slot.toLocal8Bit().constData());

      if (!c->getOk())
        delete c;
      else
        // connected message sent by qte connection when ok
        return;
    }
  }

  QTE_SR_SEND(
        event->getQteStateRef(),
        "{error,~s,~s}",
        name.toLocal8Bit().constData(),
        event->getSignal().toLocal8Bit().constData());
}
