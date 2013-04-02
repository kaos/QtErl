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


#ifndef QTEEVENT_H
#define QTEEVENT_H

#include <QEvent>
#include <QString>
#include <QIODevice>

#include "qterl_drv.h"

#define QTE_SR_SEND(_sr_, _fmt_, ...) \
  QTE_REF_SEND((_sr_).getQteState(), (_sr_).getQteRef(), (_fmt_), __VA_ARGS__)

class QteStateRef
{
public:
  explicit QteStateRef(qte_state_t);
  QteStateRef(const QteStateRef&);
  ~QteStateRef();

  qte_state_t getQteState() { return state; }
  qte_ref_t *getQteRef() { return ref_p; }

private:
  qte_state_t state;
  qte_ref_t ref;
  qte_ref_t *ref_p;
};

class QteEvent : public QEvent
{
  Q_GADGET
  Q_ENUMS(QteType)

public:
  enum QteType {
    LoadUI,
    Connect
  };

  explicit QteEvent(QteType type, qte_state_t);
  virtual ~QteEvent();
  QteType getQteType() { return t; }
  QteStateRef getQteStateRef() const { return sr; }

  static QEvent::Type QteEventType();

private:
  static int qte_event_type;
  QteType t;
  QteStateRef sr;
};

class QteLoadUIEvent : public QteEvent
{
  Q_GADGET

public:
  explicit QteLoadUIEvent(qte_state_t, const char *, QWidget *parent = 0);
  ~QteLoadUIEvent();
  QWidget *getParent() { return p; }
  QIODevice *getIODevice() { return io; }

private:
  QWidget *p;
  QIODevice *io;
};

class QteConnectEvent : public QteEvent
{
  Q_GADGET

public:
  explicit QteConnectEvent(qte_state_t, const char *name, const char *signal);
  QString getName() const { return n; }
  QString getSignal() const { return s; }

private:
  QString n;
  QString s;
};

#endif // QTEEVENT_H
