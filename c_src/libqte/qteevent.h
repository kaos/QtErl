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
#include "qte.h"

class QtEEvent : public QEvent
{
  Q_GADGET

public:
  explicit QtEEvent(QtEAbstractState *state = 0);
  virtual ~QtEEvent();

  QtEAbstractState *getState() const { return os; }
  QtEAbstractState *releaseState();
  virtual bool execute(QtE *qte);
private:
  QtEAbstractState *os;

  /* static stuff */
public:
  static QEvent::Type EventType();

private:
  static int event_type;
};

#endif // QTEEVENT_H
