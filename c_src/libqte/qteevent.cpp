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

#include "qteevent.h"

QtEEvent::QtEEvent(QtEAbstractState *state) :
  QEvent(EventType()), os(state)
{
}

QtEEvent::~QtEEvent()
{
  if (os)
    delete os;
}

QtEAbstractState *QtEEvent::releaseState()
{
  QtEAbstractState *ret;

  ret = os;
  os = NULL;

  return ret;
}

void QtEEvent::execute(QtE *qte)
{
  (void) qte;
}

int QtEEvent::event_type = 0;
QEvent::Type QtEEvent::EventType()
{
  if (!event_type)
    event_type = QEvent::registerEventType();

  return static_cast<QEvent::Type>(event_type);
}
