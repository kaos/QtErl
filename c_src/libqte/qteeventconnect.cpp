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

#include "qteeventconnect.h"
#include "qtelink.h"

QtEEventConnect::QtEEventConnect(QtEAbstractState *state, const char *name, const char *signal) :
  QtEEvent(state), n(name), s(signal)
{
}

bool QtEEventConnect::execute(QtE *qte)
{
  QtEAbstractState *state = getState();
  QString signal = QT_STRINGIFY(QSIGNAL_CODE) + s;
  QString slot = SLOT(send_signal) + s.right(s.length() - s.indexOf('('));
  QObject *o = qte->findObject(state, n);

  if (o &&
      QtELink::connect(
        releaseState(), o,
        signal.toLocal8Bit().constData(),
        slot.toLocal8Bit().constData()))
  {
    state->notifyOK(
      o->objectName().toLocal8Bit().constData(),
      s.left(s.indexOf('(')).toLocal8Bit().constData());
  }
  else
  {
    state->notifyError(
      "connect_failed",
      n.toLocal8Bit().constData(),
      s.toLocal8Bit().constData());
  }

  return true;
}
