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

#include "qtelink.h"

QtELink::QtELink(QObject *parent, QtEAbstractState *state, const QString &sig) :
  QObject(parent), s(state), sig_name(sig)
{
}

QtELink::~QtELink()
{
  delete s;
}

bool QtELink::connect(QtEAbstractState *state, QObject *sender, const char *signal, const char *slot)
{
  QString sig;

  // skip leading SLOT/SIGNAL marker
  sig = &signal[1];
  // drop args from signal name
  sig.truncate(sig.indexOf('('));

  QtELink *link = new QtELink(sender, state, sig);
  bool ok = QObject::connect(sender, signal, link, slot);

  if (!ok)
    delete link;

  return ok;
}

void QtELink::send_signal()
{
  s->notify("signal", parent()->objectName().toLocal8Bit().constData(),
             sig_name.toLocal8Bit().constData());
}

void QtELink::send_signal(QString str)
{
  s->notify("signal", parent()->objectName().toLocal8Bit().constData(),
             sig_name.toLocal8Bit().constData(),
             QStringList() << str);
}
