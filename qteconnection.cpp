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


#include "qteconnection.h"

QteConnection::QteConnection(QteEvent *event, QObject *sender, const char *signal, const char *slot) :
  QObject(sender), sr(event->getQteStateRef())
{
  // save signal name for the slot
  // skip leading SLOT/SIGNAL marker
  sig_name = &signal[1];
  // drop args from signal name
  sig_name.truncate(sig_name.indexOf('('));

  // hook it up
  ok = (bool)connect(sender, signal, slot);
  if (ok)
    QTE_SR_SEND(
          sr,
          "{ok,~s,~s}",
          sender->objectName().toLocal8Bit().constData(),
          sig_name.toLocal8Bit().constData());
}

void QteConnection::send_signal()
{
  // forward signal to port driver in erlang
  QTE_SR_SEND(
        sr,
        "{signal,~a,~a}",
        parent()->objectName().toLocal8Bit().constData(),
        sig_name.toLocal8Bit().constData());
}

void QteConnection::send_signal(QString str)
{
  // forward signal to port driver in erlang
  QTE_SR_SEND(
        sr,
        "{signal,~a,{~a, [~s]}}",
        parent()->objectName().toLocal8Bit().constData(),
        sig_name.toLocal8Bit().constData(),
        str.toLocal8Bit().constData());
}
