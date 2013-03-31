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
