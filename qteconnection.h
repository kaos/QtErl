#ifndef QTECONNECTION_H
#define QTECONNECTION_H

#include <QObject>
#include "qteevent.h"

class QteConnection : public QObject
{
  Q_OBJECT
public:
  explicit QteConnection(QteEvent *event, QObject *sender, const char *signal, const char *slot);
  bool getOk() { return ok; }

signals:
  
public slots:
  void send_signal();
  void send_signal(QString str);

private:
  QteStateRef sr;
  QString sig_name;
  bool ok;
};

#endif // QTECONNECTION_H
