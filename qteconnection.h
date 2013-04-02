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
