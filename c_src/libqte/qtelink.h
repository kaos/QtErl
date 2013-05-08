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

#ifndef QTELINK_H
#define QTELINK_H

#include <QObject>
#include "qteabstractstate.h"

class QtELink : public QObject
{
  Q_OBJECT

public:
  virtual ~QtELink();
  static bool connect(QtEAbstractState *state, QObject *sender, const char *signal, const char *slot);

public slots:
  void send_signal();
  void send_signal(QString str);

private:
  explicit QtELink(QObject *parent, QtEAbstractState *state, QString const &sig);

  QtEAbstractState *s;
  QString sig_name;
};

#endif // QTELINK_H
