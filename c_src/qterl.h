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


#ifndef QTERL_H
#define QTERL_H

#include <QApplication>
#include <QObject>
#include <QEvent>
#include <QString>
#include <QMultiHash>

#include "qterl_drv.h"
#include "qteevent.h"

class QtErl : public QObject
{
  Q_OBJECT

public:
  explicit QtErl(QObject *parent = 0);
  bool event(QEvent *event);
  void init(qte_state_t state);
  void clear(qte_state_t state);
  bool valid(qte_state_t state);
  void postLoadUI(qte_state_t state, const char *FileName, QWidget *parent = 0);
  void postConnect(qte_state_t state, const char *name, const char *signal);
  void postInvoke(qte_state_t state, const char *name, const char *method /*, todo: args */);

  template<typename T>
  T find(qte_state_t state, const QString &name);
  QObject *findObject(qte_state_t state, const QString &name);
  QWidget *findWidget(qte_state_t state, const QString &name);

signals:

public slots:

protected:
  void loadUI(QteLoadUIEvent *event);
  void connect(QteConnectEvent *event);
  void invoke(QteInvokeEvent *event);

private:
  QMultiHash<qte_state_t, QObject *> root;
};

#endif // QTERL_H
