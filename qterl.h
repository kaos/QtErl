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
  void clear(qte_state_t state);
  void postLoadUI(qte_state_t state, const char *FileName);
  void postConnect(qte_state_t state, const char *name, const char *signal);

signals:

public slots:

protected:
  void loadUI(QteLoadUIEvent *event);
  void connect(QteConnectEvent *event);

private:
  QMultiHash<qte_state_t, QWidget *> root;
};

#endif // QTERL_H
