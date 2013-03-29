#include "qterl.h"
#include <QBuffer>
#include <QFile>

/* QteStateRef */
QteStateRef::QteStateRef(qte_state_t s)
{
  state = s;
  ref_p = qte_copy_ref(state, &ref);
}

QteStateRef::QteStateRef(const QteStateRef& Other)
{
  state = Other.state;
  ref_p = qte_copy_ref(Other.ref_p, &ref);
}

QteStateRef::~QteStateRef()
{
  qte_free_ref(&ref);
}

/* QteEvent */
int QteEvent::qte_event_type = 0;

QteEvent::QteEvent(QteType type, qte_state_t state) :
  QEvent(QteEventType()), t(type), sr(state)
{
}

QteEvent::~QteEvent()
{
}

QEvent::Type QteEvent::QteEventType()
{
  if (!qte_event_type)
    qte_event_type = QEvent::registerEventType();

  return (QEvent::Type) qte_event_type;
}

/* QteLoadUIEvent */
QteLoadUIEvent::QteLoadUIEvent(qte_state_t state, const char *src)
  : QteEvent(LoadUI, state)
{
  // detect xml string data
  if ('<' == *src)
    io = new QBuffer(new QByteArray(src));
  // not xml data, assume it is a filename
  else
    io = new QFile(src);
}

QteLoadUIEvent::~QteLoadUIEvent()
{
  delete io;
}

/* QteConnectEvent */
QteConnectEvent::QteConnectEvent(qte_state_t state, const char *name, const char *signal)
  : QteEvent(Connect, state), n(name), s(signal)
{
}
