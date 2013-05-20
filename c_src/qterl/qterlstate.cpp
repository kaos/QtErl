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

#include "qterl.h"

QtErlState::QtErlState(QtErl *qterl, ei_x_buff *ref) :
  QtEAbstractState(qterl->id), q(qterl)
{
  if (ref)
  {
    ei_x_new(&r);
    ei_x_append(&r, ref);
  }
}

QtErlState::~QtErlState()
{
  ei_x_free(&r);
}

void QtErlState::notify(const char *event, const char *tag, const char *key, const char *value)
{
  const char *fmt = NULL;
  if (value)
    fmt = "{~a,{~a,~a,~s}}";
  else if (key)
    fmt = "{~a,{~a,~s}}";
  else if (tag)
    fmt = "{~a,~s}";
  else if (event)
    fmt = "~a";

  if (!fmt)
    return;

  ei_x_buff x;
  ei_x_new(&x);
  ei_x_format(&x, fmt, event, tag, key, value);
  send(&x);
  ei_x_free(&x);
}

void QtErlState::notify(const char *event, const char *tag, const char *key, const QStringList &value)
{
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, event);
  ei_x_encode_tuple_header(&x, 3);
  ei_x_encode_atom(&x, tag);
  ei_x_encode_atom(&x, key);
  ei_x_encode_list_header(&x, value.length());
  foreach(QString s, value)
  {
    ei_x_encode_string(&x, s.toLocal8Bit().constData());
  }
  ei_x_encode_empty_list(&x);

  send(&x);
  ei_x_free(&x);
}

void QtErlState::notify(const char *event, const QVariant &value)
{
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, event);

  switch ((QMetaType::Type) value.type())
  {
    case QMetaType::Bool: ei_x_encode_boolean(&x, value.toBool()); break;
    case QMetaType::Int: // drop
    case QMetaType::Short: // drop
    case QMetaType::Long: ei_x_encode_long(&x, value.toInt()); break;
    case QMetaType::UInt: // drop
    case QMetaType::UShort: // drop
    case QMetaType::ULong: ei_x_encode_ulong(&x, value.toUInt()); break;
    case QMetaType::LongLong: ei_x_encode_longlong(&x, value.toLongLong()); break;
    case QMetaType::ULongLong: ei_x_encode_ulonglong(&x, value.toULongLong()); break;
    case QMetaType::Float: // drop
    case QMetaType::Double: ei_x_encode_double(&x, value.toDouble()); break;
    case QMetaType::Char: ei_x_encode_char(&x, value.toChar().toLatin1()); break;
    case QMetaType::QString:
      ei_x_encode_string_len(
            &x,
            value.toString().toLocal8Bit().constData(),
            value.toString().length());
      break;
    default: ei_x_encode_atom(&x, "undefined"); break;
  }

  send(&x);
  ei_x_free(&x);
}

int QtErlState::send(ei_x_buff *x)
{
  ei_x_buff *ref = NULL;

  if (r.buff)
    ref = &r;

  ErlDrvTermData term[] = {
    ERL_DRV_EXT2TERM,
    (ErlDrvTermData) (ref ? ref->buff : 0),
    (ErlDrvTermData) (ref ? ref->buffsz : 0),
    ERL_DRV_EXT2TERM,
    (ErlDrvTermData) x->buff,
    (ErlDrvTermData) x->buffsz,
    ERL_DRV_TUPLE, 2
  };

  return driver_send_term(
        q->dp, driver_connected(q->dp),
        &term[ref ? 0 : 3], ref ? 8 : 3
      );
}
