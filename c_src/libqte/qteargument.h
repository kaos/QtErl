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

#ifndef QTEARGUMENT_H
#define QTEARGUMENT_H

#include <qlist.h>

class QtEArgument
{
public:
  QtEArgument(const char *name, void *data) :
    d(data), n(name) {}
  QtEArgument(const char *name, int data) :
    d(&v), n(name), v(data) {}
  QtEArgument(const char *name, long data) :
    d(&v), n(name), v(data) {}
  QtEArgument(const char *name, double data) :
    d(&v), n(name), v(data) {}
  ~QtEArgument();
  void *data() const { return const_cast<void *>(d); }
  const char *name() const { return n; }
private:
  const void *d;
  const char *n;
  union _v {
    _v() {}
    double d; _v(double _)  : d(_) {}
    long l;   _v(long _)    : l(_) {}
    int i;    _v(int _)     : i(_) {}
  } v;
};

class QtEArgumentList : public QList<QtEArgument *>
{
public:
  ~QtEArgumentList();
};

#define QTE_ARG(_type_, _value_) new QtEArgument(#_type_, _value_)
#define QTE_ARG_NEW(_type_, _value_) new QtEArgument(#_type_, new _type_(_value_))
#define QTE_Q_ARG(_argp_) _argp_ ? QGenericArgument(_argp_->name(), _argp_->data()) : QGenericArgument()

#endif // QTEARGUMENT_H
