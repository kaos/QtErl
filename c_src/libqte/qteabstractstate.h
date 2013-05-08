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

#ifndef QTEABSTRACTSTATE_H
#define QTEABSTRACTSTATE_H

#include <qwidget.h>
#include <qstringlist.h>

typedef unsigned long QtEStateId;

class QtEAbstractState
{
public:
  QtEAbstractState(QtEStateId state_id);
  virtual ~QtEAbstractState() {};
  QtEStateId getId() const { return id; }

  void notifyOK(const char *tag = 0, const char *key = 0, const char *value = 0);
  void notifyError(const char *tag = 0, const char *key = 0, const char *value = 0);

  virtual void notify(const char *event, const char *tag = 0, const char *key = 0, const char *value = 0) = 0;
  virtual void notify(const char *event, const char *tag, const char *key, QStringList const &value) = 0;

private:
  QtEStateId id;
};

#endif // QTEABSTRACTSTATE_H
