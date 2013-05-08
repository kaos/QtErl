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

#include "qteabstractstate.h"

QtEAbstractState::QtEAbstractState(QtEStateId state_id) :
  id(state_id)
{
}

void QtEAbstractState::notifyOK(const char *tag, const char *key, const char *value)
{
  notify("ok", tag, key, value);
}

void QtEAbstractState::notifyError(const char *tag, const char *key, const char *value)
{
  notify("error", tag, key, value);
}
