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

#ifndef QTEEVENTLOADUI_H
#define QTEEVENTLOADUI_H

#include "qteevent.h"

class QtEEventLoadUI : public QtEEvent
{
  Q_GADGET

public:
  QtEEventLoadUI(QtEAbstractState *state, const char *src, QWidget *parent = 0);
  virtual ~QtEEventLoadUI();

  bool execute(QtE *qte);
private:
  QWidget *p;
  QIODevice *io;
};

#endif // QTEEVENTLOADUI_H
