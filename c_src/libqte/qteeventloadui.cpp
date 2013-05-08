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

#include <qbuffer.h>
#include <qfile.h>
#include <quiloader.h>
#include "qteeventloadui.h"

QtEEventLoadUI::QtEEventLoadUI(QtEAbstractState *state, const char *src, QWidget *parent)
  : QtEEvent(state), p(parent)
{
  // detect xml string data
  if ('<' == *src)
    io = new QBuffer(new QByteArray(src));
  // not xml data, assume it is a filename
  else
    io = new QFile(src);
}

QtEEventLoadUI::~QtEEventLoadUI()
{
  delete io;
}

bool QtEEventLoadUI::execute(QtE *qte)
{
  QWidget *w;
  QUiLoader loader;

  w = loader.load(io, p);
  if (w)
  {
    qte->loaded(w, getState());
    getState()->notifyOK(w->objectName().toLocal8Bit().constData());
  }
  else
  {
    getState()->notifyError(
          "load_failed",
          loader.errorString().toLocal8Bit().constData());
  }

  return true;
}
