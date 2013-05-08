/****************************************************************************
**
** Copyright (C) 2012 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/
**
** This file is part of the tools applications of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Digia.  For licensing terms and
** conditions see http://qt.digia.com/licensing.  For further information
** use the contact form at http://qt.digia.com/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Digia gives you certain additional
** rights.  These rights are described in the Digia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "generator.h"
#include <qstring.h>
#include <qtextstream.h>
#include <stdio.h>

QT_BEGIN_NAMESPACE

Generator::Generator(ClassDef *classDef, const QList<QByteArray> &metaTypes, const QSet<QByteArray> &knownQObjectClasses, FILE *outfile)
    : out(outfile), cdef(classDef), metaTypes(metaTypes), knownQObjectClasses(knownQObjectClasses)
{
}

void Generator::generateCode()
{
  // TODO: use a pointer checking wrap thingy monitoring the life of the stored ref
  QString str;
  QTextStream(&str)
    << "class QtErlProxy_" << cdef->classname << " : public QObject" << endl
    << "{" << endl
    << "Q_OBJECT" << endl
    << "public slots:" << endl
    << generateSlotsFromMethods()
    << "public:" << endl
    << "  QtErlProxy_" << cdef->classname << "(" << cdef->classname << " *ref) : QObject(), obj(ref) {}" << endl
    << "protected:" << endl
    << "  " << cdef->classname << " *obj;" << endl
    << "};" << endl
    << "QObject getProxyObject(" << cdef->classname << " *obj) { return QtErlProxy_"
    << cdef->classname << "(obj); }" << endl
    << endl;

  fprintf(out, str.toLocal8Bit().constData());
}

QString Generator::generateSlotsFromMethods()
{
  QString res;
  QTextStream s(&res);

  for (int m = 0; m < cdef->publicList.count(); m++)
  {
    FunctionDef f = cdef->publicList.at(m);
    int d = generateFunction(s, f);
    while (d > 0)
      d = generateFunction(s, f, d);
  }

  return res;
}

int Generator::generateFunction(QTextStream &s, FunctionDef &f, int num_defaults)
{
  int a, d;

  s << "  ";
  if (f.isStatic)
    s << "/* static */ "; // alas, the meta object system doesn't support static slots
  if (f.tag.length())
    s << "template" << f.tag << " ";
  s << f.type.name;
  if (!f.type.referenceType)
    s << " ";
  s << f.name << "(";

  for (a = d = 0; a < f.arguments.count(); a++)
  {
    ArgumentDef arg = f.arguments.at(a);

    if (arg.isDefault)
      d++;
    if (d > num_defaults)
      break;

    if (a)
      s << ", ";

    s << arg.type.name;
    if (!arg.type.referenceType)
      s << " ";
    s << arg.name;
  }

  s << ") { ";

  if (f.type.name != "void" || f.type.referenceType)
    s << "return ";

  if (f.isStatic)
    s << cdef->classname << "::";
  else
    s << "obj->";
  s << f.name << "(";

  for (a = d = 0; a < f.arguments.count(); a++)
  {
    ArgumentDef arg = f.arguments.at(a);

    if (arg.isDefault)
      d++;
    if (d > num_defaults)
      break;

    if (a)
      s << ", ";
    s << f.arguments.at(a).name;
  }
  s << "); }" << endl;

  return d > num_defaults ? d : 0;
}

QT_END_NAMESPACE
