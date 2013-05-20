/****************************************************************************
** Object wrapper code from reading C++ file 'qlineedit.h'
**
** Created by: The Object Wrapper Compiler version 67 (Qt 5.0.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "qlineedit.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qlineedit.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the owc built with Qt 5.0.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE

class QtErlProxy_QLineEdit : public QObject
{
  Q_OBJECT

public:
  QtErlProxy_QLineEdit(QObject *parent, QLineEdit *ref) : QObject(parent), obj(ref) {}
  QtErlProxy_QLineEdit(QLineEdit *ref) : QObject(), obj(ref) {}
  //QtErlProxy_QLineEdit(const QtErlProxy_QLineEdit &copy) : QObject(copy.parent()), obj(copy.obj) {}

  Q_INVOKABLE QString text() { return obj->text(); }
  Q_INVOKABLE QString displayText() { return obj->displayText(); }
  Q_INVOKABLE QString placeholderText() { return obj->placeholderText(); }
  Q_INVOKABLE void setPlaceholderText(const QString &A) { obj->setPlaceholderText(A); }
  Q_INVOKABLE int maxLength() { return obj->maxLength(); }
  Q_INVOKABLE void setMaxLength(int A) { obj->setMaxLength(A); }
  Q_INVOKABLE void setFrame(bool A) { obj->setFrame(A); }
  Q_INVOKABLE bool hasFrame() { return obj->hasFrame(); }
  Q_INVOKABLE QLineEdit::EchoMode echoMode() { return obj->echoMode(); }
  Q_INVOKABLE void setEchoMode(QLineEdit::EchoMode A) { obj->setEchoMode(A); }
  Q_INVOKABLE bool isReadOnly() { return obj->isReadOnly(); }
  Q_INVOKABLE void setReadOnly(bool A) { obj->setReadOnly(A); }
  Q_INVOKABLE void setValidator(const QValidator *A) { obj->setValidator(A); }
  Q_INVOKABLE const QValidator *validator() { return obj->validator(); }
  Q_INVOKABLE void setCompleter(QCompleter *completer) { obj->setCompleter(completer); }
  Q_INVOKABLE QCompleter *completer() { return obj->completer(); }
  Q_INVOKABLE QSize sizeHint() { return obj->sizeHint(); }
  Q_INVOKABLE QSize minimumSizeHint() { return obj->minimumSizeHint(); }
  Q_INVOKABLE int cursorPosition() { return obj->cursorPosition(); }
  Q_INVOKABLE void setCursorPosition(int A) { obj->setCursorPosition(A); }
  Q_INVOKABLE int cursorPositionAt(const QPoint &pos) { return obj->cursorPositionAt(pos); }
  Q_INVOKABLE void setAlignment(Qt::Alignment flag) { obj->setAlignment(flag); }
  Q_INVOKABLE Qt::Alignment alignment() { return obj->alignment(); }
  Q_INVOKABLE void cursorForward(bool mark) { obj->cursorForward(mark); }
  Q_INVOKABLE void cursorForward(bool mark, int steps) { obj->cursorForward(mark, steps); }
  Q_INVOKABLE void cursorBackward(bool mark) { obj->cursorBackward(mark); }
  Q_INVOKABLE void cursorBackward(bool mark, int steps) { obj->cursorBackward(mark, steps); }
  Q_INVOKABLE void cursorWordForward(bool mark) { obj->cursorWordForward(mark); }
  Q_INVOKABLE void cursorWordBackward(bool mark) { obj->cursorWordBackward(mark); }
  Q_INVOKABLE void backspace() { obj->backspace(); }
  Q_INVOKABLE void del() { obj->del(); }
  Q_INVOKABLE void home(bool mark) { obj->home(mark); }
  Q_INVOKABLE void end(bool mark) { obj->end(mark); }
  Q_INVOKABLE bool isModified() { return obj->isModified(); }
  Q_INVOKABLE void setModified(bool A) { obj->setModified(A); }
  Q_INVOKABLE void setSelection(int A, int B) { obj->setSelection(A, B); }
  Q_INVOKABLE bool hasSelectedText() { return obj->hasSelectedText(); }
  Q_INVOKABLE QString selectedText() { return obj->selectedText(); }
  Q_INVOKABLE int selectionStart() { return obj->selectionStart(); }
  Q_INVOKABLE bool isUndoAvailable() { return obj->isUndoAvailable(); }
  Q_INVOKABLE bool isRedoAvailable() { return obj->isRedoAvailable(); }
  Q_INVOKABLE void setDragEnabled(bool b) { obj->setDragEnabled(b); }
  Q_INVOKABLE bool dragEnabled() { return obj->dragEnabled(); }
  Q_INVOKABLE void setCursorMoveStyle(Qt::CursorMoveStyle style) { obj->setCursorMoveStyle(style); }
  Q_INVOKABLE Qt::CursorMoveStyle cursorMoveStyle() { return obj->cursorMoveStyle(); }
  Q_INVOKABLE QString inputMask() { return obj->inputMask(); }
  Q_INVOKABLE void setInputMask(const QString &inputMask) { obj->setInputMask(inputMask); }
  Q_INVOKABLE bool hasAcceptableInput() { return obj->hasAcceptableInput(); }
  Q_INVOKABLE void setTextMargins(int left, int top, int right, int bottom) { obj->setTextMargins(left, top, right, bottom); }
  Q_INVOKABLE void setTextMargins(const QMargins &margins) { obj->setTextMargins(margins); }
  Q_INVOKABLE void getTextMargins(int *left, int *top, int *right, int *bottom) { obj->getTextMargins(left, top, right, bottom); }
  Q_INVOKABLE QMargins textMargins() { return obj->textMargins(); }
  Q_INVOKABLE void deselect() { obj->deselect(); }
  Q_INVOKABLE void insert(const QString &A) { obj->insert(A); }
  Q_INVOKABLE QMenu *createStandardContextMenu() { return obj->createStandardContextMenu(); }
  Q_INVOKABLE QVariant inputMethodQuery(Qt::InputMethodQuery A) { return obj->inputMethodQuery(A); }
  Q_INVOKABLE bool event(QEvent *A) { return obj->event(A); }

protected:
  QLineEdit *obj;
};

QtErlProxy_QLineEdit *newProxyObject(QLineEdit *obj, QObject *owner = 0)
#ifdef PROXY_IMPLEMENTATION
{
  return new QtErlProxy_QLineEdit(owner, obj);
}
struct QtErlProxy_QLineEditFactory : public QtErlProxyFactory
{
  QObject *newProxyObject(QObject *obj, QObject *owner) { return tryNewProxyObject<QLineEdit>(obj, owner); }
} QtErlProxy_QLineEditFactoryInstance;
#else
;
#endif

QT_END_MOC_NAMESPACE
