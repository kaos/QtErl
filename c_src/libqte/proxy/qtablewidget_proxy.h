/****************************************************************************
** Object wrapper code from reading C++ file 'qtablewidget.h'
**
** Created by: The Object Wrapper Compiler version 67 (Qt 5.0.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "qtablewidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qtablewidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the owc built with Qt 5.0.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE

class QtErlProxy_QTableWidgetSelectionRange : public QObject
{
  Q_OBJECT

public:
  QtErlProxy_QTableWidgetSelectionRange(QObject *parent, QTableWidgetSelectionRange *ref) : QObject(parent), obj(ref) {}
  QtErlProxy_QTableWidgetSelectionRange(QTableWidgetSelectionRange *ref) : QObject(), obj(ref) {}
  //QtErlProxy_QTableWidgetSelectionRange(const QtErlProxy_QTableWidgetSelectionRange &copy) : QObject(copy.parent()), obj(copy.obj) {}

  Q_INVOKABLE int topRow() { return obj->topRow(); }
  Q_INVOKABLE int bottomRow() { return obj->bottomRow(); }
  Q_INVOKABLE int leftColumn() { return obj->leftColumn(); }
  Q_INVOKABLE int rightColumn() { return obj->rightColumn(); }
  Q_INVOKABLE int rowCount() { return obj->rowCount(); }
  Q_INVOKABLE int columnCount() { return obj->columnCount(); }

protected:
  QTableWidgetSelectionRange *obj;
};

QtErlProxy_QTableWidgetSelectionRange *newProxyObject(QTableWidgetSelectionRange *obj)
#ifdef PROXY_IMPLEMENTATION
{
  return new QtErlProxy_QTableWidgetSelectionRange(obj);
}
struct QtErlProxy_QTableWidgetSelectionRangeFactory : public QtErlProxyFactory
{
  QObject *newProxyObject(QObject *obj) { return tryNewProxyObject<QTableWidgetSelectionRange>(obj); }
} QtErlProxy_QTableWidgetSelectionRangeFactoryInstance;
#else
;
#endif

class QtErlProxy_QTableWidgetItem : public QObject
{
  Q_OBJECT

public:
  QtErlProxy_QTableWidgetItem(QObject *parent, QTableWidgetItem *ref) : QObject(parent), obj(ref) {}
  QtErlProxy_QTableWidgetItem(QTableWidgetItem *ref) : QObject(), obj(ref) {}
  //QtErlProxy_QTableWidgetItem(const QtErlProxy_QTableWidgetItem &copy) : QObject(copy.parent()), obj(copy.obj) {}

  Q_INVOKABLE QTableWidgetItem *clone() { return obj->clone(); }
  Q_INVOKABLE QTableWidget *tableWidget() { return obj->tableWidget(); }
  Q_INVOKABLE int row() { return obj->row(); }
  Q_INVOKABLE int column() { return obj->column(); }
  Q_INVOKABLE void setSelected(bool select) { obj->setSelected(select); }
  Q_INVOKABLE bool isSelected() { return obj->isSelected(); }
  Q_INVOKABLE Qt::ItemFlags flags() { return obj->flags(); }
  Q_INVOKABLE void setFlags(Qt::ItemFlags flags) { obj->setFlags(flags); }
  Q_INVOKABLE QString text() { return obj->text(); }
  Q_INVOKABLE void setText(const QString &text) { obj->setText(text); }
  Q_INVOKABLE QIcon icon() { return obj->icon(); }
  Q_INVOKABLE void setIcon(const QIcon &icon) { obj->setIcon(icon); }
  Q_INVOKABLE QString statusTip() { return obj->statusTip(); }
  Q_INVOKABLE void setStatusTip(const QString &statusTip) { obj->setStatusTip(statusTip); }
  Q_INVOKABLE QString toolTip() { return obj->toolTip(); }
  Q_INVOKABLE void setToolTip(const QString &toolTip) { obj->setToolTip(toolTip); }
  Q_INVOKABLE QString whatsThis() { return obj->whatsThis(); }
  Q_INVOKABLE void setWhatsThis(const QString &whatsThis) { obj->setWhatsThis(whatsThis); }
  Q_INVOKABLE QFont font() { return obj->font(); }
  Q_INVOKABLE void setFont(const QFont &font) { obj->setFont(font); }
  Q_INVOKABLE int textAlignment() { return obj->textAlignment(); }
  Q_INVOKABLE void setTextAlignment(int alignment) { obj->setTextAlignment(alignment); }
  Q_INVOKABLE QColor backgroundColor() { return obj->backgroundColor(); }
  Q_INVOKABLE void setBackgroundColor(const QColor &color) { obj->setBackgroundColor(color); }
  Q_INVOKABLE QBrush background() { return obj->background(); }
  Q_INVOKABLE void setBackground(const QBrush &brush) { obj->setBackground(brush); }
  Q_INVOKABLE QColor textColor() { return obj->textColor(); }
  Q_INVOKABLE void setTextColor(const QColor &color) { obj->setTextColor(color); }
  Q_INVOKABLE QBrush foreground() { return obj->foreground(); }
  Q_INVOKABLE void setForeground(const QBrush &brush) { obj->setForeground(brush); }
  Q_INVOKABLE Qt::CheckState checkState() { return obj->checkState(); }
  Q_INVOKABLE void setCheckState(Qt::CheckState state) { obj->setCheckState(state); }
  Q_INVOKABLE QSize sizeHint() { return obj->sizeHint(); }
  Q_INVOKABLE void setSizeHint(const QSize &size) { obj->setSizeHint(size); }
  Q_INVOKABLE QVariant data(int role) { return obj->data(role); }
  Q_INVOKABLE void setData(int role, const QVariant &value) { obj->setData(role, value); }
  Q_INVOKABLE void read(QDataStream &in) { obj->read(in); }
  Q_INVOKABLE void write(QDataStream &out) { obj->write(out); }
  Q_INVOKABLE int type() { return obj->type(); }

protected:
  QTableWidgetItem *obj;
};

QtErlProxy_QTableWidgetItem *newProxyObject(QTableWidgetItem *obj)
#ifdef PROXY_IMPLEMENTATION
{
  return new QtErlProxy_QTableWidgetItem(obj);
}
struct QtErlProxy_QTableWidgetItemFactory : public QtErlProxyFactory
{
  QObject *newProxyObject(QObject *obj) { return tryNewProxyObject<QTableWidgetItem>(obj); }
} QtErlProxy_QTableWidgetItemFactoryInstance;
#else
;
#endif

class QtErlProxy_QTableWidget : public QObject
{
  Q_OBJECT

public:
  QtErlProxy_QTableWidget(QObject *parent, QTableWidget *ref) : QObject(parent), obj(ref) {}
  QtErlProxy_QTableWidget(QTableWidget *ref) : QObject(), obj(ref) {}
  //QtErlProxy_QTableWidget(const QtErlProxy_QTableWidget &copy) : QObject(copy.parent()), obj(copy.obj) {}

  Q_INVOKABLE void setRowCount(int rows) { obj->setRowCount(rows); }
  Q_INVOKABLE int rowCount() { return obj->rowCount(); }
  Q_INVOKABLE void setColumnCount(int columns) { obj->setColumnCount(columns); }
  Q_INVOKABLE int columnCount() { return obj->columnCount(); }
  Q_INVOKABLE int row(const QTableWidgetItem *item) { return obj->row(item); }
  Q_INVOKABLE int column(const QTableWidgetItem *item) { return obj->column(item); }
  Q_INVOKABLE QTableWidgetItem *item(int row, int column) { return obj->item(row, column); }
  Q_INVOKABLE void setItem(int row, int column, QTableWidgetItem *item) { obj->setItem(row, column, item); }
  Q_INVOKABLE QTableWidgetItem *takeItem(int row, int column) { return obj->takeItem(row, column); }
  Q_INVOKABLE QTableWidgetItem *verticalHeaderItem(int row) { return obj->verticalHeaderItem(row); }
  Q_INVOKABLE void setVerticalHeaderItem(int row, QTableWidgetItem *item) { obj->setVerticalHeaderItem(row, item); }
  Q_INVOKABLE QTableWidgetItem *takeVerticalHeaderItem(int row) { return obj->takeVerticalHeaderItem(row); }
  Q_INVOKABLE QTableWidgetItem *horizontalHeaderItem(int column) { return obj->horizontalHeaderItem(column); }
  Q_INVOKABLE void setHorizontalHeaderItem(int column, QTableWidgetItem *item) { obj->setHorizontalHeaderItem(column, item); }
  Q_INVOKABLE QTableWidgetItem *takeHorizontalHeaderItem(int column) { return obj->takeHorizontalHeaderItem(column); }
  Q_INVOKABLE void setVerticalHeaderLabels(const QStringList &labels) { obj->setVerticalHeaderLabels(labels); }
  Q_INVOKABLE void setHorizontalHeaderLabels(const QStringList &labels) { obj->setHorizontalHeaderLabels(labels); }
  Q_INVOKABLE int currentRow() { return obj->currentRow(); }
  Q_INVOKABLE int currentColumn() { return obj->currentColumn(); }
  Q_INVOKABLE QTableWidgetItem *currentItem() { return obj->currentItem(); }
  Q_INVOKABLE void setCurrentItem(QTableWidgetItem *item) { obj->setCurrentItem(item); }
  Q_INVOKABLE void setCurrentItem(QTableWidgetItem *item, QItemSelectionModel::SelectionFlags command) { obj->setCurrentItem(item, command); }
  Q_INVOKABLE void setCurrentCell(int row, int column) { obj->setCurrentCell(row, column); }
  Q_INVOKABLE void setCurrentCell(int row, int column, QItemSelectionModel::SelectionFlags command) { obj->setCurrentCell(row, column, command); }
  Q_INVOKABLE void sortItems(int column) { obj->sortItems(column); }
  Q_INVOKABLE void sortItems(int column, Qt::SortOrder order) { obj->sortItems(column, order); }
  Q_INVOKABLE void setSortingEnabled(bool enable) { obj->setSortingEnabled(enable); }
  Q_INVOKABLE bool isSortingEnabled() { return obj->isSortingEnabled(); }
  Q_INVOKABLE void editItem(QTableWidgetItem *item) { obj->editItem(item); }
  Q_INVOKABLE void openPersistentEditor(QTableWidgetItem *item) { obj->openPersistentEditor(item); }
  Q_INVOKABLE void closePersistentEditor(QTableWidgetItem *item) { obj->closePersistentEditor(item); }
  Q_INVOKABLE QWidget *cellWidget(int row, int column) { return obj->cellWidget(row, column); }
  Q_INVOKABLE void setCellWidget(int row, int column, QWidget *widget) { obj->setCellWidget(row, column, widget); }
  Q_INVOKABLE void removeCellWidget(int row, int column) { obj->removeCellWidget(row, column); }
  Q_INVOKABLE bool isItemSelected(const QTableWidgetItem *item) { return obj->isItemSelected(item); }
  Q_INVOKABLE void setItemSelected(const QTableWidgetItem *item, bool select) { obj->setItemSelected(item, select); }
  Q_INVOKABLE void setRangeSelected(const QTableWidgetSelectionRange &range, bool select) { obj->setRangeSelected(range, select); }
  Q_INVOKABLE QList<QTableWidgetSelectionRange> selectedRanges() { return obj->selectedRanges(); }
  Q_INVOKABLE QList<QTableWidgetItem*> selectedItems() { return obj->selectedItems(); }
  Q_INVOKABLE QList<QTableWidgetItem*> findItems(const QString &text, Qt::MatchFlags flags) { return obj->findItems(text, flags); }
  Q_INVOKABLE int visualRow(int logicalRow) { return obj->visualRow(logicalRow); }
  Q_INVOKABLE int visualColumn(int logicalColumn) { return obj->visualColumn(logicalColumn); }
  Q_INVOKABLE QTableWidgetItem *itemAt(const QPoint &p) { return obj->itemAt(p); }
  Q_INVOKABLE QTableWidgetItem *itemAt(int x, int y) { return obj->itemAt(x, y); }
  Q_INVOKABLE QRect visualItemRect(const QTableWidgetItem *item) { return obj->visualItemRect(item); }
  Q_INVOKABLE const QTableWidgetItem *itemPrototype() { return obj->itemPrototype(); }
  Q_INVOKABLE void setItemPrototype(const QTableWidgetItem *item) { obj->setItemPrototype(item); }

protected:
  QTableWidget *obj;
};

QtErlProxy_QTableWidget *newProxyObject(QTableWidget *obj)
#ifdef PROXY_IMPLEMENTATION
{
  return new QtErlProxy_QTableWidget(obj);
}
struct QtErlProxy_QTableWidgetFactory : public QtErlProxyFactory
{
  QObject *newProxyObject(QObject *obj) { return tryNewProxyObject<QTableWidget>(obj); }
} QtErlProxy_QTableWidgetFactoryInstance;
#else
;
#endif

QT_END_MOC_NAMESPACE
