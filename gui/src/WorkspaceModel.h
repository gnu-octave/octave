/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef WORKSPACEMODEL_H
#define WORKSPACEMODEL_H

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"
#include "octave/cmd-edit.h"
#include "octave/error.h"
#include "octave/file-io.h"
#include "octave/input.h"
#include "octave/lex.h"
#include "octave/load-path.h"
#include "octave/octave.h"
#include "octave/oct-hist.h"
#include "octave/oct-map.h"
#include "octave/oct-obj.h"
#include "octave/ops.h"
#include "octave/ov.h"
#include "octave/ov-usr-fcn.h"
#include "octave/symtab.h"
#include "octave/pt.h"
#include "octave/pt-eval.h"
#include "octave/config.h"
#include "octave/Range.h"
#include "octave/toplev.h"
#include "octave/procstream.h"
#include "octave/sighandlers.h"
#include "octave/debug.h"
#include "octave/sysdep.h"
#include "octave/ov.h"
#include "octave/unwind-prot.h"
#include "octave/utils.h"
#include "octave/variables.h"

// Qt includes
#include <QAbstractItemModel>

class TreeItem
{
public:
  TreeItem(const QList<QVariant> &data, TreeItem *parent = 0) {
    _parentItem = parent;
    _itemData = data;
  }

  TreeItem(QVariant data = QVariant(), TreeItem *parent = 0) {
    QList<QVariant> variantList;
    variantList << data << QVariant() << QVariant();
    _parentItem = parent;
    _itemData = variantList;
  }

  ~TreeItem() {
    qDeleteAll(_childItems);
  }

  void insertChildItem(int at, TreeItem *item) {
    item->_parentItem = this;
    _childItems.insert(at, item);
  }

  void addChild(TreeItem *item) {
    item->_parentItem = this;
    _childItems.append(item);
  }

  void removeChild(TreeItem *item) {
    _childItems.removeAll(item);
  }

  QVariant data(int column) const
  {
    return _itemData[column];
  }

  void setData(int column, QVariant data)
  {
    _itemData[column] = data;
  }

  TreeItem *child(int row) {
    return _childItems[row];
  }

  int childCount() const {
    return _childItems.count();
  }

  int columnCount() const
  {
    return _itemData.count();
  }

  int row() const {
    if (_parentItem)
      return _parentItem->_childItems.indexOf(const_cast<TreeItem*>(this));

    return 0;
  }

  TreeItem *parent()
  {
    return _parentItem;
  }

private:
  QList<TreeItem*> _childItems;
  QList<QVariant> _itemData;
  TreeItem *_parentItem;
};

class WorkspaceModel : public QAbstractItemModel
{
  Q_OBJECT

public:
  WorkspaceModel(QObject *parent = 0);
  ~WorkspaceModel();

  QVariant data(const QModelIndex &index, int role) const;
  Qt::ItemFlags flags(const QModelIndex &index) const;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const;
  QModelIndex index(int row, int column,
                    const QModelIndex &parent = QModelIndex()) const;
  QModelIndex parent(const QModelIndex &index) const;
  int rowCount(const QModelIndex &parent = QModelIndex()) const;
  int columnCount(const QModelIndex &parent = QModelIndex()) const;

  void insertTopLevelItem (int at, TreeItem *treeItem);
  TreeItem *topLevelItem (int at);

  void updateFromSymbolTable ();
  void updateTreeEntry (TreeItem * treeItem, symbol_table::symbol_record *symbolRecord);
  void updateCategory (int topLevelItemIndex, QList < symbol_table::symbol_record *> symbolTable);
  QString octaveValueAsQString (const octave_value &octaveValue);

signals:
  void expandRequest();

private:
  TreeItem *_rootItem;
};

#endif // WORKSPACEMODEL_H
