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

// Qt includes
#include <QAbstractItemModel>
#include <QVector>
#include <QSemaphore>

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

  void deleteChildItems() {
      qDeleteAll(_childItems);
      _childItems.clear();
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

class workspace_model : public QAbstractItemModel
{
  Q_OBJECT

public:
  workspace_model(QObject *parent = 0);
  ~workspace_model();

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

public slots:
  void updateFromSymbolTable ();

signals:
  void expandRequest();

private:

  TreeItem *_rootItem;
};

#endif // WORKSPACEMODEL_H
