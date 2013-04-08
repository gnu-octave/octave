
/*

Copyright (C) 2013 John W. Eaton
Copyright (C) 2011-2012 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QTreeWidget>
#include <QTime>

#include "symtab.h"
#include "variables.h"

#include "workspace-model.h"
#include "octave-link.h"

workspace_model::workspace_model(QObject *p)
  : QAbstractItemModel (p)
{
  QList<QVariant> rootData;
  rootData << tr ("Name") << tr ("Class") << tr("Dimension") << tr ("Value");
  _rootItem = new tree_item(rootData);

  insert_top_level_item(0, new tree_item ("Local"));
  insert_top_level_item(1, new tree_item ("Global"));
  insert_top_level_item(2, new tree_item ("Persistent"));
}

workspace_model::~workspace_model()
{
  delete _rootItem;
}

QModelIndex
workspace_model::index(int row, int column, const QModelIndex &p) const
{
  if (!hasIndex(row, column, p))
    return QModelIndex();

  tree_item *parentItem;

  if (!p.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<tree_item*>(p.internalPointer());

  tree_item *childItem = parentItem->child(row);
  if (childItem)
    return createIndex(row, column, childItem);
  else
    return QModelIndex();
}

QModelIndex
workspace_model::parent(const QModelIndex &idx) const
{
  if (!idx.isValid())
    return QModelIndex();

  tree_item *childItem = static_cast<tree_item*>(idx.internalPointer());

  if (childItem)
    {
      tree_item *parentItem = childItem->parent();

      if (! parentItem || parentItem == _rootItem)
        return QModelIndex();

      return createIndex(parentItem->row(), 0, parentItem);
    }
  else
    return QModelIndex ();
}

int
workspace_model::rowCount(const QModelIndex &p) const
{
  tree_item *parentItem;
  if (p.column() > 0)
    return 0;

  if (!p.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<tree_item*>(p.internalPointer());

  return parentItem->child_count();
}

int
workspace_model::columnCount(const QModelIndex &p) const
{
  if (p.isValid())
    return static_cast<tree_item*>(p.internalPointer())->column_count();
  else
    return _rootItem->column_count();
}

void
workspace_model::insert_top_level_item(int at, tree_item *treeItem)
{
  _rootItem->insert_child_item(at, treeItem);
}

tree_item *
workspace_model::top_level_item (int at)
{
  return _rootItem->child(at);
}

Qt::ItemFlags
workspace_model::flags(const QModelIndex &idx) const
{
  if (!idx.isValid())
    return 0;

  return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QVariant
workspace_model::headerData(int section, Qt::Orientation orientation, int role) const
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return _rootItem->data(section);

  return QVariant();
}

QVariant
workspace_model::data(const QModelIndex &idx, int role) const
{
  if (!idx.isValid())
    return QVariant();

  if (role != Qt::DisplayRole)
    return QVariant();

  tree_item *item = static_cast<tree_item*>(idx.internalPointer());

  return item->data(idx.column());
}

void
workspace_model::set_workspace (const QString& scopes,
                                const QStringList& symbols,
                                const QStringList& class_names,
                                const QStringList& dimensions,
                                const QStringList& values)
{
  _scopes = scopes;
  _symbols = symbols;
  _class_names = class_names;
  _dimensions = dimensions;
  _values = values;

  update_tree ();

  emit model_changed ();
}

void
workspace_model::clear_workspace (void)
{
  clear_data ();

  update_tree ();

  emit model_changed ();
}

void
workspace_model::clear_data (void)
{
  _scopes = QString ();
  _symbols = QStringList ();
  _class_names = QStringList ();
  _dimensions = QStringList ();
  _values = QStringList ();
}

void
workspace_model::clear_tree (void)
{
  top_level_item(0)->delete_child_items ();
  top_level_item(1)->delete_child_items ();
  top_level_item(2)->delete_child_items ();
}

void
workspace_model::update_tree (void)
{
  beginResetModel();

  clear_tree ();

  for (int i = 0; i < _symbols.size (); i++)
    append_tree (_scopes[i], _symbols[i], _class_names[i], _dimensions[i],
                 _values[i]);

  endResetModel ();

  emit model_changed ();
}

void
workspace_model::append_tree (QChar scope, const QString& symbol,
                              const QString& class_name,
                              const QString& dimension,
                              const QString& value)
{
  tree_item *child = new tree_item ();

  child->set_data (0, symbol);
  child->set_data (1, class_name);
  child->set_data (2, dimension);
  child->set_data (3, value);

  if (scope == 'p')
    top_level_item(2)->add_child (child);
  else if (scope == 'g')
    top_level_item(1)->add_child (child);
  else
    top_level_item(0)->add_child (child);
}
