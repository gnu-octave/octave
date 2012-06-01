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

#include "workspace-model.h"
#include <QTreeWidget>
#include <QTime>
#include "octave-link.h"

workspace_model::workspace_model(QObject *parent)
  : QAbstractItemModel(parent)
{
  QList<QVariant> rootData;
  rootData << tr ("Name") << tr ("Type") << tr ("Value");
  _rootItem = new tree_item(rootData);
}

workspace_model::~workspace_model()
{
  delete _rootItem;
}

QModelIndex
workspace_model::index(int row, int column, const QModelIndex &parent) const
{
  if (!hasIndex(row, column, parent))
    return QModelIndex();

  tree_item *parentItem;

  if (!parent.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<tree_item*>(parent.internalPointer());

  tree_item *childItem = parentItem->child(row);
  if (childItem)
    return createIndex(row, column, childItem);
  else
    return QModelIndex();
}

QModelIndex
workspace_model::parent(const QModelIndex &index) const
{
  if (!index.isValid())
    return QModelIndex();

  tree_item *childItem = static_cast<tree_item*>(index.internalPointer());
  tree_item *parentItem = childItem->parent();

  if (parentItem == _rootItem)
    return QModelIndex();

  return createIndex(parentItem->row(), 0, parentItem);
}

int
workspace_model::rowCount(const QModelIndex &parent) const
{
  tree_item *parentItem;
  if (parent.column() > 0)
    return 0;

  if (!parent.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<tree_item*>(parent.internalPointer());

  return parentItem->child_count();
}

int
workspace_model::columnCount(const QModelIndex &parent) const
{
  if (parent.isValid())
    return static_cast<tree_item*>(parent.internalPointer())->column_count();
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
workspace_model::flags(const QModelIndex &index) const
{
  if (!index.isValid())
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
workspace_model::data(const QModelIndex &index, int role) const
{
  if (!index.isValid())
    return QVariant();

  if (role != Qt::DisplayRole)
    return QVariant();

  tree_item *item = static_cast<tree_item*>(index.internalPointer());

  return item->data(index.column());
}


void
workspace_model::update_from_symbol_table ()
{
  top_level_item (0)->delete_child_items ();
  top_level_item (1)->delete_child_items ();
  top_level_item (2)->delete_child_items ();
  top_level_item (3)->delete_child_items ();

  octave_link::instance ()-> acquire_symbol_information();
  const QList <symbol_information>& symbolInformation = octave_link::instance() ->get_symbol_information ();

  foreach (const symbol_information& s, symbolInformation)
    {
      tree_item *child = new tree_item ();

      child->set_data (0, s._symbol);
      child->set_data (1, s._type);
      child->set_data (2, s._value);

      switch (s._scope)
        {
          case symbol_information::local:       top_level_item (0)->add_child (child); break;
          case symbol_information::global:      top_level_item (1)->add_child (child); break;
          case symbol_information::persistent:  top_level_item (2)->add_child (child); break;
          case symbol_information::hidden:      top_level_item (3)->add_child (child); break;
        }
    }

  octave_link::instance ()-> release_symbol_information();

  reset();
  emit expand_request();
}
