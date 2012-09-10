/*

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

#include <list>

#include <symtab.h>

#include "workspace-model.h"
#include "octave-link.h"

workspace_model::workspace_model(QObject *parent)
  : QAbstractItemModel(parent), octave_event_observer ()
{
  QList<QVariant> rootData;
  rootData << tr ("Name") << tr ("Class") << tr("Dimension") << tr ("Value");
  _rootItem = new tree_item(rootData);

  insert_top_level_item(0, new tree_item ("Local"));
  insert_top_level_item(1, new tree_item ("Global"));
  insert_top_level_item(2, new tree_item ("Persistent"));

  connect(&_update_workspace_model_timer,
          SIGNAL (timeout ()),
          this,
          SLOT (request_update_workspace()));

  _update_workspace_model_timer.setInterval (500);
  _update_workspace_model_timer.setSingleShot (true);
  _update_workspace_model_timer.start ();
}

workspace_model::~workspace_model()
{
  delete _rootItem;
}

void
workspace_model::request_update_workspace ()
{
  octave_link::instance ()
      ->post_event (new octave_update_workspace_event (*this));
}

void
workspace_model::event_accepted (octave_event *e)
{
  if (dynamic_cast <octave_update_workspace_event*> (e))
    {
      std::list < symbol_table::symbol_record > symbolTable = symbol_table::all_variables ();

      _symbol_information.clear ();
      for (std::list < symbol_table::symbol_record > ::iterator iterator = symbolTable.begin ();
           iterator != symbolTable.end (); iterator++)
        _symbol_information.push_back (symbol_information (*iterator));

      beginResetModel();
      top_level_item (0)->delete_child_items ();
      top_level_item (1)->delete_child_items ();
      top_level_item (2)->delete_child_items ();

      foreach (const symbol_information& s, _symbol_information)
        {
          tree_item *child = new tree_item ();

          child->set_data (0, s.symbol ());
          child->set_data (1, s.class_name ());
          child->set_data (2, s.dimension ());
          child->set_data (3, s.value ());

          switch (s.scope ())
            {
              case symbol_information::local:       top_level_item (0)->add_child (child); break;
              case symbol_information::global:      top_level_item (1)->add_child (child); break;
              case symbol_information::persistent:  top_level_item (2)->add_child (child); break;
            }
        }

      endResetModel();
      emit model_changed();
    }

  // Post a new event in a given time.
  // This prevents flooding the event queue when no events are being processed.
  _update_workspace_model_timer.start ();
  delete e;
}

void
workspace_model::event_reject (octave_event *e)
{
  delete e;
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

