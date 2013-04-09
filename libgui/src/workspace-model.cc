
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
  : QAbstractTableModel (p)
{
  _columnNames.append(tr("Name"));
  _columnNames.append(tr("Class"));
  _columnNames.append(tr("Dimension"));
  _columnNames.append(tr("Value"));
}

workspace_model::~workspace_model()
{
}

int
workspace_model::rowCount(const QModelIndex &p) const
{
  return _symbols.size();
}

int
workspace_model::columnCount(const QModelIndex &p) const
{
  return _columnNames.size();
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
  {
     return _columnNames[section];
  }

  return QVariant();
}

QVariant
workspace_model::data(const QModelIndex &idx, int role) const
{
  if (!idx.isValid())
    return QVariant();
  if (role != Qt::DisplayRole)
    return QVariant();

  switch(idx.column())
  {
  case 0:
    return QVariant(_symbols[idx.row()]);
  case 1:
    return QVariant(_class_names[idx.row()]);
  case 2:
    return QVariant(_dimensions[idx.row()]);
  case 3:
    return QVariant(_values[idx.row()]);
  }
  return QVariant();
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

  update_table ();

  emit model_changed ();
}

void
workspace_model::clear_workspace (void)
{
  clear_data ();
  update_table ();

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
workspace_model::update_table (void)
{
  beginResetModel();

  // nothing to do except tell the world to recalc

  endResetModel ();

  emit model_changed ();
}

