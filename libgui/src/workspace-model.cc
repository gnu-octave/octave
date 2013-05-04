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
#include <QSettings>

#include "utils.h"
#include "resource-manager.h"
#include "workspace-model.h"

workspace_model::workspace_model (QObject *p)
  : QAbstractTableModel (p)
{
  _columnNames.append (tr ("Name"));
  _columnNames.append (tr ("Class"));
  _columnNames.append (tr ("Dimension"));
  _columnNames.append (tr ("Value"));
  _columnNames.append (tr ("Storage Class"));

  for (int i = 0; i < resource_manager::storage_class_chars ().length (); i++)
    _storage_class_colors.append (QColor (Qt::white));

}

int
workspace_model::rowCount (const QModelIndex&) const
{
  return _symbols.size ();
}

int
workspace_model::columnCount (const QModelIndex&) const
{
  return _columnNames.size ();
}

Qt::ItemFlags
workspace_model::flags (const QModelIndex& idx) const
{
  Qt::ItemFlags retval = 0;

  if (idx.isValid ())
    {
      retval |= Qt::ItemIsEnabled;

      if (_top_level && idx.column () == 0)
        retval |= Qt::ItemIsSelectable;
    }

  return retval;
}

QVariant
workspace_model::headerData (int section, Qt::Orientation orientation,
                             int role) const
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return _columnNames[section];
  else
    return QVariant();
}

QVariant
workspace_model::data (const QModelIndex& idx, int role) const
{
  QVariant retval;

  if (idx.isValid ())
    {
      if (role == Qt::BackgroundColorRole)
        {
          QString class_chars = resource_manager::storage_class_chars ();
          int actual_class = class_chars.indexOf (_scopes[idx.row()].toAscii ());
          if (actual_class >= 0)
            return QVariant (_storage_class_colors.at (actual_class));
          else
            return retval;
        }

      if (role == Qt::DisplayRole
          || (idx.column () == 0 && role == Qt::EditRole)
          || (idx.column () == 0 && role == Qt::ToolTipRole) )
        {
          switch (idx.column ())
            {
            case 0:
              if (role == Qt::ToolTipRole)
                retval = QVariant (tr ("Right click to copy, rename, or display"));
              else
                retval = QVariant (_symbols[idx.row()]);
              break;

            case 1:
              retval = QVariant (_class_names[idx.row()]);
              break;

            case 2:
              retval = QVariant (_dimensions[idx.row()]);
              break;

            case 3:
              retval = QVariant (_values[idx.row()]);
              break;

            case 4:
              retval = QVariant ();
              QString class_chars = resource_manager::storage_class_chars ();
              int actual_class = class_chars.indexOf (_scopes[idx.row()].toAscii ());
              if (actual_class >= 0)
                {
                  QStringList class_names = resource_manager::storage_class_names ();
                  retval = QVariant (class_names.at (actual_class));
                }
              break;

          }
      }
    }

  return retval;
}

bool
workspace_model::setData (const QModelIndex& idx, const QVariant& value,
                          int role)
{
  bool retval = false;

  if (idx.column () == 0 && role == Qt::EditRole)
    {
      QString qold_name = _symbols[idx.row()];

      QString qnew_name = value.toString ();

      std::string new_name = qnew_name.toStdString ();

      if (valid_identifier (new_name))
        {
          emit rename_variable (qold_name, qnew_name);

          retval = true;
        }
    }

  return retval;
}


void
workspace_model::set_workspace (bool top_level,
                                const QString& scopes,
                                const QStringList& symbols,
                                const QStringList& class_names,
                                const QStringList& dimensions,
                                const QStringList& values)
{
  _top_level = top_level;
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
  _top_level = false;
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

  // Nothing to do except tell the world to recalc.

  endResetModel ();

  emit model_changed ();
}

void
workspace_model::notice_settings (const QSettings *settings)
{
  QList<QColor> default_colors = resource_manager::storage_class_default_colors ();
  QString class_chars = resource_manager::storage_class_chars ();

  for (int i = 0; i < class_chars.length (); i++)
    {
      QVariant default_var = default_colors.at (i);
      QColor setting_color = settings->value ("workspaceview/color_"+class_chars.mid (i,1),
                                             default_var).value<QColor> ();
      _storage_class_colors.replace (i,setting_color);
    }
}
