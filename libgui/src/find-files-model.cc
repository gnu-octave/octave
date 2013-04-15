/*

Copyright (C) 2013 John Donoghue

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

#include "find-files-model.h"
#include <QFileIconProvider>

find_files_model::find_files_model (QObject *p)
  : QAbstractListModel(p)
{
  _columnNames.append (tr ("Filename"));
  _columnNames.append (tr ("Directory"));
}

find_files_model::~find_files_model ()
{
}

void 
find_files_model::clear ()
{
  beginResetModel();

  _files.clear();

  endResetModel ();
}

void 
find_files_model::addFile (const QFileInfo &info)
{
  beginInsertRows(QModelIndex(), _files.size(), _files.size() );

  _files.append(info);

  endInsertRows(); 
}

int 
find_files_model::rowCount (const QModelIndex & p) const
{
  return _files.size();
}

int 
find_files_model::columnCount (const QModelIndex & p) const
{
  return _columnNames.size ();
}

QVariant 
find_files_model::data (const QModelIndex& idx, int role) const
{
  QVariant retval;

  if (idx.isValid ())
    {
      if(role == Qt::DisplayRole)
      {
        switch (idx.column ())
          {
          case 0:
            retval = QVariant (_files[idx.row()].fileName());
            break;

          case 1:
            retval = QVariant (_files[idx.row()].absolutePath());
            break;

          default:
            break;
          }
      }
      else if(role == Qt:: DecorationRole)
      {
        switch (idx.column())
          {
          case 0:
            retval = fileIcon(idx);
          default:
            break;
          }
      }
    }

  return retval;
}

QVariant 
find_files_model::headerData (int section, Qt::Orientation orientation, int role) const
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return _columnNames[section];
  else
    return QVariant ();
}

QFileInfo 
find_files_model::fileInfo (const QModelIndex & p) const
{
  if(p.isValid ())
  {
    return _files[p.row()];
  }
  return QFileInfo ();
}

QIcon
find_files_model::fileIcon (const QModelIndex &p) const
{
  QFileIconProvider icon_provider;
  if(p.isValid ())
  {
    return icon_provider.icon (_files[p.row()]);
  }
  return QIcon ();
}
