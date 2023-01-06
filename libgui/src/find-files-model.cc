////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>

#include <QFileIconProvider>
#include <QtAlgorithms>

#include "find-files-model.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class find_file_less_than
{
public:

  find_file_less_than (int ord) { m_sortorder = ord; }

  QVariant getValue (const QFileInfo& f) const
  {
    QVariant val;

    int col = (m_sortorder > 0) ? m_sortorder : -m_sortorder;

    switch (col-1)
      {
      case 0:
        val = QVariant (f.fileName ());
        break;

      case 1:
        val = QVariant (f.absolutePath ());
        break;

      default:
        break;
      }

    return val;
  }

  bool lessThan (const QVariant& left, const QVariant& right) const
  {
    return
      left.toString ().compare (right.toString (), Qt::CaseInsensitive) < 0;
  }

  bool operator () (const QFileInfo& left, const QFileInfo& right) const
  {
    QVariant leftval = getValue (left);
    QVariant rightval = getValue (right);

    if (m_sortorder > 0)
      return lessThan (leftval, rightval);
    else
      return ! lessThan (leftval, rightval);
  }

private:

  int m_sortorder;
};

find_files_model::find_files_model (QObject *p)
  : QAbstractListModel (p)
{
  m_columnNames.append (tr ("Filename"));
  m_columnNames.append (tr ("Directory"));
  m_sortorder = 0;
}

void find_files_model::clear (void)
{
  beginResetModel ();

  m_files.clear ();

  endResetModel ();
}

void find_files_model::addFile (const QFileInfo& info)
{
  beginInsertRows (QModelIndex (), m_files.size (), m_files.size ());

  QList<QFileInfo>::Iterator it;
  find_file_less_than less_than (m_sortorder);

  for (it = m_files.begin (); it != m_files.end (); it++)
    {
      if (less_than (info, *it))
        break;
    }

  m_files.insert (it, info);

  endInsertRows ();
}

int find_files_model::rowCount (const QModelIndex&) const
{
  return m_files.size ();
}

int find_files_model::columnCount (const QModelIndex&) const
{
  return m_columnNames.size ();
}

QVariant find_files_model::data (const QModelIndex& idx, int role) const
{
  QVariant retval;

  if (idx.isValid ())
    {
      if (role == Qt::DisplayRole)
        {
          switch (idx.column ())
            {
            case 0:
              retval = QVariant (m_files[idx.row ()].fileName ());
              break;

            case 1:
              retval = QVariant (m_files[idx.row ()].absolutePath ());
              break;

            default:
              break;
            }
        }
      else if (role == Qt::DecorationRole)
        {
          switch (idx.column ())
            {
            case 0:
              retval = fileIcon (idx);

            default:
              break;
            }
        }
    }

  return retval;
}

QVariant find_files_model::headerData (int section,
                                       Qt::Orientation orientation,
                                       int role) const
{
  return ((orientation == Qt::Horizontal && role == Qt::DisplayRole)
          ? m_columnNames[section] : QVariant ());
}

void find_files_model::sort (int column, Qt::SortOrder order)
{
  if (column >= 0)
    {
      if (order == Qt::DescendingOrder)
        m_sortorder = -(column+1);
      else
        m_sortorder = column+1;
    }
  else
    m_sortorder = 0;

  if (m_sortorder != 0)
    {
      beginResetModel ();

      std::sort (m_files.begin (), m_files.end (),
                 find_file_less_than (m_sortorder));

      endResetModel ();
    }
}

QFileInfo find_files_model::fileInfo (const QModelIndex& p) const
{
  return p.isValid () ? m_files[p.row ()] : QFileInfo ();
}

QIcon find_files_model::fileIcon (const QModelIndex& p) const
{
  QFileIconProvider icon_provider;

  return p.isValid () ? icon_provider.icon (m_files[p.row ()]) : QIcon ();
}

OCTAVE_END_NAMESPACE(octave)
