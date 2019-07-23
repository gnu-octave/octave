/*

Copyright (C) 2019 JunWang

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include <string>
#include <algorithm>

#include <QFileIconProvider>
#include <QtAlgorithms>
#include <QMessageBox>

#include "pathsearch.h"

#include "event-manager.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"

#include "set-path-model.h"

namespace octave
{
  set_path_model::set_path_model (QObject *p)
    : QAbstractListModel (p)
  {
    connect (this, SIGNAL (update_data_signal (const QStringList&)),
             this, SLOT (update_data (const QStringList&)));

    construct ();
  }

  std::string set_path_model::to_string (void)
  {
    std::string path_sep = directory_path::path_sep_str ();

    std::string path_str;

    QStringList::iterator it = m_dirs.begin ();

    while (it < m_dirs.end ())
      {
        if (it != m_dirs.begin ())
          path_str += path_sep;
        path_str += it->toStdString ();
        ++it;
      }

    return path_str;
  }

  void set_path_model::model_to_path (void)
  {
    event_manager& evmgr
      = __get_event_manager__ ("set_path_model::model_to_path");

    std::string path_str = to_string ();

    evmgr.post_event
      ([path_str] (void)
       {
         // INTERPRETER THREAD

         load_path& lp = __get_load_path__ ("set_path_model::model_to_path");

         lp.set (path_str);
       });
  }

  void set_path_model::clear (void)
  {
    beginResetModel ();

    m_dirs.clear ();

    endResetModel ();
  }

  void set_path_model::save (void)
  {
    model_to_path ();

    event_manager& evmgr = __get_event_manager__ ("set_path_model::save");

    evmgr.post_event
      ([] (void)
       {
         // INTERPRETER THREAD

         interpreter& interp = __get_interpreter__ ("set_path_model::save");

         interp.feval ("savepath");
       });
  }

  void set_path_model::revert (void)
  {
    clear ();
    beginInsertRows (QModelIndex (), 0, m_old_dirs.size () - 1);
    m_dirs = m_old_dirs;
    endInsertRows ();
    model_to_path ();
  }

  void set_path_model::addPath (const QString& p)
  {
    beginInsertRows (QModelIndex (), m_dirs.size (), m_dirs.size ());

    QList<QString>::Iterator it = m_dirs.begin();

    m_dirs.insert (it, p);

    endInsertRows ();

    model_to_path ();
  }

  void set_path_model::rmPath (const QModelIndexList& indices)
  {
    QModelIndexList sorted_indices (indices);
    std::sort (sorted_indices.begin (), sorted_indices.end ());
    QStringList::iterator it = m_dirs.begin ();
    int i = 0;
    int index_top = 0;
    while (it < m_dirs.end ())
      {
        if (index_top < sorted_indices.size ()
            && i == sorted_indices[index_top].row ())
          {
            int dis = std::distance (m_dirs.begin (), it);
            beginRemoveRows (QModelIndex (), dis, dis);
            it = m_dirs.erase (it);
            endRemoveRows ();
            ++index_top;
          }
        else
          ++it;

        ++i;
      }

    model_to_path ();
  }

  void set_path_model::moveUpPath (const QModelIndexList& indices)
  {
    QModelIndexList sorted_indices (indices);
    std::sort (sorted_indices.begin (), sorted_indices.end ());
    for (const auto& each : sorted_indices)
      {
        if (each.row () == 0)
          continue;

        beginMoveRows (QModelIndex (), each.row (), each.row (),
                       QModelIndex (), each.row () - 1);

        m_dirs.swap (each.row () - 1, each.row ());

        endMoveRows ();
      }

    model_to_path ();
  }

  void set_path_model::moveDownPath (const QModelIndexList& indices)
  {
    QModelIndexList sorted_indices (indices);
    std::sort (sorted_indices.begin (), sorted_indices.end ());
    std::reverse (sorted_indices.begin (), sorted_indices.end ());
    for (const auto& each : sorted_indices)
      {
        if (each.row () == m_dirs.size () - 1)
          continue;

        beginMoveRows (QModelIndex (), each.row(), each.row (),
                       QModelIndex (), each.row () + 2);

        m_dirs.swap (each.row(), each.row() + 1);

        endMoveRows ();
      }

    model_to_path ();
  }

  void set_path_model::moveToTopPath (const QModelIndexList& indices)
  {
    QModelIndexList sorted_indices (indices);
    std::sort (sorted_indices.begin (), sorted_indices.end ());

    QStringList::iterator it = m_dirs.begin ();
    int i = 0;
    int index_top = 0;
    while (it < m_dirs.end ())
      {
        if (index_top < indices.size () && indices[index_top].row () == i)
          {
            int distance = std::distance (m_dirs.begin (), it);
            beginMoveRows (QModelIndex (), distance, distance,
                           QModelIndex (), m_dirs.size ());
            QString tmp = *it;
            it = m_dirs.erase (it);
            m_dirs.push_front (tmp);
            endMoveRows ();
            ++index_top;
          }
        else
          ++it;

        ++i;
      }

    std::reverse (m_dirs.begin (), m_dirs.begin () + indices.size ());

    model_to_path ();
  }

  void set_path_model::moveToBottomPath (const QModelIndexList& indices)
  {
    QModelIndexList sorted_indices (indices);
    std::sort (sorted_indices.begin (), sorted_indices.end ());

    QStringList::iterator it = m_dirs.begin ();
    int i = 0;
    int index_top = 0;
    while (it < m_dirs.end ())
      {
        if (index_top < indices.size () && indices[index_top].row () == i)
          {
            int distance = std::distance (m_dirs.begin (), it);

            beginMoveRows (QModelIndex (), distance, distance,
                           QModelIndex (), m_dirs.size ());

            QString tmp = *it;
            it = m_dirs.erase (it);
            m_dirs.push_back (tmp);

            endMoveRows ();
            ++index_top;
          }
        else
          ++it;

        ++i;
      }

    model_to_path ();
  }

  int set_path_model::rowCount (const QModelIndex&) const
  {
    return m_dirs.size ();
  }

  QVariant set_path_model::data (const QModelIndex& idx, int role) const
  {
    QVariant retval;
    if (idx.isValid ())
      {
        switch (role)
          {
          case Qt::DisplayRole:
            retval = QVariant (m_dirs[idx.row ()]);
            break;

          case Qt::DecorationRole:
            retval = QVariant (QIcon ());
            break;

          case Qt::SizeHintRole:
            retval = QVariant (QSize (10, 20));
            break;
          }
      }

    return retval;
  }

  void set_path_model::construct (void)
  {
    event_manager& evmgr = __get_event_manager__ ("set_path_model::construct");

    evmgr.post_event
      ([this] (void)
       {
         load_path& lp = __get_load_path__ ("set_path_model::construct");

         std::list<std::string> dir_list = lp.dir_list ();

         QStringList qs_dir_list;

         for (const auto& dir : dir_list)
           qs_dir_list << QString::fromStdString (dir);

         emit update_data_signal (qs_dir_list);
       });
  }

  void set_path_model::update_data (const QStringList& dirs)
  {
    m_old_dirs = m_dirs;

    m_dirs = dirs;

    m_dirs.removeAll (".");

    int numel = m_dirs.size ();

    emit dataChanged (QAbstractListModel::index (0, 0),
                      QAbstractListModel::index (numel-1, 0));
  }
}
