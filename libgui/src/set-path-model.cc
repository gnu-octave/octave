////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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
#include <string>

#include <QFileIconProvider>
#include <QMessageBox>
#include <QPointer>
#include <QtAlgorithms>

#include "qt-interpreter-events.h"
#include "set-path-model.h"

#include "pathsearch.h"

#include "interpreter.h"
#include "load-path.h"

OCTAVE_BEGIN_NAMESPACE(octave)

set_path_model::set_path_model (QObject *p)
: QAbstractListModel (p)
{
  connect (this, &set_path_model::update_data_signal,
           this, &set_path_model::update_data);

  m_revertible = false;
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
  std::string path_str = to_string ();

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      load_path& lp = interp.get_load_path ();

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

  emit interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      interp.feval ("savepath");
    });
}

void set_path_model::revert (void)
{
  clear ();

  beginInsertRows (QModelIndex (), 0, m_orig_dirs.size () - 1);
  m_dirs = m_orig_dirs;
  endInsertRows ();

  model_to_path ();
}

void set_path_model::revert_last (void)
{
  clear ();

  beginInsertRows (QModelIndex (), 0, m_last_dirs.size () - 1);
  m_dirs = m_last_dirs;
  endInsertRows ();

  model_to_path ();
}

void set_path_model::add_dir (const QString& p)
{
  m_last_dirs = m_dirs;

  beginInsertRows (QModelIndex (), m_dirs.size (), m_dirs.size ());

  QList<QString>::Iterator it = m_dirs.begin();

  m_dirs.insert (it, p);

  endInsertRows ();

  model_to_path ();
}

void set_path_model::rm_dir (const QModelIndexList& indices)
{
  m_last_dirs = m_dirs;

  for (int i = indices.size () - 1; i >= 0; i--)
    {
      const QModelIndex& idx = indices.at (i);

      beginRemoveRows (idx, idx.row (), idx.row ());
      m_dirs.removeAt (idx.row ());
      endRemoveRows ();
    }

  model_to_path ();
}

void set_path_model::move_dir_up (const QModelIndexList& indices)
{
  m_last_dirs = m_dirs;

  for (int i = 0; i < indices.size (); i++)
    {
      const QModelIndex& idx = indices.at (i);

      if (idx.row () == 0 )
        continue; //  already at top position

      beginMoveRows (idx, idx.row (), idx.row (),
                     this->index (idx.row () - 1), idx.row () - 1);

      m_dirs.move (idx.row (), idx.row () - 1);

      endMoveRows ();
    }

  model_to_path ();
}

void set_path_model::move_dir_down (const QModelIndexList& indices)
{
  m_last_dirs = m_dirs;

  for (int i = indices.size () - 1; i >= 0; i--)
    {
      const QModelIndex& idx = indices.at (i);
      int bottom = m_dirs.size () - 1;

      if (idx.row () >= bottom)
        continue; //  already at bottom position

      beginMoveRows (idx, idx.row (), idx.row (),
                     this->index (idx.row () + 1), idx.row () + 1);

      m_dirs.move (idx.row (), idx.row () + 1);

      endMoveRows ();
    }

  model_to_path ();
}

void set_path_model::move_dir_top (const QModelIndexList& indices)
{
  m_last_dirs = m_dirs;

  for (int i = 0; i < indices.size (); i++)
    {
      const QModelIndex& idx = indices.at (i);

      if (idx.row () == i)
        continue; //  already at target position

      beginMoveRows (idx, idx.row (), idx.row (), this->index (i), i);

      m_dirs.move (idx.row (), i);

      endMoveRows ();
    }

  model_to_path ();
}

void set_path_model::move_dir_bottom (const QModelIndexList& indices)
{
  m_last_dirs = m_dirs;

  for (int i = 0; i < indices.size (); i++)
    {
      const QModelIndex& idx = indices.at (i);
      int target = m_dirs.size () - 1 - i;

      if (idx.row () == target)
        continue; //  already at target position

      beginMoveRows (idx, idx.row (), idx.row (),
                     this->index (target), target);

      m_dirs.move (idx.row (), target);

      endMoveRows ();
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

void set_path_model::path_to_model (void)
{
  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<set_path_model> this_spm (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // We can skip the entire callback function because it does not
      // make any changes to the interpreter state.

      if (this_spm.isNull ())
        return;

      load_path& lp = interp.get_load_path ();

      std::list<std::string> dir_list = lp.dir_list ();

      QStringList qs_dir_list;

      for (const auto& dir : dir_list)
        qs_dir_list << QString::fromStdString (dir);

      emit update_data_signal (qs_dir_list);
    });

  m_revertible = false;
}

void set_path_model::update_data (const QStringList& dirs)
{
  m_dirs = dirs;

  m_dirs.removeAll (".");

  if (! m_revertible)
    {
      // first time update
      m_orig_dirs = m_dirs;
      m_last_dirs = m_dirs;

      m_revertible = true;
    }

  int numel = m_dirs.size ();

  emit dataChanged (QAbstractListModel::index (0, 0),
                    QAbstractListModel::index (numel-1, 0));
}

OCTAVE_END_NAMESPACE(octave)
