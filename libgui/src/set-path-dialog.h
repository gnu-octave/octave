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
#if ! defined (octave_set_path_dialog_h)
#define octave_set_path_dialog_h 1

#include <QDialog>
#include <QFileInfo>
#include <QModelIndex>

#include "qt-interpreter-events.h"

class octave_value_list;

class QLabel;
class QPushButton;
class QListView;
class QVBoxLayout;
class QHBoxLayout;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class set_path_dialog : public QDialog
{
  Q_OBJECT

public:

  // You must call update_model to fully initialize the path displayed
  // in the dialog.  That may only be done after the intepreter_event
  // signal connections are made to the Octave interpreter.

  set_path_dialog (QWidget *parent, base_qobject& oct_qobj);

  virtual ~set_path_dialog (void) = default;

  void save_settings (void);

signals:

  //! Emitted, when the path has to be modified

  void modify_path_signal (const QStringList& dir_list, bool rm,
                           bool subdirs);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

public slots:

  void update_model (void);

protected:

  void closeEvent (QCloseEvent *e);

private slots:

  void add_dir (void);
  void add_dir_subdirs (void);

  void rm_dir (void);

  void move_dir_up (void);

  void move_dir_down (void);

  void move_dir_top (void);

  void move_dir_bottom (void);

private:

  void add_dir_common (bool subdirs);

  base_qobject& m_octave_qobj;

  QLabel *m_info_label;
  QPushButton *m_reload_button;
  QPushButton *m_save_button;
  QPushButton *m_close_button;
  QPushButton *m_revert_button;
  QPushButton *m_revert_last_button;

  QListView *m_path_list;

  QPushButton *m_add_folder_button;
  QPushButton *m_move_to_top_button;
  QPushButton *m_move_to_bottom_button;
  QPushButton *m_move_up_button;
  QPushButton *m_move_down_button;
  QPushButton *m_remove_button;
};

OCTAVE_END_NAMESPACE(octave)

#endif
