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
#if ! defined (octave_find_files_dialog_h)
#define octave_find_files_dialog_h 1

#include <QDialog>
#include <QFileInfo>
#include <QModelIndex>

class QCheckBox;
class QDirIterator;
class QLineEdit;
class QPushButton;
class QStatusBar;
class QTableView;
class QTimer;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class find_files_dialog : public QDialog
{
  Q_OBJECT

public:

  find_files_dialog (QWidget *parent, base_qobject& oct_qobj);

  virtual ~find_files_dialog (void);

  void save_settings (void);

signals:

  void file_selected (const QString& fileName);
  void dir_selected (const QString& fileName);

public slots:

  void set_search_dir (const QString& dir);

private slots:

  void start_find (void);
  void stop_find (void);
  void browse_folders (void);
  void look_for_files (void);
  void item_double_clicked (const QModelIndex&);
  void handle_done (int);

private:

  bool is_match (const QFileInfo& info);

  base_qobject& m_octave_qobj;

  QLineEdit *m_start_dir_edit;
  QLineEdit *m_file_name_edit;
  QPushButton *m_stop_button;
  QPushButton *m_find_button;
  QPushButton *m_close_button;
  QPushButton *m_browse_button;
  QTableView *m_file_list;
  QTimer *m_timer;
  QCheckBox *m_recurse_dirs_check;
  QCheckBox *m_include_dirs_check;
  QCheckBox *m_name_case_check;
  QCheckBox *m_contains_text_check;
  QCheckBox *m_content_case_check;
  QLineEdit *m_contains_text_edit;
  QDirIterator *m_dir_iterator;
  QStatusBar *m_status_bar;
};

OCTAVE_END_NAMESPACE(octave)

#endif
