// Find dialog derived from an example from Qt Toolkit (license below (**))

////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// or <https://octave.org/copyright/>.
//
//  All rights reserved.
//  Contact: Nokia Corporation (qt-info@nokia.com)
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
// ** This file is part of the examples of the Qt Toolkit.
// **
// ** $QT_BEGIN_LICENSE:LGPL$
// ** Commercial Usage
// ** Licensees holding valid Qt Commercial licenses may use this file in
// ** accordance with the Qt Commercial License Agreement provided with the
// ** Software or, alternatively, in accordance with the terms contained in
// ** a written agreement between you and Nokia.
// **
// ** GNU Lesser General Public License Usage
// ** Alternatively, this file may be used under the terms of the GNU Lesser
// ** General Public License version 2.1 as published by the Free Software
// ** Foundation and appearing in the file LICENSE.LGPL included in the
// ** packaging of this file.  Please review the following information to
// ** ensure the GNU Lesser General Public License version 2.1 requirements
// ** will be met: https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
// **
// ** In addition, as a special exception, Nokia gives you certain additional
// ** rights.  These rights are described in the Nokia Qt LGPL Exception
// ** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
// **
// ** GNU General Public License Usage
// ** Alternatively, this file may be used under the terms of the GNU
// ** General Public License version 3.0 as published by the Free Software
// ** Foundation and appearing in the file LICENSE.GPL included in the
// ** packaging of this file.  Please review the following information to
// ** ensure the GNU General Public License version 3.0 requirements will be
// ** met: https://www.gnu.org/copyleft/gpl.html.
// **
// ** If you have questions regarding the use of this file, please contact
// ** Nokia at qt-info@nokia.com.
// ** $QT_END_LICENSE$
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_find_dialog_h)
#define octave_find_dialog_h 1

#include <QDialog>
#include <QComboBox>

#include "octave-qscintilla.h"
#include "octave-dock-widget.h"

class QCheckBox;
class QDialogButtonBox;
class QGroupBox;
class QLabel;
class QLineEdit;
class QPushButton;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class file_editor;

class find_dialog : public QDialog
{
  Q_OBJECT

public:

  find_dialog (base_qobject& oct_qobj, octave_dock_widget *ed, QWidget *p);

  //! Set dialog visible or not and storing the new visibility state
  void set_visible (bool visible);

  //! Init the search text with the selected text in the editor tab
  void init_search_text (void);

  //! Restore position and the search options from the given settings
  //! where def_pos is the default position suitable for the current
  //! editor position
  void restore_settings (QPoint def_pos);

public slots:

  void find_next (void);
  void find_prev (void);

  //! Slot for updating the edit area when the active tab has changed
  void update_edit_area (octave_qscintilla *);

private slots:

  void handle_sel_search_changed (int);
  void handle_selection_changed (bool has_selected);

  void handle_backward_search_changed (int);

  void find (bool forward = true);
  void replace (void);
  void replace_all (void);

private:

  base_qobject& m_octave_qobj;

  //! Save position and the search options in the given settings
  void save_settings ();

  //! Reimplemented slot: close instead of hiding
  void reject ();

  //! Reimplemented close event
  void closeEvent (QCloseEvent *e);

  //! Update mru lists with new entry
  void mru_update (QComboBox *mru);

  void no_matches_message (void);
  void do_replace (void);

  void handle_search_text_changed (void);
  void handle_replace_text_changed (void);

  octave_dock_widget *m_editor;

  QLabel *m_search_label;
  QComboBox *m_search_line_edit;
  QLabel *m_replace_label;
  QComboBox *m_replace_line_edit;
  QCheckBox *m_case_check_box;
  QCheckBox *m_from_start_check_box;
  QCheckBox *m_wrap_check_box;
  QCheckBox *m_whole_words_check_box;
  QCheckBox *m_regex_check_box;
  QCheckBox *m_search_selection_check_box;
  QCheckBox *m_backward_check_box;
  QDialogButtonBox *m_button_box;
  QPushButton *m_find_next_button;
  QPushButton *m_find_prev_button;
  QPushButton *m_replace_button;
  QPushButton *m_replace_all_button;
  QPushButton *m_more_button;
  QWidget *m_extension;
  octave_qscintilla *m_edit_area;
  bool m_find_result_available;
  int m_rep_all;
  bool m_rep_active;

  bool m_in_sel;
  int m_sel_beg;
  int m_sel_end;

  QPoint m_last_position;

  const int m_mru_length = 10;
};

OCTAVE_END_NAMESPACE(octave)

#endif
