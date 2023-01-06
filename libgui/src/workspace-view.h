////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#if ! defined (octave_workspace_view_h)
#define octave_workspace_view_h 1

#include <QCheckBox>
#include <QComboBox>
#include <QItemDelegate>
#include <QSemaphore>
#include <QSignalMapper>
#include <QSortFilterProxyModel>
#include <QTableView>

#include "octave-dock-widget.h"
#include "workspace-model.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class workspace_view : public octave_dock_widget
{
  Q_OBJECT

public:

  workspace_view (QWidget *parent, base_qobject& oct_qobj);

  ~workspace_view (void) = default;

signals:

  //! Signal that user had requested a command on a variable.

  void command_requested (const QString& cmd);

  //! Signal that user wnats to copy a variable value to the
  //! clipboard.

  void copy_variable_value_to_clipboard (const QString&);

  //! Signal that user wants to rename a variable.

  void rename_variable_signal (const QString&, const QString&);

  //! Signal that user wants to edit a variable.

  void edit_variable_signal (const QString&);

public slots:

  void setModel (workspace_model *model);

  void notice_settings (const gui_settings *);

  void save_settings (void);

protected slots:

  void filter_update (const QString& expression);
  void filter_activate (bool enable);
  void update_filter_history (void);

  void header_contextmenu_requested (const QPoint& mpos);

  void toggle_header (int column);

  void contextmenu_requested (const QPoint& pos);

  void handle_contextmenu_copy (void);
  void handle_contextmenu_copy_value (void);
  void handle_contextmenu_rename (void);
  void handle_contextmenu_edit (void);
  void handle_contextmenu_clear (void);
  void handle_contextmenu_disp (void);
  void handle_contextmenu_plot (void);
  void handle_contextmenu_stem (void);
  void handle_contextmenu_filter (void);

public slots:

  void handle_model_changed (void);

  void copyClipboard (void);
  void selectAll (void);

private:

  void relay_contextmenu_command (const QString& cmdname, bool str = false);
  void set_filter_focus (bool focus);

  QString get_var_name (const QModelIndex& index);

  QTableView *m_view;
  int m_view_previous_row_count;

  // We are using a sort model proxy so m_model won't provide the
  // correct ordering.  It is still OK to use this pointer to access
  // other info attached to the model, for example the scope or colors.
  workspace_model *m_model;

  QSortFilterProxyModel m_filter_model;
  QCheckBox *m_filter_checkbox;
  QComboBox *m_filter;
  QWidget *m_filter_widget;
  bool m_filter_shown;
};

OCTAVE_END_NAMESPACE(octave)

#endif
