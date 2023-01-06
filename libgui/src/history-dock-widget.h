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

#if ! defined (octave_history_dock_widget_h)
#define octave_history_dock_widget_h 1

#include <QCheckBox>
#include <QComboBox>
#include <QLineEdit>
#include <QListView>
#include <QSortFilterProxyModel>
#include <QStringListModel>

#include "octave-dock-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class history_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  history_dock_widget (QWidget *parent, base_qobject& oct_qobj);

  ~history_dock_widget (void) = default;

signals:

  //! Signal emitted whenever the user double-clicks a command in the
  //! history.

  void command_double_clicked (const QString& command);

  //! Signal emitted whenever the user selects commands and chooses
  //! "Create script" from the popup menu.

  void command_create_script (const QString& commands);

public slots:

  void set_history (const QStringList& hist);
  void append_history (const QString& hist_entry);
  void clear_history (void);
  void save_settings (void);
  void notice_settings (const gui_settings *);

private slots:

  void update_filter_history (void);
  void filter_activate (bool enable);

  void ctxMenu (const QPoint& pos);
  void handle_double_click (QModelIndex modelIndex);
  void handle_contextmenu_copy (bool flag);
  void handle_contextmenu_evaluate (bool flag);
  void handle_contextmenu_create_script (bool flag);
  void handle_contextmenu_filter (void);

  void copyClipboard (void);
  void pasteClipboard (void);
  void selectAll (void);

  virtual void handle_visibility (bool visible);

private:

  void construct (void);
  void set_filter_focus (bool focus);

  QListView *m_history_list_view;
  QSortFilterProxyModel m_sort_filter_proxy_model;

  //! Stores the current history_model.

  QStringListModel *m_history_model;

  QCheckBox *m_filter_checkbox;
  QComboBox *m_filter;
  QWidget *m_filter_widget;
  bool m_filter_shown;

  enum { MaxFilterHistory = 10 };
};

OCTAVE_END_NAMESPACE(octave)

#endif
