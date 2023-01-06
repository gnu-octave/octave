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

#if ! defined (octave_variable_editor_h)
#define octave_variable_editor_h 1

#include <QHeaderView>
#include <QSignalMapper>
#include <QStackedWidget>
#include <QTableView>

#include "dw-main-window.h"
#include "gui-settings.h"
#include "octave-dock-widget.h"
#include "qt-interpreter-events.h"
#include "tab-bar.h"

class octave_value;

class QModelIndex;
class QTextEdit;
class QToolBar;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class variable_editor_model;
class variable_editor_view;

// The individual variable subwindow class

class variable_dock_widget : public label_dock_widget
{
  Q_OBJECT

public:

  variable_dock_widget (QWidget *p, base_qobject& oct_qobj);

  ~variable_dock_widget (void) = default;

signals:

  void variable_focused_signal (const QString& name);

protected:

  virtual void closeEvent (QCloseEvent *e);

  void resizeEvent (QResizeEvent *event);

public slots:

  void handle_focus_change (QWidget *old, QWidget *now);

private slots:

  void change_floating (bool);

  void change_existence (bool);

  void toplevel_change (bool);

  void change_fullscreen (void);

protected:

  QFrame *m_frame;

  QAction *m_fullscreen_action;

  bool m_full_screen;

  bool m_prev_floating;

  QRect m_prev_geom;

  // See Octave bug #53807 and https://bugreports.qt.io/browse/QTBUG-44813
#define QTBUG_44813_FIX_VERSION 0x999999
signals:

  void queue_unfloat_float (void);

  void queue_float (void);

protected slots:

  void unfloat_float (void);

  void refloat (void);

#if (QT_VERSION >= 0x050302) && (QT_VERSION <= QTBUG_44813_FIX_VERSION)
protected:

  bool event (QEvent *event);

private:

  bool m_waiting_for_mouse_move;

  bool m_waiting_for_mouse_button_release;
#endif
};

class variable_editor_stack : public QStackedWidget
{
  Q_OBJECT

public:

  variable_editor_stack (QWidget *p, base_qobject& oct_qobj);

  ~variable_editor_stack (void) = default;

  variable_editor_view * edit_view (void) {return m_edit_view;};

  QTextEdit * disp_view (void) {return m_disp_view;};

signals:

  void edit_variable_signal (const QString& name, const octave_value& val);

  void do_save_signal (const QString& format, const QString& save_opts);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

public slots:

  void set_editable (bool editable);

  void levelUp (void);

  void save (const QString& format = QString ());

  void do_save (const QString& format, const QString& save_opts);

private:

  QTextEdit * make_disp_view (QWidget *parent);

  base_qobject& m_octave_qobj;

  variable_editor_view *m_edit_view;

  QTextEdit *m_disp_view;
};

class variable_editor_view : public QTableView
{
  Q_OBJECT

public:

  variable_editor_view (QWidget *p, base_qobject& oct_qobj);

  ~variable_editor_view (void) = default;

  void setModel (QAbstractItemModel *model);

signals:

  void command_signal (const QString& cmd);

  void add_edit_actions_signal (QMenu *menu, const QString& qualifier_string);

public slots:

  void createVariable (void);

  void transposeContent (void);

  QList<int> range_selected (void);

  void delete_selected (void);

  void clearContent (void);

  void cutClipboard (void);

  void copyClipboard (void);

  void pasteClipboard (void);

  void handle_horizontal_scroll_action (int action);

  void handle_vertical_scroll_action (int action);

  void createContextMenu (const QPoint& pt);

  void createColumnMenu (const QPoint& pt);

  void createRowMenu (const QPoint& pt);

  void selected_command_requested (const QString& cmd);

private:

  void add_edit_actions (QMenu *menu, const QString& qualifier_string);

  base_qobject& m_octave_qobj;

  variable_editor_model *m_var_model;
};

// Gadgets to keep track of and restore what variable window was in focus
// just prior to selecting something on the menu bar.

class HoverToolButton : public QToolButton
{
  Q_OBJECT

public:

  HoverToolButton (QWidget *parent = nullptr);

  ~HoverToolButton (void) = default;

signals:

  void hovered_signal (void);

  void popup_shown_signal (void);

protected:

  bool eventFilter (QObject *obj, QEvent *ev);
};

class ReturnFocusToolButton : public HoverToolButton
{
  Q_OBJECT

public:

  ReturnFocusToolButton (QWidget *parent = nullptr);

  ~ReturnFocusToolButton (void) = default;

signals:

  void about_to_activate (void);

protected:

  bool eventFilter (QObject *obj, QEvent *ev);
};

class ReturnFocusMenu : public QMenu
{
  Q_OBJECT

public:

  ReturnFocusMenu (QWidget *parent = nullptr);

  ~ReturnFocusMenu (void) = default;

signals:

  void about_to_activate (void);

protected:

  bool eventFilter (QObject *obj, QEvent *ev);
};

// The variable editor class

class variable_editor : public octave_dock_widget
{
  Q_OBJECT

public:

  variable_editor (QWidget *parent, base_qobject& oct_qobj);

  ~variable_editor (void);

  // No copying!

  variable_editor (const variable_editor&) = delete;

  variable_editor& operator = (const variable_editor&) = delete;

  void refresh (void);

  void tab_to_front (void);

signals:

  void updated (void);

  void finished (void);

  void command_signal (const QString& cmd);

  void refresh_signal (void);

  void clear_content_signal (void);

  void copy_clipboard_signal (void);

  void paste_clipboard_signal (void);

  void level_up_signal (void);

  void save_signal (void);

  void delete_selected_signal (void);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

public slots:

  void callUpdate (const QModelIndex&, const QModelIndex&);

  void notice_settings (const gui_settings *);

  void edit_variable (const QString& name, const octave_value& val);

  void variable_destroyed (QObject *obj);

  void variable_focused (const QString& name);

  void record_hovered_focus_variable (void);

  void restore_hovered_focus_variable (void);

protected slots:

  void closeEvent (QCloseEvent *);

  void save (void);

  void cutClipboard (void);

  void copyClipboard (void);

  void pasteClipboard (void);

  void levelUp (void);

protected:

  void focusInEvent (QFocusEvent *ev);

private:

  dw_main_window *m_main;

  QToolBar *m_tool_bar;
  QAction *m_save_action;

  int m_default_width;

  int m_default_height;

  int m_add_font_height;

  bool m_use_terminal_font;

  bool m_alternate_rows;

  QString m_stylesheet;

  QFont m_font;

  // If use_terminal_font is true then this will be different since
  // "font" will contain the terminal font.
  QFont m_sel_font;

  QList<QColor> m_table_colors;

  void update_colors (void);

  QAction * add_tool_bar_button (const QIcon& icon, const QString& text,
                                 const QObject *receiver, const char *member);

  void construct_tool_bar (void);

  QString m_current_focus_vname;

  QString m_hovered_focus_vname;

  QSignalMapper *m_plot_mapper;
  QSignalMapper *m_save_mapper;

  QWidget *m_focus_widget;

  variable_dock_widget *m_focus_widget_vdw;
};

OCTAVE_END_NAMESPACE(octave)

#endif
