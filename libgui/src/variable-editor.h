/*

Copyright (C) 2013-2017 John W. Eaton
Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (variable_editor_h)
#define variable_editor_h 1

#include <QHeaderView>
#include <QSettings>

#include "octave-dock-widget.h"
#include "tab-bar.h"

class octave_value;

class QModelIndex;
class QStackedWidget;
class QTabWidget;
class QTableView;
class QTextEdit;
class QToolBar;

class variable_editor_model;

class var_editor_tab : public QWidget
{
  Q_OBJECT

public:

  var_editor_tab (QStackedWidget *widget_stack, QWidget *p = nullptr)
    : QWidget (p), m_widget_stack (widget_stack)
  { }

  ~var_editor_tab (void) = default;

  // No copying!

  var_editor_tab (const var_editor_tab&) = delete;

  var_editor_tab& operator = (const var_editor_tab&) = delete;

  QTableView * get_edit_view (void) const;
  QTextEdit * get_disp_view (void) const;

  void set_edit_view (QTableView *);
  void set_disp_view (QTextEdit *);

  void set_model (variable_editor_model *model)
  {
    m_model = model;
  }

  bool has_focus (void) const;

public slots:

  void set_editable (bool);

private:

  variable_editor_model *m_model;

  QStackedWidget *m_widget_stack;

  int m_edit_view_idx;
  int m_disp_view_idx;
};

// Subclassed QTabWidget for using custom tabbar

class var_editor_tab_widget : public QTabWidget
{
  Q_OBJECT

public:

  var_editor_tab_widget (QWidget *p);

  ~var_editor_tab_widget (void) = default;

  // No copying!

  var_editor_tab_widget (const var_editor_tab_widget&) = delete;

  var_editor_tab_widget& operator = (const var_editor_tab_widget&) = delete;

  tab_bar * get_tab_bar (void) const;

  bool current_tab_has_focus (void) const;

  QTextEdit *get_disp_view (void) const;
  QTableView *get_edit_view (void) const;
};


// The variable editor class

class variable_editor : public octave_dock_widget
{
  Q_OBJECT

public:

  variable_editor (QWidget *parent = nullptr);

  ~variable_editor (void) = default;

  // No copying!

  variable_editor (const variable_editor&) = delete;

  variable_editor& operator = (const variable_editor&) = delete;

  void edit_variable (const QString& name, const octave_value& val);

  QTableView *make_edit_view (var_editor_tab *page,
                              variable_editor_model *model);

  QTextEdit *make_disp_view (var_editor_tab *page,
                             variable_editor_model *model);

  void refresh (void);

  bool has_focus (void);

  static QList<QColor> default_colors (void);

  static QStringList color_names (void);

public slots:

  void callUpdate (const QModelIndex&,const QModelIndex&);

  void notice_settings (const QSettings *);

protected slots:

  void request_close_tab (bool);
  void request_close_other_tabs (bool);
  void request_close_all_tabs (bool);

  void closeEvent (QCloseEvent *);

  void closeTab (int idx);

  void contextmenu_requested (const QPoint& pt);

  void columnmenu_requested (const QPoint& pt);

  void rowmenu_requested (const QPoint& pt);

  void double_click (const QModelIndex& idx);

  void save (void);

  void clearContent (void);

  void cutClipboard (void);

  void copyClipboard (void);

  void pasteClipboard (void);

  void pasteTableClipboard (void);

  void createVariable (void);

  void transposeContent (void);

  void up (void);

  void delete_selected (void);

  // Send command to Octave interpreter.
  // %1 in CMD is replaced with the value of selected_to_octave.
  void relay_command (const QString& cmd);

signals:

  void updated (void);

  void finished (void);

  void command_requested (const QString& cmd);

  void refresh_signal (void);

private:

  QAction * add_action (QMenu *menu, const QIcon& icon, const QString& text,
                        const char *member);

  void enable_actions (void);

  QWidget *m_container;

  QToolBar *m_tool_bar;

  var_editor_tab_widget *m_tab_widget;

  tab_bar *m_tab_bar;

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

  QAction *m_close_action;
  QAction *m_close_others_action;
  QAction *m_close_all_action;

  QList<int> octave_to_coords (QString&);

  // Get the real variable name from the tab text
  QString real_var_name (int index);

  // Convert selection to an Octave expression.
  QString selected_to_octave (void);

  void update_colors (void);

  void construct_tool_bar (void);
};

#endif
