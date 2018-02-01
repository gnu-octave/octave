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

class octave_value;

class QTabWidget;
class QToolBar;
class QMainWindow;
class QTableView;
class QModelIndex;

class variable_editor : public octave_dock_widget
{
  Q_OBJECT

public:

  variable_editor (QWidget *parent = nullptr);

  ~variable_editor (void);

  // No copying!

  variable_editor (const variable_editor&) = delete;

  variable_editor& operator = (const variable_editor&) = delete;

  void edit_variable (const QString& name, const octave_value& val);

  void refresh (void);

  bool has_focus (void);

  static QList<QColor> default_colors (void);

  static QStringList color_names (void);

public slots:

  void callUpdate (const QModelIndex&,const QModelIndex&);

  void notice_settings (const QSettings *);

protected slots:

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

private:

  QMainWindow *m_main;

  QToolBar *m_tool_bar;

  QTabWidget *m_tab_widget;

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

  QList<int> octave_to_coords (QString&);

  // Get the real variable name from the tab text
  QString real_var_name (int index);

  // Convert selection to an Octave expression.
  QString selected_to_octave (void);

  void update_colors (void);

  void construct_tool_bar (void);
};

#endif
