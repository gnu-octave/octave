/*

Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld
Copyright (C) 2013 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifndef variable_editor_h
#define variable_editor_h

#include "octave-dock-widget.h"
#include <QHeaderView>
#include <QSettings>

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

  //~variable_editor ();

  void edit_variable (const QString& name);

  /// Clear all the models' data cache
  void clear_data_cache ();

  bool has_focus ();

  static QList<QColor> default_colors ();
  static QStringList color_names ();

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

  void save ();
  void clearContent ();
  void cutClipboard ();
  void copyClipboard ();
  void pasteClipboard ();
  void pasteTableClipboard ();
  void createVariable ();
  void transposeContent ();
  void up ();

  void delete_selected();

  /** Send command to Octave interpreter.
   * %1 in CMD is replaced with the value of selected_to_octave.
   */
  void relay_command (const QString& cmd);

signals:

  void updated ();
  void finished ();
  void command_requested (const QString& cmd);

private:
  QMainWindow *main;
  QToolBar *tool_bar;
  QTabWidget *tab_widget;

  int default_width;
  int default_height;
  int add_font_height;

  bool autofit;
  bool autofit_max;
  bool use_terminal_font;
  bool alternate_rows;

  QString stylesheet;

  QFont font;

  // If use_terminal_font then this will be different since "font"
  // will contain the terminal font.
  QFont sel_font;
  QList<QColor> table_colors;

  void update_colors ();

  void construct_tool_bar ();

  // Convert selection to an Octave expression.
  QString selected_to_octave ();

  QList<int> octave_to_coords (QString&);
};

#endif //variable_editor_h
