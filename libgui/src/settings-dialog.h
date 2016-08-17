/*

Copyright (C) 2011-2016 Jacob Dawid

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

#if ! defined (octave_settings_dialog_h)
#define octave_settings_dialog_h 1

#include <QDialog>
#include <QSettings>
#include <QLineEdit>

#include "color-picker.h"

namespace Ui
{
  class settings_dialog;
}

class settings_dialog:public QDialog
{
  Q_OBJECT public:
  explicit settings_dialog (QWidget * parent,
                            const QString& desired_tab = QString ());
  ~settings_dialog ();
  void show_tab (const QString&);

signals:
  void apply_new_settings ();

private slots:
  void get_octave_dir ();
  void get_file_browser_dir ();
  void get_dir (QLineEdit*, const QString&);
  void set_disabled_pref_file_browser_dir (bool disable);

  // slots for dialog's buttons
  void button_clicked (QAbstractButton *button);

  // slots for import/export-buttons of shortcut sets
  void import_shortcut_set ();
  void export_shortcut_set ();
  void default_shortcut_set ();

private:
  Ui::settings_dialog * ui;

  void write_changed_settings (bool closing);

  void read_workspace_colors (QSettings *settings);
  void write_workspace_colors (QSettings *settings);

  void read_terminal_colors (QSettings *settings);
  void write_terminal_colors (QSettings *settings);

  color_picker *_widget_title_bg_color;
  color_picker *_widget_title_bg_color_active;
  color_picker *_widget_title_fg_color;
  color_picker *_widget_title_fg_color_active;
  color_picker *_editor_current_line_color;
};

#endif
