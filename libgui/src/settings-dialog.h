/*

Copyright (C) 2011-2018 Jacob Dawid

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_settings_dialog_h)
#define octave_settings_dialog_h 1

#include <QCheckBox>
#include <QDialog>
#include <QSettings>
#include <QLineEdit>
#include <QRadioButton>

#include "color-picker.h"
#include "gui-preferences.h"
#include "ui-settings-dialog.h"

class QsciLexer;

namespace octave
{
  // Ui::settings_dialog is a generated class.

  class settings_dialog : public QDialog, private Ui::settings_dialog
  {
    Q_OBJECT public:

    explicit settings_dialog (QWidget *parent,
                              const QString& desired_tab = QString ());

    ~settings_dialog (void) = default;

    void show_tab (const QString&);

  signals:

    void apply_new_settings (void);

  private slots:

    void get_octave_dir (void);
    void get_file_browser_dir (void);
    void get_dir (QLineEdit*, const QString&);
    void set_disabled_pref_file_browser_dir (bool disable);

    // slots for dialog's buttons
    void button_clicked (QAbstractButton *button);

    // slots for import/export-buttons of shortcut sets
    void import_shortcut_set (void);
    void export_shortcut_set (void);
    void default_shortcut_set (void);

  private:

    void read_lexer_settings (QsciLexer *lexer, QSettings *settings);
    void write_lexer_settings (QsciLexer *lexer, QSettings *settings);

    void write_changed_settings (bool closing);

    void read_workspace_colors (QSettings *settings);
    void write_workspace_colors (QSettings *settings);

    void read_terminal_colors (QSettings *settings);
    void write_terminal_colors (QSettings *settings);

    void read_varedit_colors (QSettings *settings);
    void write_varedit_colors (QSettings *settings);

    color_picker *m_widget_title_bg_color;
    color_picker *m_widget_title_bg_color_active;
    color_picker *m_widget_title_fg_color;
    color_picker *m_widget_title_fg_color_active;
    color_picker *m_editor_current_line_color;

    QRadioButton *m_rb_comment_strings[ed_comment_strings_count];
    QRadioButton *m_rb_uncomment_strings[ed_comment_strings_count];

    QCheckBox *m_ws_enable_colors;
    QCheckBox *m_ws_hide_tool_tips;
  };
}

#endif
