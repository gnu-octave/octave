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

// Programming Note: this file has many lines longer than 80 characters
// due to long function, variable, and property names.  Please don't
// break those lines as it tends to make this code even harder to read.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QButtonGroup>
#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QHash>
#include <QMessageBox>
#include <QScrollBar>
#include <QStyleFactory>
#include <QTextCodec>
#include <QVector>

#if defined (HAVE_QSCINTILLA)
#  include "octave-qscintilla.h"
#  include "octave-txt-lexer.h"
#  include <QScrollArea>

#  if defined (HAVE_QSCI_QSCILEXEROCTAVE_H)
#    define HAVE_LEXER_OCTAVE 1
#    include <Qsci/qscilexeroctave.h>
#  elif defined (HAVE_QSCI_QSCILEXERMATLAB_H)
#    define HAVE_LEXER_MATLAB 1
#    include <Qsci/qscilexermatlab.h>
#  endif

#  include <Qsci/qscilexercpp.h>
#  include <Qsci/qscilexerjava.h>
#  include <Qsci/qscilexerbash.h>
#  include <Qsci/qscilexerperl.h>
#  include <Qsci/qscilexerbatch.h>
#  include <Qsci/qscilexerdiff.h>
#endif

#include "gui-preferences-all.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "settings-dialog.h"
#include "variable-editor.h"
#include "workspace-model.h"

OCTAVE_BEGIN_NAMESPACE(octave)

settings_dialog::settings_dialog (QWidget *p, base_qobject& oct_qobj,
                                  const QString& desired_tab)
: QDialog (p), Ui::settings_dialog (), m_octave_qobj (oct_qobj)
{
  setupUi (this);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings)
    {
      QMessageBox msgBox
        (QMessageBox::Warning, tr ("Octave Preferences"),
         tr ("Unable to save preferences.  Missing preferences file or unknown directory."));

      msgBox.exec ();

      return;
    }

  // look for available language files and the actual settings
  QString qm_dir_name = rmgr.get_gui_translation_dir ();
  QDir qm_dir (qm_dir_name);
  QFileInfoList qm_files = qm_dir.entryInfoList (QStringList ("*.qm"),
                                                 QDir::Files | QDir::Readable, QDir::Name);

  for (int i = 0; i < qm_files.length (); i++)   // insert available languages
    comboBox_language->addItem (qm_files.at (i).baseName ());
  // System at beginning
  comboBox_language->insertItem (0, tr ("System setting"));
  comboBox_language->insertSeparator (1);    // separator after System
  QString language = settings->value (global_language.key,
                                      global_language.def).toString ();
  if (language == global_language.def.toString ())
    language = tr ("System setting");
  int selected = comboBox_language->findText (language);
  if (selected >= 0)
    comboBox_language->setCurrentIndex (selected);
  else
    comboBox_language->setCurrentIndex (0);  // System is default

  // Global style
  QStringList styles = QStyleFactory::keys();
  styles.append (global_extra_styles);
  combo_styles->addItems (styles);
  combo_styles->insertItem (0, global_style.def.toString ());
  combo_styles->insertSeparator (1);
  QString current_style = settings->value (global_style).toString ();
  if (current_style == global_style.def.toString ())
    current_style = global_style.def.toString ();
  selected = combo_styles->findText (current_style);
  if (selected >= 0)
    combo_styles->setCurrentIndex (selected);
  else
    combo_styles->setCurrentIndex (0);

  // icon size and theme
  QButtonGroup *icon_size_group = new QButtonGroup (this);
  icon_size_group->addButton (icon_size_small);
  icon_size_group->addButton (icon_size_normal);
  icon_size_group->addButton (icon_size_large);
  int icon_size = settings->value (global_icon_size).toInt ();
  icon_size_normal->setChecked (true);  // the default
  icon_size_small->setChecked (icon_size < 0);
  icon_size_large->setChecked (icon_size > 0);
  combo_box_icon_theme->addItems (global_all_icon_theme_names);
  int theme = settings->value (global_icon_theme_index.key).toInt ();
  combo_box_icon_theme->setCurrentIndex (theme);

  // which icon has to be selected
  QButtonGroup *icon_group = new QButtonGroup (this);
  icon_group->addButton (general_icon_octave);
  icon_group->addButton (general_icon_graphic);
  icon_group->addButton (general_icon_letter);
  QString widget_icon_set =
    settings->value (dw_icon_set).toString ();
  general_icon_octave->setChecked (true);  // the default (if invalid set)
  general_icon_octave->setChecked (widget_icon_set == "NONE");
  general_icon_graphic->setChecked (widget_icon_set == "GRAPHIC");
  general_icon_letter->setChecked (widget_icon_set == "LETTER");

  // custom title bar of dock widget
  QColor bg_color = settings->value (dw_title_bg_color).value<QColor> ();
  m_widget_title_bg_color = new color_picker (bg_color);
  m_widget_title_bg_color->setEnabled (false);
  layout_widget_bgtitle->addWidget (m_widget_title_bg_color, 0);

  connect (cb_widget_custom_style, &QCheckBox::toggled,
           m_widget_title_bg_color, &color_picker::setEnabled);

  QColor bg_color_active = settings->value (dw_title_bg_color_active).value<QColor> ();
  m_widget_title_bg_color_active = new color_picker (bg_color_active);
  m_widget_title_bg_color_active->setEnabled (false);
  layout_widget_bgtitle_active->addWidget (m_widget_title_bg_color_active, 0);

  connect (cb_widget_custom_style, &QCheckBox::toggled,
           m_widget_title_bg_color_active, &color_picker::setEnabled);

  QColor fg_color = settings->value (dw_title_fg_color).value<QColor> ();
  m_widget_title_fg_color = new color_picker (fg_color);
  m_widget_title_fg_color->setEnabled (false);
  layout_widget_fgtitle->addWidget (m_widget_title_fg_color, 0);

  connect (cb_widget_custom_style, &QCheckBox::toggled,
           m_widget_title_fg_color, &color_picker::setEnabled);

  QColor fg_color_active = settings->value (dw_title_fg_color_active).value<QColor> ();
  m_widget_title_fg_color_active = new color_picker (fg_color_active);
  m_widget_title_fg_color_active->setEnabled (false);
  layout_widget_fgtitle_active->addWidget (m_widget_title_fg_color_active, 0);

  connect (cb_widget_custom_style, &QCheckBox::toggled,
           m_widget_title_fg_color_active, &color_picker::setEnabled);

  sb_3d_title->setValue (settings->value (dw_title_3d.key,
                                          dw_title_3d.def).toInt ());
  cb_widget_custom_style->setChecked (settings->value (dw_title_custom_style).toBool ());

  // Native file dialogs.
  // FIXME: This preference can be deprecated / removed if all display
  //       managers, especially KDE, run those dialogs without hangs or
  //       delays from the start (bug #54607).
  cb_use_native_file_dialogs->setChecked (settings->value (global_use_native_dialogs).toBool ());

  // Cursor blinking: consider old terminal related setting if not yet set
  // FIXME: This pref. can be deprecated / removed if Qt adds support for
  //       getting the cursor blink preferences from all OS environments
  if (settings->contains (global_cursor_blinking.key))
    {
      // Preference exists, read its value
      cb_cursor_blinking->setChecked (settings->value
                                      (global_cursor_blinking.key, global_cursor_blinking.def).toBool ());
    }
  else
    {
      // Pref. does not exist, so take old terminal related pref.
      cb_cursor_blinking->setChecked (settings->value
                                      (cs_cursor_blinking.key, cs_cursor_blinking.def).toBool ());
    }

  // focus follows mouse
  cb_focus_follows_mouse->setChecked (
                                      settings->value (dw_focus_follows_mouse).toBool ());

  // prompt on exit
  cb_prompt_to_exit->setChecked (
                                 settings->value (global_prompt_to_exit.key, global_prompt_to_exit.def).toBool ());

  // Main status bar
  cb_status_bar->setChecked (
                             settings->value (global_status_bar.key, global_status_bar.def).toBool ());

  // Octave startup
  cb_restore_octave_dir->setChecked (
                                     settings->value (global_restore_ov_dir.key, global_restore_ov_dir.def).toBool ());
  le_octave_dir->setText (settings->value (global_ov_startup_dir.key,
                                           global_ov_startup_dir.def).toString ());

  connect (pb_octave_dir, &QPushButton::pressed,
           this, &settings_dialog::get_octave_dir);

  //
  // editor
  //
  useCustomFileEditor->setChecked (
                                   settings->value (global_use_custom_editor.key, global_use_custom_editor.def).toBool ());
  customFileEditor->setText (
                             settings->value (global_custom_editor.key, global_custom_editor.def).toString ());
  editor_showLineNumbers->setChecked (settings->value (ed_show_line_numbers).toBool ());
  editor_linenr_size->setValue (settings->value (ed_line_numbers_size).toInt ());

  rmgr.combo_encoding (editor_combo_encoding);

  editor_highlightCurrentLine->setChecked (settings->value (ed_highlight_current_line).toBool ());
  editor_long_line_marker->setChecked (settings->value (ed_long_line_marker).toBool ());
  bool long_line =
    settings->value (ed_long_line_marker_line).toBool ();
  editor_long_line_marker_line->setChecked (long_line);
  bool long_back =
    settings->value (ed_long_line_marker_background).toBool ();
  editor_long_line_marker_background->setChecked (long_back);
  if (! (long_line || long_back))
    editor_long_line_marker_line->setChecked (true);
  editor_long_line_column->setValue (settings->value (ed_long_line_column).toInt ());
  editor_break_checkbox->setChecked (settings->value (ed_break_lines).toBool ());
  editor_break_comments_checkbox->setChecked (settings->value (ed_break_lines_comments).toBool ());
  editor_wrap_checkbox->setChecked (settings->value (ed_wrap_lines).toBool ());
  cb_edit_status_bar->setChecked (settings->value (ed_show_edit_status_bar).toBool ());
  cb_edit_tool_bar->setChecked (settings->value (ed_show_toolbar).toBool ());
  cb_code_folding->setChecked (settings->value (ed_code_folding).toBool ());
  editor_highlight_all_occurrences->setChecked (settings->value (ed_highlight_all_occurrences).toBool ());

  editor_auto_endif->setCurrentIndex (settings->value (ed_auto_endif).toInt () );
  editor_codeCompletion->setChecked (settings->value (ed_code_completion).toBool ());
  editor_spinbox_ac_threshold->setValue (settings->value (ed_code_completion_threshold).toInt ());
  editor_checkbox_ac_keywords->setChecked (settings->value (ed_code_completion_keywords).toBool ());
  editor_checkbox_ac_builtins->setEnabled (editor_checkbox_ac_keywords->isChecked ());
  editor_checkbox_ac_functions->setEnabled (editor_checkbox_ac_keywords->isChecked ());
  editor_checkbox_ac_builtins->setChecked (settings->value (ed_code_completion_octave_builtins).toBool ());
  editor_checkbox_ac_functions->setChecked (settings->value (ed_code_completion_octave_functions).toBool ());
  editor_checkbox_ac_document->setChecked (settings->value (ed_code_completion_document).toBool ());
  editor_checkbox_ac_case->setChecked (settings->value (ed_code_completion_case).toBool ());
  editor_checkbox_ac_replace->setChecked (settings->value (ed_code_completion_replace).toBool ());
  editor_ws_checkbox->setChecked (settings->value (ed_show_white_space).toBool ());
  editor_ws_indent_checkbox->setChecked (settings->value (ed_show_white_space_indent).toBool ());
  cb_show_eol->setChecked (settings->value (ed_show_eol_chars).toBool ());
  cb_show_hscrollbar->setChecked (settings->value (ed_show_hscroll_bar).toBool ());

  for (int i = 0; i < ed_tab_position_names.length (); i++)
    editor_combox_tab_pos->insertItem (i,
                                       tr (ed_tab_position_names.at (i).toStdString ().data ()));
  editor_combox_tab_pos->setCurrentIndex
    (settings->value (ed_tab_position).toInt ());

  editor_cb_tabs_rotated->setChecked (settings->value (ed_tabs_rotated).toBool ());
  editor_sb_tabs_max_width->setValue (settings->value (ed_tabs_max_width).toInt ());

  int selected_comment_string, selected_uncomment_string;

  if (settings->contains (ed_comment_str.key))   // new version (radio buttons)
    selected_comment_string = settings->value (ed_comment_str).toInt ();
  else                                         // old version (combo box)
    selected_comment_string = settings->value (ed_comment_str_old.key,                                                 ed_comment_str.def).toInt ();

  selected_uncomment_string = settings->value (ed_uncomment_str).toInt ();

  for (int i = 0; i < ed_comment_strings_count; i++)
    {
      m_rb_comment_strings[i] = new QRadioButton ();
      m_rb_uncomment_strings[i] = new QCheckBox ();

      connect (m_rb_comment_strings[i], &QRadioButton::clicked,
               m_rb_uncomment_strings[i], &QCheckBox::setChecked);
      connect (m_rb_comment_strings[i], &QRadioButton::toggled,
               m_rb_uncomment_strings[i], &QCheckBox::setDisabled);

      m_rb_comment_strings[i]->setText (ed_comment_strings.at(i));
      m_rb_comment_strings[i]->setChecked (i == selected_comment_string);
      layout_comment_strings->addWidget (m_rb_comment_strings[i]);

      m_rb_uncomment_strings[i]->setText (ed_comment_strings.at(i));
      m_rb_uncomment_strings[i]->setAutoExclusive (false);
      m_rb_uncomment_strings[i]->setChecked ( 1 << i & selected_uncomment_string);
      layout_uncomment_strings->addWidget (m_rb_uncomment_strings[i]);
    }

  combo_eol_mode->setCurrentIndex (settings->value (ed_default_eol_mode).toInt ());
  editor_auto_ind_checkbox->setChecked (settings->value (ed_auto_indent).toBool ());
  editor_tab_ind_checkbox->setChecked (settings->value (ed_tab_indents_line).toBool ());
  editor_bs_unind_checkbox->setChecked (settings->value (ed_backspace_unindents_line).toBool ());
  editor_ind_guides_checkbox->setChecked (settings->value (ed_show_indent_guides).toBool ());
  editor_ind_width_spinbox->setValue (settings->value (ed_indent_width).toInt ());
  editor_ind_uses_tabs_checkbox->setChecked (settings->value (ed_indent_uses_tabs).toBool ());
  editor_tab_width_spinbox->setValue (settings->value (ed_tab_width).toInt ());
  editor_restoreSession->setChecked (settings->value (ed_restore_session).toBool ());
  editor_create_new_file->setChecked (settings->value (ed_create_new_file).toBool ());
  editor_reload_changed_files->setChecked (settings->value (ed_always_reload_changed_files).toBool ());
  editor_force_newline->setChecked (settings->value (ed_force_newline).toBool ());
  editor_remove_trailing_spaces->setChecked (settings->value (ed_rm_trailing_spaces).toBool ());
  editor_hiding_closes_files->setChecked (settings->value (ed_hiding_closes_files).toBool ());
  editor_show_dbg_file->setChecked (settings->value (ed_show_dbg_file).toBool ());

  // terminal
  QString default_font = settings->value (global_mono_font).toString ();
  terminal_fontName->setCurrentFont (QFont (settings->value (cs_font.key, default_font).toString ()));
  terminal_fontSize->setValue (settings->value (cs_font_size).toInt ());
  terminal_history_buffer->setValue (settings->value (cs_hist_buffer).toInt ());
  terminal_cursorUseForegroundColor->setChecked (settings->value (cs_cursor_use_fgcol).toBool ());
  terminal_focus_command->setChecked (settings->value (cs_focus_cmd).toBool ());
  terminal_print_dbg_location->setChecked (settings->value (cs_dbg_location).toBool ());

  QString cursor_type
    = settings->value (cs_cursor).toString ();

  QStringList items;
  items << QString ("0") << QString ("1") << QString ("2");
  terminal_cursorType->addItems (items);
  terminal_cursorType->setItemText (0, tr ("IBeam Cursor"));
  terminal_cursorType->setItemText (1, tr ("Block Cursor"));
  terminal_cursorType->setItemText (2, tr ("Underline Cursor"));

  for (unsigned int i = 0; i < cs_cursor_types.size (); i++)
    {
      if (cursor_type.toStdString () == cs_cursor_types[i])
        {
          terminal_cursorType->setCurrentIndex (i);
          break;
        }
    }

  read_terminal_colors (settings);

  // file browser
  connect (sync_octave_directory, &QCheckBox::toggled,
           this, &settings_dialog::set_disabled_pref_file_browser_dir);

  sync_octave_directory->setChecked (settings->value (fb_sync_octdir).toBool ());
  cb_restore_file_browser_dir->setChecked (settings->value (fb_restore_last_dir).toBool ());
  le_file_browser_dir->setText (settings->value (fb_startup_dir.key).toString ());

  connect (pb_file_browser_dir, &QPushButton::pressed,
           this, &settings_dialog::get_file_browser_dir);

  le_file_browser_extensions->setText (settings->value (fb_txt_file_ext).toString ());

  checkbox_allow_web_connect->setChecked (settings->value (nr_allow_connection).toBool ());

  // Proxy
  bool use_proxy = settings->value (global_use_proxy.key, global_use_proxy.def).toBool ();
  use_proxy_server->setChecked (use_proxy);
  // Fill combo box and activate current one
  QString proxy_type_string = settings->value (global_proxy_type.key, global_proxy_type.def).toString ();
  proxy_type->addItems (global_proxy_all_types);
  for (int i = 0; i < global_proxy_all_types.length (); i++)
    {
      if (proxy_type->itemText (i) == proxy_type_string)
        {
          proxy_type->setCurrentIndex (i);
          break;
        }
    }
  // Fill all line edits
  proxy_host_name->setText (settings->value (global_proxy_host.key, global_proxy_host.def).toString ());
  proxy_port->setText (settings->value (global_proxy_port.key, global_proxy_port.def).toString ());
  proxy_username->setText (settings->value (global_proxy_user.key, global_proxy_user.def).toString ());
  proxy_password->setText (settings->value (global_proxy_pass.key, global_proxy_pass.def).toString ());
  // Connect relevant signals for dis-/enabling some elements
  connect (proxy_type, QOverload<int>::of (&QComboBox::currentIndexChanged),
           this, &settings_dialog::proxy_items_update);
  connect (use_proxy_server, &QCheckBox::toggled,
           this, &settings_dialog::proxy_items_update);
  // Check whehter line edits have to be enabled
  proxy_items_update ();

  // Workspace
  read_workspace_colors (settings);

  // variable editor
  varedit_columnWidth->setValue (settings->value (ve_column_width).toInt ());
  varedit_rowHeight->setValue (settings->value (ve_row_height).toInt ());

  varedit_font->setCurrentFont (QFont (settings->value (ve_font_name.key,
                                                        settings->value (cs_font.key, default_font)).toString ()));
  varedit_fontSize->setValue (settings->value (ve_font_size).toInt ());
  connect (varedit_useTerminalFont, &QCheckBox::toggled,
           varedit_font, &QFontComboBox::setDisabled);
  connect (varedit_useTerminalFont, &QCheckBox::toggled,
           varedit_fontSize, &QSpinBox::setDisabled);
  varedit_useTerminalFont->setChecked (settings->value (ve_use_terminal_font).toBool ());
  varedit_font->setDisabled (varedit_useTerminalFont->isChecked ());
  varedit_fontSize->setDisabled (varedit_useTerminalFont->isChecked ());

  varedit_alternate->setChecked (settings->value (ve_alternate_rows).toBool ());

  // variable editor colors
  read_varedit_colors (settings);

  // shortcuts

  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  cb_prevent_readline_conflicts->setChecked (
                                             settings->value (sc_prevent_rl_conflicts.key,
                                                              sc_prevent_rl_conflicts.def).toBool ());
  cb_prevent_readline_conflicts_menu->setChecked (
                                                  settings->value (sc_prevent_rl_conflicts_menu.key,
                                                                   sc_prevent_rl_conflicts_menu.def).toBool ());

  // initialize the tree view with all shortcut data
  scmgr.fill_treewidget (shortcuts_treewidget);

  // connect the buttons for import/export of the shortcut sets
  connect (btn_import_shortcut_set, &QPushButton::clicked,
           this, &settings_dialog::import_shortcut_set);

  connect (btn_export_shortcut_set, &QPushButton::clicked,
           this, &settings_dialog::export_shortcut_set);

  connect (btn_default_shortcut_set, &QPushButton::clicked,
           this, &settings_dialog::default_shortcut_set);

#if defined (HAVE_QSCINTILLA)

  int mode = settings->value (ed_color_mode).toInt ();

  QCheckBox *cb_color_mode = new QCheckBox (tr (settings_color_modes.toStdString ().data ()),
                                            group_box_editor_styles);
  cb_color_mode->setToolTip (tr (settings_color_modes_tooltip.toStdString ().data ()));
  cb_color_mode->setChecked (mode > 0);
  cb_color_mode->setObjectName (ed_color_mode.key);

  QPushButton *pb_reload_default_colors = new QPushButton (tr (settings_reload_styles.toStdString ().data ()));
  pb_reload_default_colors->setToolTip (tr (settings_reload_styles_tooltip.toStdString ().data ()));

  color_picker *current_line_color = new color_picker (
                                                       settings->value (ed_highlight_current_line_color.key +
                                                                        settings_color_modes_ext[mode],
                                                                        ed_highlight_current_line_color.def).value<QColor> (), this);
  current_line_color->setObjectName (ed_highlight_current_line_color.key);
  QLabel *current_line_color_label = new QLabel(
                                                tr ("Color of highlighted current line (magenta (255,0,255) for automatic color)")
                                                );

  QHBoxLayout *color_mode = new QHBoxLayout ();
  color_mode->addWidget (cb_color_mode);
  color_mode->addItem (new QSpacerItem (5, 5, QSizePolicy::Expanding));
  color_mode->addWidget (pb_reload_default_colors);

  QHBoxLayout *current_line = new QHBoxLayout ();
  current_line->addWidget (current_line_color_label);
  current_line->addWidget (current_line_color);
  current_line->addItem (new QSpacerItem (5, 5, QSizePolicy::Expanding));

  editor_styles_layout->addLayout (color_mode);
  editor_styles_layout->addLayout (current_line);

  // update colors depending on second theme selection
  connect (cb_color_mode, &QCheckBox::stateChanged,
           this, &settings_dialog::update_editor_lexers);
  connect (pb_reload_default_colors, &QPushButton::clicked,
           [=] () { update_editor_lexers (settings_reload_default_colors_flag); });

  // finally read the lexer colors using the update slot
  update_editor_lexers ();

#endif

  // which tab is the desired one?
  show_tab (desired_tab);

  // connect button box signal
  connect (button_box, &QDialogButtonBox::clicked,
           this, &settings_dialog::button_clicked);

  // restore last geometry
  if (settings->contains (sd_geometry.key))
    restoreGeometry (settings->value (sd_geometry).toByteArray ());
  else
    setGeometry (QRect (10, 50, 1000, 600));
}

void settings_dialog::show_tab (const QString& tab)
{
  if (tab.isEmpty ())
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();
      if (settings)
        tabWidget->setCurrentIndex (settings->value (sd_last_tab).toInt ());
    }
  else
    {
      QHash <QString, QWidget *> tab_hash;
      tab_hash["editor"] = tab_editor;
      tab_hash["editor_styles"] = tab_editor;
      tabWidget->setCurrentIndex (tabWidget->indexOf (tab_hash.value (tab)));
      if (tab == "editor_styles")
        tab_editor_scroll_area->ensureWidgetVisible (group_box_editor_styles);
    }
}

void settings_dialog::get_octave_dir (void)
{
  get_dir (le_octave_dir, tr ("Set Octave Startup Directory"));
}

void settings_dialog::get_file_browser_dir (void)
{
  get_dir (le_file_browser_dir, tr ("Set File Browser Startup Directory"));
}

void settings_dialog::get_dir (QLineEdit *line_edit, const QString& title)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks;
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts |= QFileDialog::DontUseNativeDialog;

  QString dir = QFileDialog::getExistingDirectory
    (this, title, line_edit->text (), QFileDialog::Option (opts));

  line_edit->setText (dir);
}

void settings_dialog::button_clicked (QAbstractButton *button)
{
  QDialogButtonBox::ButtonRole button_role = button_box->buttonRole (button);

  if (button_role == QDialogButtonBox::ApplyRole
      || button_role == QDialogButtonBox::AcceptRole)
    {
      write_changed_settings (button_role == QDialogButtonBox::AcceptRole);
      emit apply_new_settings ();
    }

  if (button_role == QDialogButtonBox::RejectRole
      || button_role == QDialogButtonBox::AcceptRole)
    {
      // save last settings dialog's geometry and close
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();

      settings->setValue (sd_last_tab.key, tabWidget->currentIndex ());
      settings->setValue (sd_geometry.key, saveGeometry ());
      settings->sync ();

      close ();
    }
}

void settings_dialog::set_disabled_pref_file_browser_dir (bool disable)
{
  cb_restore_file_browser_dir->setDisabled (disable);

  if (! disable)
    {
      le_file_browser_dir->setDisabled (cb_restore_file_browser_dir->isChecked ());
      pb_file_browser_dir->setDisabled (cb_restore_file_browser_dir->isChecked ());
    }
  else
    {
      le_file_browser_dir->setDisabled (disable);
      pb_file_browser_dir->setDisabled (disable);
    }
}

// slot for updating enabled state of proxy settings
void settings_dialog::proxy_items_update (void)
{
  bool use_proxy = use_proxy_server->isChecked ();

  bool manual = false;
  for (int i = 0; i < global_proxy_manual_types.length (); i++)
    {
      if (proxy_type->currentIndex () == global_proxy_manual_types.at (i))
        {
          manual = true;
          break;
        }
    }

  proxy_type->setEnabled (use_proxy);
  proxy_host_name_label->setEnabled (use_proxy && manual);
  proxy_host_name->setEnabled (use_proxy && manual);
  proxy_port_label->setEnabled (use_proxy && manual);
  proxy_port->setEnabled (use_proxy && manual);
  proxy_username_label->setEnabled (use_proxy && manual);
  proxy_username->setEnabled (use_proxy && manual);
  proxy_password_label->setEnabled (use_proxy && manual);
  proxy_password->setEnabled (use_proxy && manual);
}

// slots for import/export of shortcut sets

void settings_dialog::import_shortcut_set (void)
{
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  scmgr.import_export (shortcut_manager::OSC_IMPORT);
}

void settings_dialog::export_shortcut_set (void)
{
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  scmgr.import_export (shortcut_manager::OSC_EXPORT);
}

void settings_dialog::default_shortcut_set (void)
{
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  scmgr.import_export (shortcut_manager::OSC_DEFAULT);
}

void settings_dialog::update_editor_lexers (int def)
{
#if defined (HAVE_QSCINTILLA)

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QCheckBox *cb_color_mode
    = group_box_editor_styles->findChild <QCheckBox *> (ed_color_mode.key);

  int m = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    m = 1;

  color_picker *c_picker = findChild <color_picker *> (ed_highlight_current_line_color.key);
  if (c_picker)
    {
      if (def != settings_reload_default_colors_flag)
        {
          // Get current value from settings or the default
          c_picker->set_color (settings->color_value (ed_highlight_current_line_color, m));
        }
      else
        {
          // Get the default value
          c_picker->set_color (settings->get_color_value (ed_highlight_current_line_color.def, m));
        }
    }

  // editor styles: create lexer, read settings, and
  // create or update dialog elements
  QsciLexer *lexer;

#  if defined (HAVE_LEXER_OCTAVE)
  lexer = new QsciLexerOctave ();
  update_lexer (lexer, settings, m, def);
  delete lexer;
#  elif defined (HAVE_LEXER_MATLAB)
  lexer = new QsciLexerMatlab ();
  update_lexer (lexer, settings, m, def);
  delete lexer;
#  endif

  lexer = new QsciLexerCPP ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new QsciLexerJava ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new QsciLexerPerl ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new QsciLexerBatch ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new QsciLexerDiff ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new QsciLexerBash ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

  lexer = new octave_txt_lexer ();
  update_lexer (lexer, settings, m, def);
  delete lexer;

#else

  octave_unused_parameter (def);

#endif
}

#if defined (HAVE_QSCINTILLA)

void settings_dialog::update_lexer (QsciLexer *lexer, gui_settings *settings,
                                    int mode, int def)
{
  // Get lexer settings and copy from default settings if not yet
  // available in normal settings file
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  rmgr.read_lexer_settings (lexer, settings, mode, def);

  // When reloading default styles, the style tabs do already exists.
  // Otherwise, check if they exist or not.
  QString lexer_name = lexer->language ();

  int index = -1;
  for (int i = 0; i < tabs_editor_lexers->count (); i++)
    {
      if (tabs_editor_lexers->tabText (i) == lexer_name)
        {
          index = i;
          break;
        }
    }

  if (index == -1)
    {
      // This is not an update, call get_lexer_settings for building
      // the settings tab
      get_lexer_settings (lexer, settings);
      return;
    }

  // Update the styles elements in all styles
  int styles[ed_max_lexer_styles];  // array for saving valid styles
  int max_style = rmgr.get_valid_lexer_styles (lexer, styles);
  QWidget *tab = tabs_editor_lexers->widget (index);
  int default_size = 0;
  QString default_family;

  for (int i = 0; i < max_style; i++)  // create dialog elements for all styles
    {
      QString actual_name = lexer->description (styles[i]);
      color_picker *bg_color
        = tab->findChild <color_picker *> (actual_name + "_bg_color");
      if (bg_color)
        {
          // Update
          if (styles[i] == 0)
            bg_color->set_color (lexer->defaultPaper ());
          else
            {
              if (lexer->paper (styles[i]) == lexer->defaultPaper ())
                bg_color->set_color (settings_color_no_change);
              else
                bg_color->set_color (lexer->paper (styles[i]));
            }
        }

      color_picker *color = tab->findChild <color_picker *> (actual_name + "_color");
      if (color)
        color->set_color (lexer->color (styles[i]));

      QFont font = lexer->font (styles[i]);

      QCheckBox *cb = tab->findChild <QCheckBox *> (actual_name + "_bold");
      if (cb)
        cb->setChecked (font.bold ());
      cb = tab->findChild <QCheckBox *> (actual_name + "_italic");
      if (cb)
        cb->setChecked (font.italic ());
      cb = tab->findChild <QCheckBox *> (actual_name + "_underline");
      if (cb)
        cb->setChecked (font.underline ());

      QFontComboBox *fcb = tab->findChild <QFontComboBox *> (actual_name + "_font");
      if (fcb)
        {
          if (styles[i] == 0)
            {
              default_family = font.family ();
              fcb->setEditText (default_family);
            }
          else
            {
              if (font.family () == default_family)
                fcb->setEditText (lexer->description (0));
              else
                fcb->setEditText (font.family ());
            }
        }
      QSpinBox *fs = tab->findChild <QSpinBox *> (actual_name + "_size");
      if (fs)
        {
          if (styles[i] == 0)
            {
              default_size = font.pointSize ();
              fs->setValue (default_size);
            }
          else
            fs->setValue (font.pointSize () - default_size);
        }
    }

}

void settings_dialog::get_lexer_settings (QsciLexer *lexer,
                                          gui_settings *settings)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  int styles[ed_max_lexer_styles];  // array for saving valid styles
  // (enum is not continuous)
  int max_style = rmgr.get_valid_lexer_styles (lexer, styles);
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel *> description (max_style);
  QVector<QFontComboBox *> select_font (max_style);
  QVector<QSpinBox *> font_size (max_style);
  QVector<QCheckBox *> attrib_font (3 * max_style);
  QVector<color_picker *> color (max_style);
  QVector<color_picker *> bg_color (max_style);
  int default_size = 10;
  QFont default_font = QFont ();
  int label_width;
  QColor default_color = QColor ();

  for (int i = 0; i < max_style; i++)  // create dialog elements for all styles
    {
      QString actual_name = lexer->description (styles[i]);
      QFont   actual_font = lexer->font (styles[i]);
      description[i] = new QLabel (actual_name);
      description[i]->setWordWrap (true);
      label_width = 24*description[i]->fontMetrics ().averageCharWidth ();
      description[i]->setMaximumSize (label_width, QWIDGETSIZE_MAX);
      description[i]->setMinimumSize (label_width, 1);
      select_font[i] = new QFontComboBox ();
      select_font[i]->setObjectName (actual_name + "_font");
      select_font[i]->setMaximumSize (label_width, QWIDGETSIZE_MAX);
      select_font[i]->setMinimumSize (label_width, 1);
      select_font[i]->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
      font_size[i] = new QSpinBox ();
      font_size[i]->setObjectName (actual_name + "_size");
      if (styles[i] == 0) // the default
        {
          select_font[i]->setCurrentFont (actual_font);
          default_font = actual_font;
          font_size[i]->setRange (6, 24);
          default_size = actual_font.pointSize ();
          font_size[i]->setValue (default_size);
          default_color = lexer->defaultPaper ();
          bg_color[i] = new color_picker (default_color);
        }
      else   // other styles
        {
          select_font[i]->setCurrentFont (actual_font);
          if (actual_font.family () == default_font.family ())
            select_font[i]->setEditText (lexer->description (0));
          font_size[i]->setRange (-4, 4);
          font_size[i]->setValue (actual_font.pointSize ()-default_size);
          font_size[i]->setToolTip (QObject::tr ("Difference to the default size"));
          if (lexer->paper (styles[i]) == default_color)
            bg_color[i] = new color_picker (settings_color_no_change);
          else
            bg_color[i] = new color_picker (lexer->paper (styles[i]));
          bg_color[i]->setToolTip
            (QObject::tr ("Background color, magenta (255, 0, 255) means default"));
        }
      attrib_font[0+3*i] = new QCheckBox (QObject::tr ("b", "short form for bold"));
      attrib_font[1+3*i] = new QCheckBox (QObject::tr ("i", "short form for italic"));
      attrib_font[2+3*i] = new QCheckBox (QObject::tr ("u", "short form for underlined"));
      attrib_font[0+3*i]->setChecked (actual_font.bold ());
      attrib_font[0+3*i]->setObjectName (actual_name + "_bold");
      attrib_font[1+3*i]->setChecked (actual_font.italic ());
      attrib_font[1+3*i]->setObjectName (actual_name + "_italic");
      attrib_font[2+3*i]->setChecked (actual_font.underline ());
      attrib_font[2+3*i]->setObjectName (actual_name + "_underline");
      color[i] = new color_picker (lexer->color (styles[i]));
      color[i]->setObjectName (actual_name + "_color");
      bg_color[i]->setObjectName (actual_name + "_bg_color");
      int column = 1;
      style_grid->addWidget (description[i], i, column++);
      style_grid->addWidget (select_font[i], i, column++);
      style_grid->addWidget (font_size[i], i, column++);
      style_grid->addWidget (attrib_font[0+3*i], i, column++);
      style_grid->addWidget (attrib_font[1+3*i], i, column++);
      style_grid->addWidget (attrib_font[2+3*i], i, column++);
      style_grid->addWidget (color[i], i, column++);
      style_grid->addWidget (bg_color[i], i, column++);
    }

  // place grid with elements into the tab
  QScrollArea *scroll_area = new QScrollArea ();
  QWidget *scroll_area_contents = new QWidget ();
  scroll_area_contents->setObjectName (QString (lexer->language ()) + "_styles");
  scroll_area_contents->setLayout (style_grid);
  scroll_area->setWidget (scroll_area_contents);
  tabs_editor_lexers->addTab (scroll_area, lexer->language ());

  tabs_editor_lexers->setCurrentIndex (settings->value (sd_last_editor_styles_tab).toInt ());
}

void settings_dialog::write_lexer_settings (QsciLexer *lexer,
                                            gui_settings *settings)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  QCheckBox *cb_color_mode
    = group_box_editor_styles->findChild <QCheckBox *> (ed_color_mode.key);
  int mode = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    mode = 1;

  settings->setValue (ed_color_mode.key, mode);

  QWidget *tab = tabs_editor_lexers->
    findChild <QWidget *> (QString (lexer->language ()) + "_styles");
  int styles[ed_max_lexer_styles];  // array for saving valid styles
  // (enum is not continuous)
  int max_style = rmgr.get_valid_lexer_styles (lexer, styles);
  QFontComboBox *select_font;
  QSpinBox *font_size;
  QCheckBox *attrib_font[3];
  color_picker *color;
  color_picker *bg_color;
  int default_size = 10;

  color = findChild <color_picker *> (ed_highlight_current_line_color.key);
  if (color)
    settings->setValue (ed_highlight_current_line_color.key
                        + settings_color_modes_ext[mode], color->color ());

  QString default_font_name
    = settings->value (global_mono_font).toString ();
  QFont default_font = QFont (default_font_name, 10, -1, 0);
  QColor default_color = QColor ();

  for (int i = 0; i < max_style; i++)  // get dialog elements and their contents
    {
      QString actual_name = lexer->description (styles[i]);
      select_font = tab->findChild <QFontComboBox *> (actual_name + "_font");
      font_size = tab->findChild <QSpinBox *> (actual_name + "_size");
      attrib_font[0] = tab->findChild <QCheckBox *> (actual_name + "_bold");
      attrib_font[1] = tab->findChild <QCheckBox *> (actual_name + "_italic");
      attrib_font[2] = tab->findChild <QCheckBox *> (actual_name + "_underline");
      color = tab->findChild <color_picker *> (actual_name + "_color");
      bg_color = tab->findChild <color_picker *> (actual_name + "_bg_color");
      QFont new_font = default_font;
      if (select_font)
        {
          new_font = select_font->currentFont ();
          if (styles[i] == 0)
            default_font = new_font;
          else if (select_font->currentText () == lexer->description (0))
            new_font = default_font;
        }
      if (font_size)
        {
          if (styles[i] == 0)
            {
              default_size = font_size->value ();
              new_font.setPointSize (font_size->value ());
            }
          else
            new_font.setPointSize (font_size->value ()+default_size);
        }
      if (attrib_font[0])
        new_font.setBold (attrib_font[0]->isChecked ());
      if (attrib_font[1])
        new_font.setItalic (attrib_font[1]->isChecked ());
      if (attrib_font[2])
        new_font.setUnderline (attrib_font[2]->isChecked ());
      lexer->setFont (new_font, styles[i]);
      if (styles[i] == 0)
        lexer->setDefaultFont (new_font);
      if (color)
        lexer->setColor (color->color (), styles[i]);
      if (bg_color)
        {
          if (styles[i] == 0)
            {
              default_color = bg_color->color ();
              lexer->setPaper (default_color, styles[i]);
              lexer->setDefaultPaper (default_color);
            }
          else
            {
              if (bg_color->color () == settings_color_no_change)
                lexer->setPaper (default_color, styles[i]);
              else
                lexer->setPaper (bg_color->color (), styles[i]);
            }
        }
    }

  const std::string group =
    QString ("Scintilla" + settings_color_modes_ext[mode]).toStdString ();

  lexer->writeSettings (*settings, group.c_str ());

  settings->setValue (sd_last_editor_styles_tab.key,
                      tabs_editor_lexers->currentIndex ());
  settings->sync ();
}

#endif

void settings_dialog::write_changed_settings (bool closing)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  // the icon set
  QString widget_icon_set = "NONE";
  if (general_icon_letter->isChecked ())
    widget_icon_set = "LETTER";
  else if (general_icon_graphic->isChecked ())
    widget_icon_set = "GRAPHIC";
  settings->setValue (dw_icon_set.key, widget_icon_set);

  // language
  QString language = comboBox_language->currentText ();
  if (language == tr ("System setting"))
    language = global_language.def.toString ();
  settings->setValue (global_language.key, language);

  // style
  QString selected_style = combo_styles->currentText ();
  if (selected_style == global_style.def.toString ())
    selected_style = global_style.def.toString ();
  settings->setValue (global_style.key, selected_style);

  // dock widget title bar
  settings->setValue (dw_title_custom_style.key, cb_widget_custom_style->isChecked ());
  settings->setValue (dw_title_3d.key, sb_3d_title->value ());
  settings->setValue (dw_title_bg_color.key, m_widget_title_bg_color->color ());
  settings->setValue (dw_title_bg_color_active.key, m_widget_title_bg_color_active->color ());
  settings->setValue (dw_title_fg_color.key, m_widget_title_fg_color->color ());
  settings->setValue (dw_title_fg_color_active.key, m_widget_title_fg_color_active->color ());

  // icon size and theme
  int icon_size = icon_size_large->isChecked () - icon_size_small->isChecked ();
  settings->setValue (global_icon_size.key, icon_size);
  settings->setValue (global_icon_theme_index.key, combo_box_icon_theme->currentIndex ());

  // native file dialogs
  settings->setValue (global_use_native_dialogs.key, cb_use_native_file_dialogs->isChecked ());

  // cursor blinking
  settings->setValue (global_cursor_blinking.key, cb_cursor_blinking->isChecked ());

  // focus follows mouse
  settings->setValue (dw_focus_follows_mouse.key, cb_focus_follows_mouse->isChecked ());

  // promp to exit
  settings->setValue (global_prompt_to_exit.key, cb_prompt_to_exit->isChecked ());

  // status bar
  settings->setValue (global_status_bar.key, cb_status_bar->isChecked ());

  // Octave startup
  settings->setValue (global_restore_ov_dir.key, cb_restore_octave_dir->isChecked ());
  settings->setValue (global_ov_startup_dir.key, le_octave_dir->text ());

  //editor
  settings->setValue (global_use_custom_editor.key, useCustomFileEditor->isChecked ());
  settings->setValue (global_custom_editor.key, customFileEditor->text ());
  settings->setValue (ed_show_line_numbers.key, editor_showLineNumbers->isChecked ());
  settings->setValue (ed_line_numbers_size.key, editor_linenr_size->value ());
  settings->setValue (ed_highlight_current_line.key, editor_highlightCurrentLine->isChecked ());
  settings->setValue (ed_long_line_marker.key, editor_long_line_marker->isChecked ());
  settings->setValue (ed_long_line_marker_line.key, editor_long_line_marker_line->isChecked ());
  settings->setValue (ed_long_line_marker_background.key, editor_long_line_marker_background->isChecked ());
  settings->setValue (ed_long_line_column.key, editor_long_line_column->value ());
  settings->setValue (ed_break_lines.key, editor_break_checkbox->isChecked ());
  settings->setValue (ed_break_lines_comments.key, editor_break_comments_checkbox->isChecked ());
  settings->setValue (ed_wrap_lines.key, editor_wrap_checkbox->isChecked ());
  settings->setValue (ed_code_folding.key, cb_code_folding->isChecked ());
  settings->setValue (ed_show_edit_status_bar.key, cb_edit_status_bar->isChecked ());
  settings->setValue (ed_show_toolbar.key, cb_edit_tool_bar->isChecked ());
  settings->setValue (ed_highlight_all_occurrences.key, editor_highlight_all_occurrences->isChecked ());
  settings->setValue (ed_code_completion.key, editor_codeCompletion->isChecked ());
  settings->setValue (ed_code_completion_threshold.key, editor_spinbox_ac_threshold->value ());
  settings->setValue (ed_code_completion_keywords.key, editor_checkbox_ac_keywords->isChecked ());
  settings->setValue (ed_code_completion_octave_builtins.key, editor_checkbox_ac_builtins->isChecked ());
  settings->setValue (ed_code_completion_octave_functions.key, editor_checkbox_ac_functions->isChecked ());
  settings->setValue (ed_code_completion_document.key, editor_checkbox_ac_document->isChecked ());
  settings->setValue (ed_code_completion_case.key, editor_checkbox_ac_case->isChecked ());
  settings->setValue (ed_code_completion_replace.key, editor_checkbox_ac_replace->isChecked ());
  settings->setValue (ed_auto_endif.key, editor_auto_endif->currentIndex ());
  settings->setValue (ed_show_white_space.key, editor_ws_checkbox->isChecked ());
  settings->setValue (ed_show_white_space_indent.key, editor_ws_indent_checkbox->isChecked ());
  settings->setValue (ed_show_eol_chars.key, cb_show_eol->isChecked ());
  settings->setValue (ed_show_hscroll_bar.key, cb_show_hscrollbar->isChecked ());
  settings->setValue (ed_default_eol_mode.key, combo_eol_mode->currentIndex ());

  settings->setValue (ed_tab_position.key, editor_combox_tab_pos->currentIndex ());
  settings->setValue (ed_tabs_rotated.key, editor_cb_tabs_rotated->isChecked ());
  settings->setValue (ed_tabs_max_width.key, editor_sb_tabs_max_width->value ());

  // Comment strings
  int rb_uncomment = 0;
  for (int i = 0; i < ed_comment_strings_count; i++)
    {
      if (m_rb_comment_strings[i]->isChecked ())
        {
          settings->setValue (ed_comment_str.key, i);
          if (i < 3)
            settings->setValue (ed_comment_str_old.key, i);
          else
            settings->setValue (ed_comment_str_old.key, ed_comment_str.def);
        }
      if (m_rb_uncomment_strings[i]->isChecked ())
        rb_uncomment = rb_uncomment + (1 << i);
    }
  settings->setValue (ed_uncomment_str.key, rb_uncomment);

  settings->setValue (ed_default_enc.key, editor_combo_encoding->currentText ());
  settings->setValue (ed_auto_indent.key, editor_auto_ind_checkbox->isChecked ());
  settings->setValue (ed_tab_indents_line.key, editor_tab_ind_checkbox->isChecked ());
  settings->setValue (ed_backspace_unindents_line.key, editor_bs_unind_checkbox->isChecked ());
  settings->setValue (ed_show_indent_guides.key, editor_ind_guides_checkbox->isChecked ());
  settings->setValue (ed_indent_width.key, editor_ind_width_spinbox->value ());
  settings->setValue (ed_indent_uses_tabs.key, editor_ind_uses_tabs_checkbox->isChecked ());
  settings->setValue (ed_tab_width.key, editor_tab_width_spinbox->value ());
  settings->setValue (ed_restore_session.key, editor_restoreSession->isChecked ());
  settings->setValue (ed_create_new_file.key, editor_create_new_file->isChecked ());
  settings->setValue (ed_hiding_closes_files.key, editor_hiding_closes_files->isChecked ());
  settings->setValue (ed_always_reload_changed_files.key, editor_reload_changed_files->isChecked ());
  settings->setValue (ed_force_newline.key, editor_force_newline->isChecked ());
  settings->setValue (ed_rm_trailing_spaces.key, editor_remove_trailing_spaces->isChecked ());
  settings->setValue (ed_show_dbg_file.key, editor_show_dbg_file->isChecked ());

  // file browser
  settings->setValue (fb_sync_octdir.key, sync_octave_directory->isChecked ());
  settings->setValue (fb_restore_last_dir.key, cb_restore_file_browser_dir->isChecked ());
  settings->setValue (fb_startup_dir.key, le_file_browser_dir->text ());
  settings->setValue (fb_txt_file_ext.key, le_file_browser_extensions->text ());

  // network
  settings->setValue (nr_allow_connection.key, checkbox_allow_web_connect->isChecked ());
  settings->setValue (global_use_proxy.key, use_proxy_server->isChecked ());
  settings->setValue (global_proxy_type.key, proxy_type->currentText ());
  settings->setValue (global_proxy_host.key, proxy_host_name->text ());
  settings->setValue (global_proxy_port.key, proxy_port->text ());
  settings->setValue (global_proxy_user.key, proxy_username->text ());
  settings->setValue (global_proxy_pass.key, proxy_password->text ());

  // command window
  settings->setValue (cs_font_size.key, terminal_fontSize->value ());
  settings->setValue (cs_font.key, terminal_fontName->currentFont ().family ());
  settings->setValue (cs_cursor_use_fgcol.key, terminal_cursorUseForegroundColor->isChecked ());
  settings->setValue (cs_focus_cmd.key, terminal_focus_command->isChecked ());
  settings->setValue (cs_dbg_location.key, terminal_print_dbg_location->isChecked ());
  settings->setValue (cs_hist_buffer.key, terminal_history_buffer->value ());
  write_terminal_colors (settings);

  // the cursor
  QString cursor_type;
  unsigned int cursor_int = terminal_cursorType->currentIndex ();
  if ((cursor_int > 0) && (cursor_int < cs_cursor_types.size ()))
    cursor_type = QString (cs_cursor_types[cursor_int].data ());
  else
    cursor_type = cs_cursor.def.toString ();

  settings->setValue (cs_cursor.key, cursor_type);

#if defined (HAVE_QSCINTILLA)
  // editor styles: create lexer, get dialog contents, and write settings
  QsciLexer *lexer;

#if defined (HAVE_LEXER_OCTAVE)

  lexer = new QsciLexerOctave ();
  write_lexer_settings (lexer, settings);
  delete lexer;

#elif defined (HAVE_LEXER_MATLAB)

  lexer = new QsciLexerMatlab ();
  write_lexer_settings (lexer, settings);
  delete lexer;

#endif

  lexer = new QsciLexerCPP ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new QsciLexerJava ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new QsciLexerPerl ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new QsciLexerBatch ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new QsciLexerDiff ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new QsciLexerBash ();
  write_lexer_settings (lexer, settings);
  delete lexer;

  lexer = new octave_txt_lexer ();
  write_lexer_settings (lexer, settings);
  delete lexer;

#endif

  // Workspace
  write_workspace_colors (settings);

  // Variable editor
  settings->setValue (ve_column_width.key, varedit_columnWidth->value ());
  settings->setValue (ve_row_height.key, varedit_rowHeight->value ());
  settings->setValue (ve_use_terminal_font.key, varedit_useTerminalFont->isChecked ());
  settings->setValue (ve_alternate_rows.key, varedit_alternate->isChecked ());
  settings->setValue (ve_font_name.key, varedit_font->currentFont ().family ());
  settings->setValue (ve_font_size.key, varedit_fontSize->value ());
  write_varedit_colors (settings);

  // shortcuts

  settings->setValue (sc_prevent_rl_conflicts.key, cb_prevent_readline_conflicts->isChecked ());
  settings->setValue (sc_prevent_rl_conflicts_menu.key, cb_prevent_readline_conflicts_menu->isChecked ());
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();
  scmgr.write_shortcuts (settings, closing);

  settings->sync ();
}

void settings_dialog::read_workspace_colors (gui_settings *settings)
{
  // Construct the grid with all color related settings
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel *> description (ws_colors_count);
  QVector<color_picker *> color (ws_colors_count);

  int column = 0;
  const int color_columns = 3;  // place colors in so many columns
  int row = 0;
  int mode = settings->value (ws_color_mode).toInt ();

  m_ws_enable_colors = new QCheckBox (tr ("Enable attribute colors"));
  style_grid->addWidget (m_ws_enable_colors, row++, column, 1, 4);

  m_ws_hide_tool_tips = new QCheckBox (tr ("Hide tools tips"));
  style_grid->addWidget (m_ws_hide_tool_tips, row++, column, 1, 4);
  connect (m_ws_enable_colors, &QCheckBox::toggled,
           m_ws_hide_tool_tips, &QCheckBox::setEnabled);
  m_ws_hide_tool_tips->setChecked
    (settings->value (ws_hide_tool_tips).toBool ());

  QCheckBox *cb_color_mode = new QCheckBox (tr (settings_color_modes.toStdString ().data ()));
  cb_color_mode->setToolTip (tr (settings_color_modes_tooltip.toStdString ().data ()));
  cb_color_mode->setChecked (mode == 1);
  cb_color_mode->setObjectName (ws_color_mode.key);
  connect (m_ws_enable_colors, &QCheckBox::toggled,
           cb_color_mode, &QCheckBox::setEnabled);
  style_grid->addWidget (cb_color_mode, row, column);

  QPushButton *pb_reload_default_colors = new QPushButton (tr (settings_reload_colors.toStdString ().data ()));
  pb_reload_default_colors->setToolTip (tr (settings_reload_colors_tooltip.toStdString ().data ()));
  connect (m_ws_enable_colors, &QCheckBox::toggled,
           pb_reload_default_colors, &QPushButton::setEnabled);
  style_grid->addWidget (pb_reload_default_colors, row+1, column++);

  bool colors_enabled = settings->value (ws_enable_colors).toBool ();

  for (int i = 0; i < ws_colors_count; i++)
    {
      description[i] = new QLabel ("    "
                                   + tr (ws_color_names.at (i).toStdString ().data ()));
      description[i]->setAlignment (Qt::AlignRight);
      description[i]->setEnabled (colors_enabled);
      connect (m_ws_enable_colors, &QCheckBox::toggled,
               description[i], &QLabel::setEnabled);

      QColor setting_color = settings->color_value (ws_colors[i], mode);
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName (ws_colors[i].key);
      color[i]->setMinimumSize (30, 10);
      color[i]->setEnabled (colors_enabled);
      connect (m_ws_enable_colors, &QCheckBox::toggled,
               color[i], &color_picker::setEnabled);

      style_grid->addWidget (description[i], row, 3*column);
      style_grid->addWidget (color[i], row, 3*column+1);
      if (++column > color_columns)
        {
          style_grid->setColumnStretch (4*column, 10);
          row++;
          column = 1;
        }
    }

  // Load enable settings at the end for having signals already connected
  m_ws_enable_colors->setChecked (colors_enabled);
  m_ws_hide_tool_tips->setEnabled (colors_enabled);
  cb_color_mode->setEnabled (colors_enabled);
  pb_reload_default_colors->setEnabled (colors_enabled);

  // place grid with elements into the tab
  workspace_colors_box->setLayout (style_grid);

  // update colors depending on second theme selection or reloading
  // the dfault values
  connect (cb_color_mode, &QCheckBox::stateChanged,
           this, &settings_dialog::update_workspace_colors);
  connect (pb_reload_default_colors, &QPushButton::clicked,
           [=] () { update_workspace_colors (settings_reload_default_colors_flag); });
}

void settings_dialog::update_workspace_colors (int def)
{
  QCheckBox *cb_color_mode
    = workspace_colors_box->findChild <QCheckBox *> (ws_color_mode.key);

  int m = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    m = 1;

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  color_picker *c_picker;

  for (unsigned int i = 0; i < ws_colors_count; i++)
    {
      c_picker = workspace_colors_box->findChild <color_picker *> (ws_colors[i].key);
      if (c_picker)
        {
          if (def != settings_reload_default_colors_flag)
            {
              // Get current value from settings or the default
              c_picker->set_color (settings->color_value (ws_colors[i], m));
            }
          else
            {
              // Get the default value
              c_picker->set_color (settings->get_color_value (ws_colors[i].def, m));
            }
        }
    }
}

void settings_dialog::write_workspace_colors (gui_settings *settings)
{
  settings->setValue (ws_enable_colors.key, m_ws_enable_colors->isChecked ());
  settings->setValue (ws_hide_tool_tips.key, m_ws_hide_tool_tips->isChecked ());

  QCheckBox *cb_color_mode
    = workspace_colors_box->findChild <QCheckBox *> (ws_color_mode.key);

  int mode = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    mode = 1;

  color_picker *color;

  for (int i = 0; i < ws_colors_count; i++)
    {
      color = workspace_colors_box->findChild <color_picker *> (ws_colors[i].key);
      if (color)
        settings->set_color_value (ws_colors[i], color->color (), mode);
    }

  settings->setValue (ws_color_mode.key, mode);

  settings->sync ();
}

void settings_dialog::read_terminal_colors (gui_settings *settings)
{
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel *> description (cs_colors_count);
  QVector<color_picker *> color (cs_colors_count);

  int mode = settings->value (cs_color_mode).toInt ();

  QCheckBox *cb_color_mode = new QCheckBox (tr (settings_color_modes.toStdString ().data ()));
  cb_color_mode->setToolTip (tr (settings_color_modes_tooltip.toStdString ().data ()));
  cb_color_mode->setChecked (mode == 1);
  cb_color_mode->setObjectName (cs_color_mode.key);
  style_grid->addWidget (cb_color_mode, 0, 0);

  QPushButton *pb_reload_default_colors = new QPushButton (tr (settings_reload_colors.toStdString ().data ()));
  pb_reload_default_colors->setToolTip (tr (settings_reload_colors_tooltip.toStdString ().data ()));
  style_grid->addWidget (pb_reload_default_colors, 1, 0);

  int column = 1;               // column 0 is for the color mode checkbox
  const int color_columns = 2;  // place colors in so many columns
  int row = 0;
  for (unsigned int i = 0; i < cs_colors_count; i++)
    {
      description[i] = new QLabel ("    "
                                   + tr (cs_color_names.at (i).toStdString ().data ()));
      description[i]->setAlignment (Qt::AlignRight);
      QColor setting_color = settings->color_value (cs_colors[i], mode);
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName (cs_colors[i].key);
      color[i]->setMinimumSize (30, 10);
      style_grid->addWidget (description[i], row, 2*column);
      style_grid->addWidget (color[i], row, 2*column+1);
      if (++column > color_columns)
        {
          style_grid->setColumnStretch (3*column, 10);
          row++;
          column = 1;
        }
    }

  // place grid with elements into the tab
  terminal_colors_box->setLayout (style_grid);

  // update colors depending on second theme selection
  connect (cb_color_mode, &QCheckBox::stateChanged,
           this, &settings_dialog::update_terminal_colors);
  connect (pb_reload_default_colors, &QPushButton::clicked,
           [=] () { update_terminal_colors (settings_reload_default_colors_flag); });
}

void settings_dialog::update_terminal_colors (int def)
{
  QCheckBox *cb_color_mode
    = terminal_colors_box->findChild <QCheckBox *> (cs_color_mode.key);

  int m = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    m = 1;

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  color_picker *c_picker;

  for (unsigned int i = 0; i < cs_colors_count; i++)
    {
      c_picker = terminal_colors_box->findChild <color_picker *> (cs_colors[i].key);
      if (c_picker)
        {
          if (def != settings_reload_default_colors_flag)
            {
              // Get current value from settings or the default
              c_picker->set_color (settings->color_value (cs_colors[i], m));
            }
          else
            {
              // Get the default value
              c_picker->set_color (settings->get_color_value (cs_colors[i].def, m));
            }
        }
    }
}

void settings_dialog::write_terminal_colors (gui_settings *settings)
{
  QCheckBox *cb_color_mode
    = terminal_colors_box->findChild <QCheckBox *> (cs_color_mode.key);

  int mode = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    mode = 1;

  color_picker *color;

  for (int i = 0; i < cs_color_names.size (); i++)
    {
      color = terminal_colors_box->findChild <color_picker *> (cs_colors[i].key);
      if (color)
        settings->set_color_value (cs_colors[i], color->color (), mode);
    }

  settings->setValue (cs_color_mode.key, mode);

  settings->sync ();
}

void settings_dialog::read_varedit_colors (gui_settings *settings)
{
  QGridLayout *style_grid = new QGridLayout ();
  QVector<QLabel *> description (ve_colors_count);
  QVector<color_picker *> color (ve_colors_count);

  int mode = settings->value (ve_color_mode).toInt ();

  QCheckBox *cb_color_mode = new QCheckBox (tr (settings_color_modes.toStdString ().data ()));
  cb_color_mode->setToolTip (tr (settings_color_modes_tooltip.toStdString ().data ()));
  cb_color_mode->setChecked (mode == 1);
  cb_color_mode->setObjectName (ve_color_mode.key);
  style_grid->addWidget (cb_color_mode, 0, 0);

  QPushButton *pb_reload_default_colors = new QPushButton (tr (settings_reload_colors.toStdString ().data ()));
  pb_reload_default_colors->setToolTip (tr (settings_reload_colors_tooltip.toStdString ().data ()));
  style_grid->addWidget (pb_reload_default_colors, 1, 0);

  int column = 1;
  int color_columns = 2;
  int row = 0;
  for (int i = 0; i < ve_colors_count; i++)
    {
      description[i] = new QLabel ("    "
                                   + tr (ve_color_names.at (i).toStdString ().data ()));
      description[i]->setAlignment (Qt::AlignRight);

      QColor setting_color = settings->color_value (ve_colors[i], mode);
      color[i] = new color_picker (setting_color);
      color[i]->setObjectName (ve_colors[i].key);
      color[i]->setMinimumSize (30, 10);
      style_grid->addWidget (description[i], row, 2*column);
      style_grid->addWidget (color[i], row, 2*column+1);
      if (++column > color_columns)
        {
          style_grid->setColumnStretch (3*column, 10);
          row++;
          column = 1;
        }
    }

  // place grid with elements into the tab
  varedit_colors_box->setLayout (style_grid);

  // update colors depending on second theme selection
  connect (cb_color_mode, &QCheckBox::stateChanged,
           this, &settings_dialog::update_varedit_colors);
  connect (pb_reload_default_colors, &QPushButton::clicked,
           [=] () { update_varedit_colors (settings_reload_default_colors_flag); });
}

void settings_dialog::update_varedit_colors (int def)
{
  QCheckBox *cb_color_mode
    = varedit_colors_box->findChild <QCheckBox *> (ve_color_mode.key);

  int m = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    m = 1;

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  color_picker *c_picker;

  for (unsigned int i = 0; i < ve_colors_count; i++)
    {
      c_picker = varedit_colors_box->findChild <color_picker *> (ve_colors[i].key);
      if (c_picker)
        {
          if (def != settings_reload_default_colors_flag)
            {
              // Get current value from settings or the default
              c_picker->set_color (settings->color_value (ve_colors[i], m));
            }
          else
            {
              // Get the default value
              c_picker->set_color (settings->get_color_value (ve_colors[i].def, m));
            }
        }
    }
}

void settings_dialog::write_varedit_colors (gui_settings *settings)
{
  QCheckBox *cb_color_mode
    = varedit_colors_box->findChild <QCheckBox *> (ve_color_mode.key);

  int mode = 0;
  if (cb_color_mode && cb_color_mode->isChecked ())
    mode = 1;

  color_picker *color;

  for (int i = 0; i < ve_colors_count; i++)
    {
      color = varedit_colors_box->findChild <color_picker *> (ve_colors[i].key);
      if (color)
        settings->set_color_value (ve_colors[i], color->color (), mode);
    }

  settings->setValue (ve_color_mode.key, mode);

  settings->sync ();
}

OCTAVE_END_NAMESPACE(octave)
