////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2014-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QAction>
#include <QApplication>
#include <QCheckBox>
#include <QDebug>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QGridLayout>
#include <QHeaderView>
#include <QKeySequence>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtCore>

#include "octave-qobject.h"
#include "shortcut-manager.h"
#include "gui-preferences-global.h"
#include "gui-preferences-sc.h"
#include "error.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// enter_shortcut:
// class derived from QLineEdit for directly entering key sequences which

enter_shortcut::enter_shortcut (QWidget *p) : QLineEdit (p)
{
  m_direct_shortcut = true;      // the shortcut is directly entered
  m_shift_modifier = false;      // the shift modifier is not added
}

// new keyPressEvent
void enter_shortcut::keyPressEvent (QKeyEvent *e)
{
  if (! m_direct_shortcut)
    {
      QLineEdit::keyPressEvent (e);
      return;
    }

  if (e->type () == QEvent::KeyPress)
    {
      int key = e->key ();

      if (key == Qt::Key_unknown || key == 0)
        return;

      Qt::KeyboardModifiers modifiers = QGuiApplication::keyboardModifiers (); //e->modifiers ();

      if (m_shift_modifier || (modifiers & Qt::ShiftModifier))
        key += Qt::SHIFT;
      if (modifiers & Qt::ControlModifier)
        key += Qt::CTRL;
      if (modifiers & Qt::AltModifier)
        key += Qt::ALT;
      if (modifiers & Qt::MetaModifier)
        key += Qt::META;

      setText (QKeySequence (key).toString ());
    }
}

// slot for checkbox whether the shortcut is directly entered or not
void enter_shortcut::handle_direct_shortcut (int state)
{
  if (state)
    m_direct_shortcut = true;  // the shortcut is directly entered
  else
    m_direct_shortcut = false; // the shortcut has to be written as text
}

// slot for checkbox whether the shift modifier should be added
void enter_shortcut::handle_shift_modifier (int state)
{
  if (state)
    m_shift_modifier = true;  // the shortcut is directly entered
  else
    m_shift_modifier = false; // the shortcut has to be written as text
}

shortcut_manager::shortcut_manager (base_qobject& oct_qobj)
  : m_octave_qobj (oct_qobj)
{
  setObjectName ("Shortcut_Manager");

  // Mac: don't let Qt interpret CMD key ("Meta" in Qt terminology) as Ctrl
#if defined (Q_OS_MAC)
  QCoreApplication::setAttribute (Qt::AA_MacDontSwapCtrlAndMeta, true);
#endif
}

void shortcut_manager::init_data (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  settings->setValue (sc_main_ctrld.key, false); // reset use fo ctrl-d

  // actions not related to specific menus or widgets

  // dock widgets
  init (tr ("Undock/Dock Widget"), sc_dock_widget_dock);
  init (tr ("Close Widget"), sc_dock_widget_close);

  // actions of the main window

  // file
  init (tr ("New File"), sc_main_file_new_file);
  init (tr ("New Function"), sc_main_file_new_function);
  init (tr ("New Figure"), sc_main_file_new_figure);
  init (tr ("Open File"), sc_main_file_open_file);
  init (tr ("Load Workspace"), sc_main_file_load_workspace);
  init (tr ("Save Workspace As"), sc_main_file_save_workspace);
  init (tr ("Exit Octave"), sc_main_file_exit);

  // edit
  init (tr ("Copy"), sc_main_edit_copy);
  init (tr ("Paste"), sc_main_edit_paste);
  init (tr ("Undo"), sc_main_edit_undo);
  init (tr ("Select All"), sc_main_edit_select_all);
  init (tr ("Clear Clipboard"), sc_main_edit_clear_clipboard);
  init (tr ("Find in Files"), sc_main_edit_find_in_files);
  init (tr ("Clear Command Window"), sc_main_edit_clear_command_window);
  init (tr ("Clear Command History"), sc_main_edit_clear_history);
  init (tr ("Clear Workspace"), sc_main_edit_clear_workspace);
  init (tr ("Set Path"), sc_main_edit_set_path);
  init (tr ("Preferences"), sc_main_edit_preferences);

  // debug
  init (tr ("Step"), sc_main_debug_step_over);
  init (tr ("Step Into"), sc_main_debug_step_into);
  init (tr ("Step Out"), sc_main_debug_step_out);
  init (tr ("Continue"), sc_main_debug_continue);
  init (tr ("Quit Debug Mode"), sc_main_debug_quit);

  // tools
  init (tr ("Start/Stop Profiler Session"), sc_main_tools_start_profiler);
  init (tr ("Resume Profiler Session"), sc_main_tools_resume_profiler);
  init (tr ("Show Profile Data"), sc_main_tools_show_profiler);

  // window
  init (tr ("Show Command Window"), sc_main_window_show_command);
  init (tr ("Show Command History"), sc_main_window_show_history);
  init (tr ("Show File Browser"), sc_main_window_show_file_browser);
  init (tr ("Show Workspace"), sc_main_window_show_workspace);
  init (tr ("Show Editor"), sc_main_window_show_editor);
  init (tr ("Show Documentation"), sc_main_window_show_doc);
  init (tr ("Show Variable Editor"), sc_main_window_show_variable_editor);
  init (tr ("Command Window"), sc_main_window_command);
  init (tr ("Command History"), sc_main_window_history);
  init (tr ("File Browser"), sc_main_window_file_browser);
  init (tr ("Workspace"), sc_main_window_workspace);
  init (tr ("Editor"), sc_main_window_editor);
  init (tr ("Documentation"), sc_main_window_doc);
  init (tr ("Variable Editor"), sc_main_window_variable_editor);
  init (tr ("Previous Widget"), sc_main_window_previous_dock);
  init (tr ("Reset Default Window Layout"), sc_main_window_reset);

  // help
  init (tr ("Show On-disk Documentation"), sc_main_help_ondisk_doc);
  init (tr ("Show Online Documentation"), sc_main_help_online_doc);
  init (tr ("Report Bug"), sc_main_help_report_bug);
  init (tr ("Octave Packages"), sc_main_help_packages);
  init (tr ("Contribute to Octave"), sc_main_help_contribute);
  init (tr ("Octave Developer Resources"), sc_main_help_developer);
  init (tr ("About Octave"), sc_main_help_about);

  // news
  init (tr ("Release Notes"), sc_main_news_release_notes);
  init (tr ("Community News"), sc_main_news_community_news);

  // Tab handling
  // The following shortcuts are moved into a separate tab.  The key names
  // are not changed, to preserve compatibility with older versions.
  init (tr ("Close Tab"), sc_edit_file_close);
  init (tr ("Close All Tabs"), sc_edit_file_close_all);
  init (tr ("Close Other Tabs"), sc_edit_file_close_other);
  init (tr ("Switch to Left Tab"), sc_edit_tabs_switch_left_tab);
  init (tr ("Switch to Right Tab"), sc_edit_tabs_switch_right_tab);
  init (tr ("Move Tab Left"), sc_edit_tabs_move_tab_left);
  init (tr ("Move Tab Right"), sc_edit_tabs_move_tab_right);

  // Zooming
  init (tr ("Zoom In"), sc_edit_view_zoom_in);
  init (tr ("Zoom Out"), sc_edit_view_zoom_out);
#if defined (Q_OS_MAC)
  init (tr ("Zoom Normal"), sc_edit_view_zoom_normal);
#else
  init (tr ("Zoom Normal"), sc_edit_view_zoom_normal);
#endif

  // actions of the editor

  // file
  init (tr ("Edit Function"), sc_edit_file_edit_function);
  init (tr ("Save File"), sc_edit_file_save);
  init (tr ("Save File As"), sc_edit_file_save_as);
  init (tr ("Print"), sc_edit_file_print);

  // edit
  init (tr ("Redo"), sc_edit_edit_redo);
  init (tr ("Cut"), sc_edit_edit_cut);
  init (tr ("Find and Replace"), sc_edit_edit_find_replace);
  init (tr ("Find Next"), sc_edit_edit_find_next);
  init (tr ("Find Previous"), sc_edit_edit_find_previous);
  init (tr ("Delete to Start of Word"), sc_edit_edit_delete_start_word);
  init (tr ("Delete to End of Word"), sc_edit_edit_delete_end_word);
  init (tr ("Delete to Start of Line"), sc_edit_edit_delete_start_line);
  init (tr ("Delete to End of Line"), sc_edit_edit_delete_end_line);
  init (tr ("Delete Line"), sc_edit_edit_delete_line);
  init (tr ("Copy Line"), sc_edit_edit_copy_line);
  init (tr ("Cut Line"), sc_edit_edit_cut_line);
  init (tr ("Duplicate Selection/Line"), sc_edit_edit_duplicate_selection);
  init (tr ("Transpose Line"), sc_edit_edit_transpose_line);
  init (tr ("Show Completion List"), sc_edit_edit_completion_list);

  init (tr ("Comment Selection"), sc_edit_edit_comment_selection);
  init (tr ("Uncomment Selection"), sc_edit_edit_uncomment_selection);
  init (tr ("Comment Selection (Choosing String)"), sc_edit_edit_comment_var_selection);
  init (tr ("Uppercase Selection"), sc_edit_edit_upper_case);
  init (tr ("Lowercase Selection"), sc_edit_edit_lower_case);

#if defined (Q_OS_MAC)
  init (tr ("Indent Selection Rigidly"), sc_edit_edit_indent_selection);
  init (tr ("Unindent Selection Rigidly"), sc_edit_edit_unindent_selection);
#else
  init (tr ("Indent Selection Rigidly"), sc_edit_edit_indent_selection);
  init (tr ("Unindent Selection Rigidly"), sc_edit_edit_unindent_selection);
#endif
  init (tr ("Indent Code"), sc_edit_edit_smart_indent_line_or_selection);

  init (tr ("Convert Line Endings to Windows"), sc_edit_edit_conv_eol_winows);
  init (tr ("Convert Line Endings to Unix"), sc_edit_edit_conv_eol_unix);
  init (tr ("Convert Line Endings to Mac"), sc_edit_edit_conv_eol_mac);

  init (tr ("Goto Line"), sc_edit_edit_goto_line);
  init (tr ("Move to Matching Brace"), sc_edit_edit_move_to_brace);
  init (tr ("Select to Matching Brace"), sc_edit_edit_select_to_brace);
  init (tr ("Toggle Bookmark"), sc_edit_edit_toggle_bookmark);
  init (tr ("Next Bookmark"), sc_edit_edit_next_bookmark);
  init (tr ("Previous Bookmark"), sc_edit_edit_previous_bookmark);
  init (tr ("Remove All Bookmark"), sc_edit_edit_remove_bookmark);

  init (tr ("Preferences"), sc_edit_edit_preferences);
  init (tr ("Styles Preferences"), sc_edit_edit_styles_preferences);

  // view
  init (tr ("Show Line Numbers"), sc_edit_view_show_line_numbers);
  init (tr ("Show Whitespace Characters"), sc_edit_view_show_white_spaces);
  init (tr ("Show Line Endings"), sc_edit_view_show_eol_chars);
  init (tr ("Show Indentation Guides"), sc_edit_view_show_ind_guides);
  init (tr ("Show Long Line Marker"), sc_edit_view_show_long_line);
  init (tr ("Show Toolbar"), sc_edit_view_show_toolbar);
  init (tr ("Show Statusbar"), sc_edit_view_show_statusbar);
  init (tr ("Show Horizontal Scrollbar"), sc_edit_view_show_hscrollbar);
  init (tr ("Sort Tabs Alphabetically"), sc_edit_view_sort_tabs);

  // debug
  init (tr ("Toggle Breakpoint"), sc_edit_debug_toggle_breakpoint);
  init (tr ("Next Breakpoint"), sc_edit_debug_next_breakpoint);
  init (tr ("Previous Breakpoint"), sc_edit_debug_previous_breakpoint);
  init (tr ("Remove All Breakpoints"), sc_edit_debug_remove_breakpoints);

  // run
  init (tr ("Run File"), sc_edit_run_run_file);
  init (tr ("Run Selection"), sc_edit_run_run_selection);

  // help
  init (tr ("Help on Keyword"), sc_edit_help_help_keyword);
  init (tr ("Document on Keyword"), sc_edit_help_doc_keyword);

  // Documentation browser
  init (tr ("Go to Homepage"), sc_doc_go_home);
  init (tr ("Go Back one Page"), sc_doc_go_back);
  init (tr ("Go Forward one Page"), sc_doc_go_next);
  init (tr ("Bookmark this Page"), sc_doc_bookmark);
}

// write one or all actual shortcut set(s) into a settings file
void shortcut_manager::write_shortcuts (gui_settings *settings,
                                        bool closing)
{
  bool sc_ctrld = false;

  QString sc_main = sc_main_file.mid (0, sc_main_file.indexOf ('_') + 1);

  for (int i = 0; i < m_sc.count (); i++)  // loop over all shortcuts
    {
      settings->setValue (sc_group + "/" + m_sc.at (i).m_settings_key,
                          m_sc.at (i).m_actual_sc.toString ());
      // special: check main-window for Ctrl-D (Terminal)
      if (m_sc.at (i).m_settings_key.startsWith (sc_main)
          && m_sc.at (i).m_actual_sc == QKeySequence (Qt::ControlModifier+Qt::Key_D))
        sc_ctrld = true;
    }

  settings->setValue (sc_main_ctrld.key, sc_ctrld);

  if (closing)
    {
      delete m_dialog;     // the dialog for key sequences can be removed now
      m_dialog = nullptr;  // make sure it is zero again
    }

  settings->sync ();      // sync the settings file
}

void shortcut_manager::set_shortcut (QAction *action, const sc_pref& scpref,
                                     bool enable)
{
  if (! enable)
    {
      // Disable => remove existing shortcut from the action
      action->setShortcut (QKeySequence ());
      return;
    }

  // Enable: Is the given key known? If yes, get the value from the
  //         settings file and set it to the action
  int index;

  index = m_action_hash[scpref.key] - 1;

  if (index > -1 && index < m_sc.count ())
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();
      action->setShortcut (QKeySequence (settings->sc_value (scpref)));
    }
  else
    qDebug () << "Key: " << scpref.key << " not found in m_action_hash";
}

void shortcut_manager::shortcut (QShortcut *sc, const sc_pref& scpref)
{
  int index;

  index = m_action_hash[scpref.key] - 1;

  if (index > -1 && index < m_sc.count ())
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();
      sc->setKey (QKeySequence (settings->sc_value (scpref)));
    }
  else
    qDebug () << "Key: " << scpref.key << " not found in m_action_hash";
}

void shortcut_manager::fill_treewidget (QTreeWidget *tree_view)
{
  m_dialog = nullptr;
  m_level_hash.clear ();

  tree_view->header ()->setSectionResizeMode (QHeaderView::ResizeToContents);

  QTreeWidgetItem *main = new QTreeWidgetItem (tree_view);
  main->setText (0, tr ("Global"));
  main->setExpanded (true);
  QTreeWidgetItem *main_file = new QTreeWidgetItem (main);
  main_file->setText (0, tr ("File Menu"));
  QTreeWidgetItem *main_edit = new QTreeWidgetItem (main);
  main_edit->setText (0, tr ("Edit Menu"));
  QTreeWidgetItem *main_debug = new QTreeWidgetItem (main);
  main_debug->setText (0, tr ("Debug Menu"));
  QTreeWidgetItem *main_tools = new QTreeWidgetItem (main);
  main_tools->setText (0, tr ("Tools Menu"));
  QTreeWidgetItem *main_window = new QTreeWidgetItem (main);
  main_window->setText (0, tr ("Window Menu"));
  QTreeWidgetItem *main_help = new QTreeWidgetItem (main);
  main_help->setText (0, tr ("Help Menu"));
  QTreeWidgetItem *main_news = new QTreeWidgetItem (main);
  main_news->setText (0, tr ("News Menu"));
  QTreeWidgetItem *main_dock_widgets = new QTreeWidgetItem (main);
  main_dock_widgets->setText (0, tr ("Handling of Dock Widgets"));
  QTreeWidgetItem *main_tabs = new QTreeWidgetItem (main);
  main_tabs->setText (0, tr ("Tab Handling in Dock Widgets"));
  QTreeWidgetItem *main_find = new QTreeWidgetItem (main);
  main_find->setText (0, tr ("Find & Replace in Dock Widgets"));
  QTreeWidgetItem *main_zoom = new QTreeWidgetItem (main);
  main_zoom->setText (0, tr ("Zooming in Editor and Documentation"));

  m_level_hash[sc_main_file]   = main_file;
  m_level_hash[sc_main_edit]   = main_edit;
  m_level_hash[sc_main_debug]   = main_debug;
  m_level_hash[sc_main_tools]   = main_tools;
  m_level_hash[sc_main_window]   = main_window;
  m_level_hash[sc_main_help]   = main_help;
  m_level_hash[sc_main_news]   = main_news;
  m_level_hash[sc_dock_widget] = main_dock_widgets;
  m_level_hash[sc_edit_tabs]   = main_tabs;
  m_level_hash[sc_edit_find]   = main_find;
  m_level_hash[sc_edit_zoom]   = main_zoom;

  QTreeWidgetItem *editor = new QTreeWidgetItem (tree_view);
  editor->setText (0, tr ("Editor"));
  editor->setExpanded (true);
  QTreeWidgetItem *editor_file = new QTreeWidgetItem (editor);
  editor_file->setText (0, tr ("File Menu"));
  QTreeWidgetItem *editor_edit = new QTreeWidgetItem (editor);
  editor_edit->setText (0, tr ("Edit Menu"));
  QTreeWidgetItem *editor_view = new QTreeWidgetItem (editor);
  editor_view->setText (0, tr ("View Menu"));
  QTreeWidgetItem *editor_debug = new QTreeWidgetItem (editor);
  editor_debug->setText (0, tr ("Debug Menu"));
  QTreeWidgetItem *editor_run = new QTreeWidgetItem (editor);
  editor_run->setText (0, tr ("Run Menu"));
  QTreeWidgetItem *editor_help = new QTreeWidgetItem (editor);
  editor_help->setText (0, tr ("Help Menu"));

  m_level_hash[sc_edit_file] = editor_file;
  m_level_hash[sc_edit_edit] = editor_edit;
  m_level_hash[sc_edit_view] = editor_view;
  m_level_hash[sc_edit_debug] = editor_debug;
  m_level_hash[sc_edit_run] = editor_run;
  m_level_hash[sc_edit_help] = editor_help;

  QTreeWidgetItem *doc = new QTreeWidgetItem (tree_view);
  doc->setText (0, tr ("Documentation Viewer"));
  doc->setExpanded (true);

  QTreeWidgetItem *doc_browser = new QTreeWidgetItem (doc);
  doc_browser->setText (0, tr ("Browser"));

  m_level_hash[sc_doc] = doc_browser;

  connect (tree_view, &QTreeWidget::itemDoubleClicked,
           this, &shortcut_manager::handle_double_clicked);

  for (int i = 0; i < m_sc.count (); i++)
    {
      shortcut_t sc = m_sc.at (i);

      QTreeWidgetItem *section = m_level_hash[sc.m_settings_key.section (':', 0, 0)];

      // handle sections which have changed and do not correspond to the
      // previously defined keyname
      if (section == editor_file)
        {
          // Closing tabs now in global tab handling section
          if (sc.m_settings_key.contains (sc_edit_file_cl))
            section = main_tabs;
        }
      if (section == editor_edit)
        {
          // Find & replace now in global file & replace handling section
          if (sc.m_settings_key.contains (sc_edit_edit_find))
            section = main_find;
        }
      if (section == editor_view)
        {
          // Zooming now in global zoom handling section
          if (sc.m_settings_key.contains (sc_edit_view_zoom))
            section = main_zoom;
        }

      QTreeWidgetItem *tree_item = new QTreeWidgetItem (section);

      // set a slightly transparent foreground for default columns
      QColor fg = QColor (tree_item->foreground (1).color ());
      fg.setAlpha (128);
      tree_item->setForeground (1, QBrush (fg));

      // write the shortcuts
      tree_item->setText (0, sc.m_description);
      tree_item->setText (1, sc.m_default_sc.toString ());
      tree_item->setText (2, sc.m_actual_sc.toString ());

      m_item_index_hash[tree_item] = i + 1; // index+1 to avoid 0
      m_index_item_hash[i] = tree_item;
    }
}

// import or export of shortcut sets,
// called from settings dialog when related buttons are clicked;
// returns true on success, false otherwise
bool
shortcut_manager::import_export (int action)
{
  // ask to save the current shortcuts, maybe abort import
  if (action == OSC_DEFAULT || action == OSC_IMPORT)
    {
      if (! overwrite_all_shortcuts ())
        return false;
    }

  // get the filename to read or write the shortcuts,
  // the default extension is .osc (octave shortcuts)
  if (action != OSC_DEFAULT)
    {
      QString file;

      // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
      int opts = 0;  // No options by default.
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();
      if (! settings->value (global_use_native_dialogs).toBool ())
        opts = QFileDialog::DontUseNativeDialog;

      if (action == OSC_IMPORT)
        file = QFileDialog::getOpenFileName (this,
                                             tr ("Import shortcuts from file..."), QString (),
                                             tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                                             nullptr, QFileDialog::Option (opts));
      else if (action == OSC_EXPORT)
        file = QFileDialog::getSaveFileName (this,
                                             tr ("Export shortcuts to file..."), QString (),
                                             tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                                             nullptr, QFileDialog::Option (opts));

      if (file.isEmpty ())
        return false;

      gui_settings osc_settings (file, QSettings::IniFormat);

      if (osc_settings.status () !=  QSettings::NoError)
        {
          qWarning () << tr ("Failed to open %1 as Octave shortcut file")
            .arg (file);
          return false;
        }
      else
        {
          if (action == OSC_IMPORT)
            import_shortcuts (&osc_settings);   // import (special action)
          else if (action == OSC_EXPORT)
            write_shortcuts (&osc_settings, false); // export, (save settings)
        }
    }
  else
    {
      import_shortcuts (nullptr);
    }

  return true;
}

void shortcut_manager::handle_double_clicked (QTreeWidgetItem *item, int col)
{
  if (col != 2)
    return;

  int i = m_item_index_hash[item];
  if (i == 0)
    return;  // top-level-item clicked

  shortcut_dialog (i-1); // correct to index starting at 0
}

void shortcut_manager::shortcut_dialog_finished (int result)
{
  if (result == QDialog::Rejected)
    return;

  // check for duplicate
  int double_index = m_shortcut_hash[m_edit_actual->text ()] - 1;

  if (double_index >= 0 && double_index != m_handled_index)
    {
      int ret = QMessageBox::warning (this, tr ("Double Shortcut"),
                                      tr ("The chosen shortcut\n  \"%1\"\n"
                                          "is already used for the action\n  \"%2\".\n"
                                          "Do you want to use the shortcut anyhow removing it "
                                          "from the previous action?")
                                      .arg (m_edit_actual->text ())
                                      .arg (m_sc.at (double_index).m_description),
                                      QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ret == QMessageBox::Yes)
        {
          shortcut_t double_shortcut = m_sc.at (double_index);
          double_shortcut.m_actual_sc = QKeySequence ();
          m_sc.replace (double_index, double_shortcut);
          m_index_item_hash[double_index]->setText (2, QString ());
        }
      else
        return;
    }

  shortcut_t shortcut = m_sc.at (m_handled_index);
  if (! shortcut.m_actual_sc.isEmpty ())
    m_shortcut_hash.remove (shortcut.m_actual_sc.toString ());
  shortcut.m_actual_sc = m_edit_actual->text ();
  m_sc.replace (m_handled_index, shortcut);

  m_index_item_hash[m_handled_index]->setText (2, shortcut.m_actual_sc.toString ());

  if (! shortcut.m_actual_sc.isEmpty ())
    m_shortcut_hash[shortcut.m_actual_sc.toString ()] = m_handled_index + 1;
}

void shortcut_manager::shortcut_dialog_set_default (void)
{
  m_edit_actual->setText (m_label_default->text ());
}

void shortcut_manager::init (const QString& description, const sc_pref& sc)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QKeySequence actual = QKeySequence (settings->sc_value (sc));

  // append the new shortcut to the list
  shortcut_t shortcut_info;
  shortcut_info.m_description = description;
  shortcut_info.m_settings_key = sc.key;
  shortcut_info.m_actual_sc = actual;
  shortcut_info.m_default_sc = settings->sc_def_value (sc);
  m_sc << shortcut_info;

  // insert shortcut in order to check for duplicates later
  if (! actual.isEmpty ())
    m_shortcut_hash[actual.toString ()] = m_sc.count ();
  m_action_hash[sc.key] = m_sc.count ();

  // check whether ctrl+d is used from main window, i.e. is a global shortcut
  QString main_group_prefix
    = sc_main_file.mid (0, sc_main_file.indexOf ('_') + 1);
  if (sc.key.startsWith (main_group_prefix)
      && actual == QKeySequence (Qt::ControlModifier+Qt::Key_D))
    settings->setValue (sc_main_ctrld.key, true);
}

void shortcut_manager::shortcut_dialog (int index)
{
  if (! m_dialog)
    {
      m_dialog = new QDialog (this);

      m_dialog->setWindowTitle (tr ("Enter new Shortcut"));

      QVBoxLayout *box = new QVBoxLayout (m_dialog);
      box->setSpacing (2);
      box->setContentsMargins (12, 12, 12, 12);

      QLabel *help = new QLabel (tr ("Apply the desired shortcut or click "
                                     "on the right button to reset the "
                                     "shortcut to its default."));
      help->setWordWrap (true);
      box->addWidget (help);

      QCheckBox *direct
        = new QCheckBox (tr ("Enter shortcut directly by performing it"));

      QCheckBox *shift
        = new QCheckBox (tr ("Add Shift modifier\n"
                             "(allows one to enter number keys)"));

      shift->setStyleSheet
        ("QCheckBox::indicator { subcontrol-position: left top; }");

      connect (direct, &QCheckBox::clicked, shift, &QCheckBox::setEnabled);

      direct->setCheckState (Qt::Checked);

      box->addWidget (direct);
      box->addWidget (shift);

      box->addSpacing (15);

      QGridLayout *grid = new QGridLayout ();

      QLabel *actual = new QLabel (tr ("Actual shortcut"));
      m_edit_actual = new enter_shortcut (m_dialog);
      m_edit_actual->setAlignment (Qt::AlignHCenter);
      grid->addWidget (actual, 0, 0);
      grid->addWidget (m_edit_actual, 0, 1);

      QLabel *def = new QLabel (tr ("Default shortcut"));
      m_label_default = new QLabel (m_dialog);
      m_label_default->setAlignment (Qt::AlignHCenter);
      grid->addWidget (def, 1, 0);
      grid->addWidget (m_label_default, 1, 1);

      QPushButton *set_default = new QPushButton (tr ("Set to default"));
      grid->addWidget (set_default, 0, 2);
      connect (set_default, &QPushButton::clicked,
               this, &shortcut_manager::shortcut_dialog_set_default);

      box->addLayout (grid);

      box->addSpacing (18);

      QDialogButtonBox *button_box = new QDialogButtonBox (QDialogButtonBox::Ok
                                                           | QDialogButtonBox::Cancel);
      QList<QAbstractButton *> buttons = button_box->buttons ();
      for (int i = 0; i < buttons.count (); i++)
        buttons.at (i)->setShortcut (QKeySequence ());
      connect (button_box, &QDialogButtonBox::accepted,
               m_dialog, &QDialog::accept);
      connect (button_box, &QDialogButtonBox::rejected,
               m_dialog, &QDialog::reject);
      box->addWidget (button_box);

      m_dialog->setLayout (box);

      connect (direct, &QCheckBox::stateChanged,
               m_edit_actual, &enter_shortcut::handle_direct_shortcut);
      connect (shift, &QCheckBox::stateChanged,
               m_edit_actual, &enter_shortcut::handle_shift_modifier);
      connect (m_dialog, &QDialog::finished,
               this, &shortcut_manager::shortcut_dialog_finished);

    }

  m_edit_actual->setText (m_sc.at (index).m_actual_sc.toString ());
  m_label_default->setText (m_sc.at (index).m_default_sc.toString ());
  m_handled_index = index;

  m_edit_actual->setFocus ();
  m_dialog->setFocusProxy (m_edit_actual);
  m_dialog->exec ();
}

// import a shortcut set from a given settings file or reset to
// the defaults (settings = 0) and refresh the tree view
void shortcut_manager::import_shortcuts (gui_settings *settings)
{
  for (int i = 0; i < m_sc.count (); i++)
    {
      // update the list of all shortcuts
      shortcut_t sc = m_sc.at (i);           // make a copy

      if (settings)
        sc.m_actual_sc = QKeySequence (         // get new shortcut from settings
                                       settings->value (sc_group + sc.m_settings_key,sc.m_actual_sc).
                                       toString ());       // and use the old one as default
      else
        sc.m_actual_sc = QKeySequence (sc.m_default_sc); // get default shortcut

      m_sc.replace (i, sc);                  // replace the old with the new one

      // update the tree view
      QTreeWidgetItem *tree_item = m_index_item_hash[i]; // get related tree item
      tree_item->setText (2, sc.m_actual_sc.toString ()); // display new shortcut
    }
}

// ask the user whether to save the current shortcut set;
// returns true to proceed with import action, false to abort it
bool shortcut_manager::overwrite_all_shortcuts (void)
{
  QMessageBox msg_box;
  msg_box.setWindowTitle (tr ("Overwriting Shortcuts"));
  msg_box.setIcon (QMessageBox::Warning);
  msg_box.setText (tr ("You are about to overwrite all shortcuts.\n"
                       "Would you like to save the current shortcut set or cancel the action?"));
  msg_box.setStandardButtons (QMessageBox::Save | QMessageBox::Cancel);
  QPushButton *discard = msg_box.addButton (tr ("Don't save"),
                                            QMessageBox::DestructiveRole);
  msg_box.setDefaultButton (QMessageBox::Save);

  int ret = msg_box.exec ();

  if (msg_box.clickedButton () == discard)
    return true;  // do not save and go ahead

  if (ret == QMessageBox::Save)
    {
      if (import_export (OSC_EXPORT))
        return true;  // go ahead
    }

  return false; // abort the import
}

OCTAVE_END_NAMESPACE(octave)
