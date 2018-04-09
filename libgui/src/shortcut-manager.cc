/*

Copyright (C) 2014-2018 Torsten <ttl@justmail.de>

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QtCore>
#include <QMessageBox>
#include <QDebug>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QKeySequence>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QHeaderView>
#include <QAction>
#include <QFileDialog>

#include "error.h"
#include "resource-manager.h"
#include "shortcut-manager.h"

namespace octave
{
  // enter_shortcut:
  // class derived from QLineEdit for directly entering key sequences which

  enter_shortcut::enter_shortcut (QWidget *p) : QLineEdit (p)
  {
    m_direct_shortcut = true;      // the shortcut is directly entered
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

        Qt::KeyboardModifiers modifiers = e->modifiers ();

        if (modifiers & Qt::ShiftModifier)
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

  shortcut_manager *shortcut_manager::instance = nullptr;

  shortcut_manager::shortcut_manager (void)
  {
    setObjectName ("Shortcut_Manager");

    // Mac: don't let Qt interpret CMD key ("Meta" in Qt terminology) as Ctrl
#if defined (Q_OS_MAC)
    QCoreApplication::setAttribute (Qt::AA_MacDontSwapCtrlAndMeta, true);
#endif

    m_settings = resource_manager::get_settings ();
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

  bool shortcut_manager::instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new shortcut_manager ();

    if (! instance)
      {
        error ("unable to create shortcut_manager object!");

        retval = false;
      }

    return retval;
  }

  void shortcut_manager::init (const QString& description, const QString& key,
                               const QKeySequence& def_sc)
  {
    QKeySequence actual
      = QKeySequence (m_settings->value ("shortcuts/" + key, def_sc).toString ());

    // append the new shortcut to the list
    shortcut_t shortcut_info;
    shortcut_info.m_description = description;
    shortcut_info.m_settings_key = key;
    shortcut_info.m_actual_sc = actual;
    shortcut_info.m_default_sc = def_sc;
    m_sc << shortcut_info;

    // insert shortcut in order check for duplicates later
    if (! actual.isEmpty ())
      m_shortcut_hash[actual.toString ()] = m_sc.count ();
    m_action_hash[key] = m_sc.count ();

    // check whether ctrl+d is used from main window, i.e. is a global shortcut
    if (key.startsWith ("main_")
        && actual == QKeySequence (Qt::ControlModifier+Qt::Key_D))
      m_settings->setValue ("shortcuts/main_ctrld",true);
  }

  void shortcut_manager::do_init_data (void)
  {
    Qt::KeyboardModifier ctrl;
    int prefix;
#if defined (Q_OS_MAC)
    // Use CMD key as an equivalent of Ctrl key on other platforms
    ctrl = Qt::MetaModifier;
    // Some of octave default shortcuts on windows/linux are already defined
    // as system wide shortcuts on Mac Os X (almost all Function keys).
    // Prefix those with Option (Alt) modifier to avoid conflicts.
    prefix = Qt::AltModifier;
#else
    ctrl = Qt::ControlModifier;
    prefix = Qt::NoModifier;
#endif

    Qt::KeyboardModifiers ctrl_shift = ctrl | Qt::ShiftModifier;
    Qt::KeyboardModifiers ctrl_alt = ctrl | Qt::AltModifier;

    // actions of the main window

    m_settings->setValue ("shortcuts/main_ctrld",false); // reset use fo ctrl-d

    // file
    init (tr ("New File"), "main_file:new_file", QKeySequence::New);
    init (tr ("New Function"), "main_file:new_function",
          QKeySequence (ctrl_shift + Qt::Key_N));
    init (tr ("New Figure"), "main_file:new_figure", QKeySequence ());
    init (tr ("Open File"), "main_file:open_file", QKeySequence::Open);
    init (tr ("Load Workspace"), "main_file:load_workspace", QKeySequence ());
    init (tr ("Save Workspace As"), "main_file:save_workspace", QKeySequence ());
    init (tr ("Exit Octave"), "main_file:exit", QKeySequence::Quit);

    // edit
    init (tr ("Copy"), "main_edit:copy", QKeySequence::Copy);
    init (tr ("Paste"), "main_edit:paste", QKeySequence::Paste);
    init (tr ("Undo"), "main_edit:undo", QKeySequence::Undo);
    init (tr ("Select All"), "main_edit:select_all", QKeySequence::SelectAll);
    init (tr ("Clear Clipboard"), "main_edit:clear_clipboard", QKeySequence ());
    init (tr ("Find in Files"), "main_edit:find_in_files",
          QKeySequence (ctrl_shift + Qt::Key_F));
    init (tr ("Clear Command Window"), "main_edit:clear_command_window",
          QKeySequence ());
    init (tr ("Clear Command History"), "main_edit:clear_history",
          QKeySequence ());
    init (tr ("Clear Workspace"), "main_edit:clear_workspace", QKeySequence ());
    init (tr ("Preferences"), "main_edit:preferences", QKeySequence ());

    // debug
    init (tr ("Step"), "main_debug:step_over",
          QKeySequence (prefix + Qt::Key_F10));
    init (tr ("Step Into"), "main_debug:step_into",
          QKeySequence (prefix + Qt::Key_F11));
    init (tr ("Step Out"), "main_debug:step_out",
          QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_F11));
    init (tr ("Continue"), "main_debug:continue",
          QKeySequence (prefix + Qt::Key_F5));
    init (tr ("Quit Debug Mode"), "main_debug:quit",
          QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_F5));

    // window
    init (tr ("Show Command Window"), "main_window:show_command",
          prefix + ctrl_shift + Qt::Key_0);
    init (tr ("Show Command History"), "main_window:show_history",
          prefix + ctrl_shift + Qt::Key_1);
    init (tr ("Show File Browser"), "main_window:show_file_browser",
          prefix + ctrl_shift + Qt::Key_2);
    init (tr ("Show Workspace"), "main_window:show_workspace",
          prefix + ctrl_shift + Qt::Key_3);
    init (tr ("Show Editor"), "main_window:show_editor",
          prefix + ctrl_shift + Qt::Key_4);
    init (tr ("Show Documentation"), "main_window:show_doc",
          prefix + ctrl_shift + Qt::Key_5);
    init (tr ("Show Variable Editor"), "main_window:show_variable_editor",
          prefix + ctrl_shift + Qt::Key_6);
    init (tr ("Command Window"), "main_window:command",
          prefix + ctrl + Qt::Key_0);
    init (tr ("Command History"), "main_window:history",
          prefix + ctrl + Qt::Key_1);
    init (tr ("File Browser"), "main_window:file_browser",
          prefix + ctrl + Qt::Key_2);
    init (tr ("Workspace"), "main_window:workspace",
          prefix + ctrl + Qt::Key_3);
    init (tr ("Editor"), "main_window:editor",
          prefix + ctrl + Qt::Key_4);
    init (tr ("Documentation"), "main_window:doc",
          prefix + ctrl + Qt::Key_5);
    init (tr ("Variable Editor"), "main_window:variable_editor",
          prefix + ctrl + Qt::Key_6);
    init (tr ("Reset Default Window Layout"), "main_window:reset", QKeySequence ());

    // help
    init (tr ("Show Ondisk Documentation"), "main_help:ondisk_doc",
          QKeySequence ());
    init (tr ("Show Online Documentation"), "main_help:online_doc",
          QKeySequence ());
    init (tr ("Report Bug"), "main_help:report_bug", QKeySequence ());
    init (tr ("Octave Packages"), "main_help:packages", QKeySequence ());
    init (tr ("Contribute to Octave"), "main_help:contribute", QKeySequence ());
    init (tr ("Octave Developer Resources"), "main_help:developer",
          QKeySequence ());
    init (tr ("About Octave"), "main_help:about", QKeySequence ());

    // news
    init (tr ("Release Notes"), "main_news:release_notes", QKeySequence ());
    init (tr ("Community News"), "main_news:community_news", QKeySequence ());

    // Tab handling
    // The following shortcuts are moved into a separate tab. The key names
    // are not change for preserving compatibility with older versions
    init (tr ("Close Tab"), "editor_file:close", QKeySequence::Close);
    init (tr ("Close All Tabs"), "editor_file:close_all", QKeySequence ());
    init (tr ("Close Other Tabs"), "editor_file:close_other", QKeySequence ());
    init (tr ("Switch to Left Tab"), "editor_tabs:switch_left_tab",
          QKeySequence (ctrl + Qt::Key_PageUp));
    init (tr ("Switch to Right Tab"), "editor_tabs:switch_right_tab",
          QKeySequence (ctrl + Qt::Key_PageDown));
    init (tr ("Move Tab Left"), "editor_tabs:move_tab_left",
          QKeySequence (Qt::AltModifier + Qt::Key_PageUp));
    init (tr ("Move Tab Right"), "editor_tabs:move_tab_right",
          QKeySequence (Qt::AltModifier + Qt::Key_PageDown));

    // actions of the editor

    // file
    init (tr ("Edit Function"), "editor_file:edit_function",
          QKeySequence (ctrl + Qt::Key_E));
    init (tr ("Save File"), "editor_file:save", QKeySequence::Save);
    init (tr ("Save File As"), "editor_file:save_as", QKeySequence::SaveAs);
    init (tr ("Print"), "editor_file:print", QKeySequence::Print);

    // edit
    init (tr ("Redo"), "editor_edit:redo", QKeySequence::Redo);
    init (tr ("Cut"), "editor_edit:cut", QKeySequence::Cut);
    init (tr ("Find and Replace"), "editor_edit:find_replace",
          QKeySequence::Find);
    init (tr ("Find Next"), "editor_edit:find_next",
          QKeySequence::FindNext);
    init (tr ("Find Previous"), "editor_edit:find_previous",
          QKeySequence::FindPrevious);
    init (tr ("Delete to Start of Word"), "editor_edit:delete_start_word",
          QKeySequence::DeleteStartOfWord);
    init (tr ("Delete to End of Word"), "editor_edit:delete_end_word",
          QKeySequence::DeleteEndOfWord);
    init (tr ("Delete to Start of Line"), "editor_edit:delete_start_line",
          QKeySequence (ctrl_shift + Qt::Key_Backspace));
    init (tr ("Delete to End of Line"), "editor_edit:delete_end_line",
          QKeySequence (ctrl_shift + Qt::Key_Delete));
    init (tr ("Delete Line"), "editor_edit:delete_line",
          QKeySequence (ctrl_shift + Qt::Key_L));
    init (tr ("Copy Line"), "editor_edit:copy_line",
          QKeySequence (ctrl_shift + Qt::Key_C));
    init (tr ("Cut Line"), "editor_edit:cut_line",
          QKeySequence (ctrl_shift + Qt::Key_X));
    init (tr ("Duplicate Selection/Line"), "editor_edit:duplicate_selection",
          QKeySequence (ctrl + Qt::Key_D));
    init (tr ("Transpose Line"), "editor_edit:transpose_line",
          QKeySequence (ctrl + Qt::Key_T));
    init (tr ("Show Completion List"), "editor_edit:completion_list",
          QKeySequence (ctrl + Qt::Key_Space));

    init (tr ("Comment Selection"), "editor_edit:comment_selection",
          QKeySequence (ctrl + Qt::Key_R));
    init (tr ("Uncomment Selection"), "editor_edit:uncomment_selection",
          QKeySequence (ctrl_shift + Qt::Key_R));
    init (tr ("Comment Selection (Choosing String)"), "editor_edit:comment_var_selection",
          QKeySequence (ctrl_alt + Qt::Key_R));
    init (tr ("Uppercase Selection"), "editor_edit:upper_case",
          QKeySequence (ctrl + Qt::Key_U));
    init (tr ("Lowercase Selection"), "editor_edit:lower_case",
          QKeySequence (ctrl_alt + Qt::Key_U));

#if defined (Q_OS_MAC)
    init (tr ("Indent Selection Rigidly"), "editor_edit:indent_selection",
          QKeySequence (prefix + Qt::Key_Tab));
    init (tr ("Unindent Selection Rigidly"), "editor_edit:unindent_selection",
          QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_Tab));
#else
    init (tr ("Indent Selection Rigidly"), "editor_edit:indent_selection",
          QKeySequence (ctrl + Qt::Key_Tab));
    init (tr ("Unindent Selection Rigidly"), "editor_edit:unindent_selection",
          QKeySequence (ctrl_shift + Qt::Key_Tab));
#endif
    init (tr ("Indent Code"), "editor_edit:smart_indent_line_or_selection",
          QKeySequence ());

    init (tr ("Convert Line Endings to Windows"), "editor_edit:conv_eol_winows",
          QKeySequence ());
    init (tr ("Convert Line Endings to Unix"), "editor_edit:conv_eol_unix",
          QKeySequence ());
    init (tr ("Convert Line Endings to Mac"), "editor_edit:conv_eol_mac",
          QKeySequence ());

    init (tr ("Goto Line"), "editor_edit:goto_line",
          QKeySequence (ctrl + Qt::Key_L));
    init (tr ("Move to Matching Brace"), "editor_edit:move_to_brace",
          QKeySequence (ctrl + Qt::Key_M));
    init (tr ("Select to Matching Brace"), "editor_edit:select_to_brace",
          QKeySequence (ctrl_shift + Qt::Key_M));
    init (tr ("Toggle Bookmark"), "editor_edit:toggle_bookmark",
          QKeySequence (prefix + Qt::Key_F7));
    init (tr ("Next Bookmark"), "editor_edit:next_bookmark",
          QKeySequence (prefix + Qt::Key_F2));
    init (tr ("Previous Bookmark"), "editor_edit:previous_bookmark",
          QKeySequence (prefix + Qt::SHIFT + Qt::Key_F2));
    init (tr ("Remove All Bookmark"), "editor_edit:remove_bookmark",
          QKeySequence ());

    init (tr ("Preferences"), "editor_edit:preferences", QKeySequence ());
    init (tr ("Styles Preferences"), "editor_edit:styles_preferences",
          QKeySequence ());

    // view
    init (tr ("Show Line Numbers"), "editor_view:show_line_numbers",
          QKeySequence ());
    init (tr ("Show Whitespace Characters"), "editor_view:show_white_spaces",
          QKeySequence ());
    init (tr ("Show Line Endings"), "editor_view:show_eol_chars", QKeySequence ());
    init (tr ("Show Indentation Guides"), "editor_view:show_ind_guides",
          QKeySequence ());
    init (tr ("Show Long Line Marker"), "editor_view:show_long_line",
          QKeySequence ());
    init (tr ("Show Toolbar"), "editor_view:show_toolbar",
          QKeySequence ());
    init (tr ("Show Statusbar"), "editor_view:show_statusbar",
          QKeySequence ());
    init (tr ("Show Horizontal Scrollbar"), "editor_view:show_hscrollbar",
          QKeySequence ());
    init (tr ("Zoom In"), "editor_view:zoom_in", QKeySequence::ZoomIn);
    init (tr ("Zoom Out"), "editor_view:zoom_out", QKeySequence::ZoomOut);
#if defined (Q_OS_MAC)
    init (tr ("Zoom Normal"), "editor_view:zoom_normal",
          QKeySequence (ctrl + Qt::Key_Underscore));
#else
    init (tr ("Zoom Normal"), "editor_view:zoom_normal",
          QKeySequence (ctrl + Qt::Key_Period));
#endif

    // debug
    init (tr ("Toggle Breakpoint"), "editor_debug:toggle_breakpoint",
          QKeySequence ());
    init (tr ("Next Breakpoint"), "editor_debug:next_breakpoint",
          QKeySequence ());
    init (tr ("Previous Breakpoint"), "editor_debug:previous_breakpoint",
          QKeySequence ());
    init (tr ("Remove All Breakpoints"), "editor_debug:remove_breakpoints",
          QKeySequence ());

    // run
    init (tr ("Run File"), "editor_run:run_file",
          QKeySequence (prefix + Qt::Key_F5));
    init (tr ("Run Selection"), "editor_run:run_selection",
          QKeySequence (prefix + Qt::Key_F9));

    // help
    init (tr ("Help on Keyword"), "editor_help:help_keyword",
          QKeySequence::HelpContents);
    init (tr ("Document on Keyword"), "editor_help:doc_keyword",
          QKeySequence (Qt::SHIFT + Qt::Key_F1));

  }

  // write one or all actual shortcut set(s) into a settings file
  void shortcut_manager::do_write_shortcuts (QSettings *settings,
                                             bool closing)
  {
    bool sc_ctrld = false;

    for (int i = 0; i < m_sc.count (); i++)  // loop over all shortcuts
      {
        settings->setValue ("shortcuts/" + m_sc.at (i).m_settings_key,
                            m_sc.at (i).m_actual_sc.toString ());
        // special: check main-window for Ctrl-D (Terminal)
        if (m_sc.at (i).m_settings_key.startsWith ("main_")
            && m_sc.at (i).m_actual_sc == QKeySequence (Qt::ControlModifier+Qt::Key_D))
          sc_ctrld = true;
      }

    settings->setValue ("shortcuts/main_ctrld",sc_ctrld);

    if (closing)
      {
        delete m_dialog;     // the dialog for key sequences can be removed now
        m_dialog = nullptr;  // make sure it is zero again
      }

    settings->sync ();      // sync the settings file
  }

  void shortcut_manager::do_set_shortcut (QAction *action, const QString& key)
  {
    int index;

    index = m_action_hash[key] - 1;

    if (index > -1 && index < m_sc.count ())
      action->setShortcut (QKeySequence (
                                         m_settings->value ("shortcuts/" + key, m_sc.at (index).m_default_sc).toString ()));
    else
      qDebug () << "Key: " << key << " not found in m_action_hash";
  }

  void shortcut_manager::do_fill_treewidget (QTreeWidget *tree_view)
  {
    m_dialog = nullptr;
    m_level_hash.clear ();

#if defined (HAVE_QHEADERVIEW_SETSECTIONRESIZEMODE)
    tree_view->header ()->setSectionResizeMode (QHeaderView::ResizeToContents);
#else
    tree_view->header ()->setResizeMode (QHeaderView::ResizeToContents);
#endif

    QTreeWidgetItem *main = new QTreeWidgetItem (tree_view);
    main->setText (0, tr ("Global"));
    main->setExpanded (true);
    QTreeWidgetItem *main_file = new QTreeWidgetItem (main);
    main_file->setText (0, tr ("File Menu"));
    QTreeWidgetItem *main_edit = new QTreeWidgetItem (main);
    main_edit->setText (0, tr ("Edit Menu"));
    QTreeWidgetItem *main_debug = new QTreeWidgetItem (main);
    main_debug->setText (0, tr ("Debug Menu"));
    QTreeWidgetItem *main_window = new QTreeWidgetItem (main);
    main_window->setText (0, tr ("Window Menu"));
    QTreeWidgetItem *main_help = new QTreeWidgetItem (main);
    main_help->setText (0, tr ("Help Menu"));
    QTreeWidgetItem *main_news = new QTreeWidgetItem (main);
    main_news->setText (0, tr ("News Menu"));
    QTreeWidgetItem *main_tabs = new QTreeWidgetItem (main);
    main_tabs->setText (0, tr ("Tab Handling in Dock Widgets"));

    m_level_hash["main_file"]   = main_file;
    m_level_hash["main_edit"]   = main_edit;
    m_level_hash["main_debug"]   = main_debug;
    m_level_hash["main_window"]   = main_window;
    m_level_hash["main_help"]   = main_help;
    m_level_hash["main_news"]   = main_news;
    m_level_hash["main_tabs"]   = main_tabs;
    m_level_hash["editor_tabs"]   = main_tabs;

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

    m_level_hash["editor_file"] = editor_file;
    m_level_hash["editor_edit"] = editor_edit;
    m_level_hash["editor_view"] = editor_view;
    m_level_hash["editor_debug"] = editor_debug;
    m_level_hash["editor_run"] = editor_run;
    m_level_hash["editor_help"] = editor_help;

    connect (tree_view, SIGNAL (itemDoubleClicked (QTreeWidgetItem*, int)),
             this, SLOT (handle_double_clicked (QTreeWidgetItem*, int)));

    for (int i = 0; i < m_sc.count (); i++)
      {
        shortcut_t sc = m_sc.at (i);

        QTreeWidgetItem *section = m_level_hash[sc.m_settings_key.section (':',0,0)];

        // handle sections which have changed and do not correspond to the
        // previously defined keyname
        if (section == editor_file)
          {
            // Closing tabs now in global tab handling section
            if (sc.m_settings_key.contains ("editor_file:close"))
              section = main_tabs;
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
  shortcut_manager::do_import_export (int action)
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

        if (action == OSC_IMPORT)
          file = QFileDialog::getOpenFileName (this,
                                               tr ("Import shortcuts from file"), QString (),
                                               tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                                               nullptr, QFileDialog::DontUseNativeDialog);
        else if (action == OSC_EXPORT)
          file = QFileDialog::getSaveFileName (this,
                                               tr ("Export shortcuts to file"), QString (),
                                               tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                                               nullptr, QFileDialog::DontUseNativeDialog);

        if (file.isEmpty ())
          return false;

        QSettings *osc_settings = new QSettings (file, QSettings::IniFormat);

        if (! osc_settings)
          {
            qWarning () << tr ("Failed to open %1 as Octave shortcut file")
                        .arg (file);
            return false;
          }
        else
          {
            if (action == OSC_IMPORT)
              import_shortcuts (osc_settings);   // import (special action)
            else if (action == OSC_EXPORT)
              do_write_shortcuts (osc_settings, false); // export, (save settings)
          }
      }
    else
      {
        import_shortcuts (nullptr);
      }

    return true;
  }

  void shortcut_manager::shortcut_dialog (int index)
  {
    if (! m_dialog)
      {
        m_dialog = new QDialog (this);

        m_dialog->setWindowTitle (tr ("Enter new Shortcut"));

        QVBoxLayout *box = new QVBoxLayout (m_dialog);

        QLabel *help = new QLabel (tr ("Apply the desired shortcut or click "
                                       "on the right button to reset the "
                                       "shortcut to its default."));
        help->setWordWrap (true);
        box->addWidget (help);

        QCheckBox *direct = new QCheckBox (
                                           tr ("Enter shortcut directly by performing it"));
        direct->setCheckState (Qt::Checked);
        box->addWidget (direct);

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
        connect (set_default, SIGNAL (clicked ()),
                 this, SLOT (shortcut_dialog_set_default ()));

        box->addLayout (grid);

        QDialogButtonBox *button_box = new QDialogButtonBox (QDialogButtonBox::Ok
                                                             | QDialogButtonBox::Cancel);
        QList<QAbstractButton *> buttons = button_box->buttons ();
        for (int i = 0; i < buttons.count (); i++)
          buttons.at (i)->setShortcut (QKeySequence ());
        connect (button_box, SIGNAL (accepted ()), m_dialog, SLOT (accept ()));
        connect (button_box, SIGNAL (rejected ()), m_dialog, SLOT (reject ()));
        box->addWidget (button_box);

        m_dialog->setLayout (box);

        connect (direct, SIGNAL (stateChanged (int)),
                 m_edit_actual, SLOT (handle_direct_shortcut (int)));
        connect (m_dialog, SIGNAL (finished (int)),
                 this, SLOT (shortcut_dialog_finished (int)));

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
  void shortcut_manager::import_shortcuts (QSettings *settings)
  {
    for (int i = 0; i < m_sc.count (); i++)
      {
        // update the list of all shortcuts
        shortcut_t sc = m_sc.at (i);           // make a copy

        if (settings)
          sc.m_actual_sc = QKeySequence (         // get new shortcut from settings
                                         settings->value ("shortcuts/" + sc.m_settings_key,sc.m_actual_sc).
                                         toString ());       // and use the old one as default
        else
          sc.m_actual_sc = QKeySequence (sc.m_default_sc); // get default shortcut

        m_sc.replace (i,sc);                   // replace the old with the new one

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
        if (do_import_export (OSC_EXPORT))
          return true;  // go ahead
      }

    return false; // abort the import
  }
}
