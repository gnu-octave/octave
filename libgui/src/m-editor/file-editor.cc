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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include "file-editor.h"
#include "resource-manager.h"
#include "shortcut-manager.h"

#include <QApplication>
#include <QFile>
#include <QFileDialog>
#include <QFont>
#include <QMessageBox>
#include <QMimeData>
#include <QProcess>
#include <QPushButton>
#include <QStyle>
#include <QTabBar>
#include <QTextStream>
#include <QVBoxLayout>
#include <Qsci/qscicommandset.h>

#include "main-window.h"
#include "oct-map.h"
#include "octave-link.h"
#include "utils.h"

namespace octave
{
  // Functions of the the reimplemented tab widget

  file_editor_tab_widget::file_editor_tab_widget (QWidget *p)
    : QTabWidget (p)
  {
    tab_bar *bar = new tab_bar (this);

    connect (bar, SIGNAL (close_current_tab_signal (bool)),
             p->parent (), SLOT (request_close_file (bool)));

    this->setTabBar (bar);

    setTabsClosable (true);
#if defined (HAVE_QTABWIDGET_SETMOVABLE)
    setMovable (true);
#endif
  }

  tab_bar * file_editor_tab_widget::get_tab_bar (void) const
  {
    return qobject_cast<tab_bar *> (tabBar ());
  }


  // File editor

  file_editor::file_editor (QWidget *p)
    : file_editor_interface (p)
  {
    // Set current editing directory before construct because loaded
    // files will change ced accordingly.
    m_ced = QDir::currentPath ();

    // set action that are later added by the main window to null,
    // preventing access to them when they are still undefined
    m_undo_action = nullptr;
    m_copy_action = nullptr;
    m_paste_action = nullptr;
    m_selectall_action = nullptr;
    m_closed = false;
    m_no_focus = false;

    construct ();

    // actions that should also be available in the find dialog
    m_fetab_actions << m_find_next_action;
    m_fetab_actions << m_find_previous_action;

    setVisible (false);
    setAcceptDrops (true);
  }

  file_editor::~file_editor (void)
  {
    delete m_mru_file_menu;
  }

  // insert global actions, that should also be displayed in the editor window,
  // into the editor's menu and/or toolbar
  void file_editor::insert_global_actions (QList<QAction*> shared_actions)
  {
    // actions/menus that have to be added to the toolbar or the menu
    QAction *open_action = shared_actions.at (OPEN_ACTION);
    QAction *new_action = shared_actions.at (NEW_SCRIPT_ACTION);
    QAction *new_fcn_action = shared_actions.at (NEW_FUNCTION_ACTION);
    m_fileMenu->insertAction (m_mru_file_menu->menuAction (), open_action);
    m_fileMenu->insertAction (open_action, new_fcn_action);
    m_fileMenu->insertAction (new_fcn_action, new_action);
    m_tool_bar->insertAction (m_popdown_mru_action, open_action);
    m_tool_bar->insertAction (open_action, new_action);

    // actions that are additionally enabled/disabled later by the editor
    // undo
    m_undo_action = shared_actions.at (UNDO_ACTION);
    m_tool_bar->insertAction (m_redo_action,m_undo_action);
    m_edit_menu->insertAction (m_redo_action,m_undo_action);
    // copy
    m_copy_action = shared_actions.at (COPY_ACTION);
    m_tool_bar->insertAction (m_cut_action,m_copy_action);
    m_edit_menu->insertAction (m_cut_action,m_copy_action);
    // select all
    m_selectall_action = shared_actions.at (SELECTALL_ACTION);
    m_edit_menu->insertAction (m_find_action,m_selectall_action);
    m_edit_menu->insertSeparator (m_find_action);
    // paste
    m_paste_action = shared_actions.at (PASTE_ACTION);
    m_tool_bar->insertAction (m_find_action,m_paste_action);
    m_edit_menu->insertAction (m_selectall_action,m_paste_action);
    m_edit_menu->insertSeparator (m_selectall_action);
    // find files
    m_find_files_action = shared_actions.at (FIND_FILES_ACTION);
    m_edit_menu->insertAction (m_find_action, m_find_files_action);
  }

  void file_editor::handle_enter_debug_mode (void)
  {
    m_run_action->setEnabled (false);
    m_run_action->setShortcut (QKeySequence ());
  }

  void file_editor::handle_exit_debug_mode (void)
  {
    m_run_action->setEnabled (true);
    shortcut_manager::set_shortcut (m_run_action, "editor_run:run_file");
  }

  void file_editor::check_actions (void)
  {
    bool have_tabs = m_tab_widget->count () > 0;

    m_edit_cmd_menu->setEnabled (have_tabs);
    m_edit_fmt_menu->setEnabled (have_tabs);
    m_edit_nav_menu->setEnabled (have_tabs);

    m_comment_selection_action->setEnabled (have_tabs);
    m_uncomment_selection_action->setEnabled (have_tabs);
    m_comment_var_selection_action->setEnabled (have_tabs);
    m_indent_selection_action->setEnabled (have_tabs);
    m_unindent_selection_action->setEnabled (have_tabs);
    m_smart_indent_line_or_selection_action->setEnabled (have_tabs);

    m_context_help_action->setEnabled (have_tabs);
    m_context_doc_action->setEnabled (have_tabs);

    m_view_editor_menu->setEnabled (have_tabs);
    m_zoom_in_action->setEnabled (have_tabs);
    m_zoom_out_action->setEnabled (have_tabs);
    m_zoom_normal_action->setEnabled (have_tabs);

    m_find_action->setEnabled (have_tabs);
    m_find_next_action->setEnabled (have_tabs);
    m_find_previous_action->setEnabled (have_tabs);
    m_print_action->setEnabled (have_tabs);
    m_run_action->setEnabled (have_tabs);

    m_edit_function_action->setEnabled (have_tabs);
    m_save_action->setEnabled (have_tabs);
    m_save_as_action->setEnabled (have_tabs);
    m_close_action->setEnabled (have_tabs);
    m_close_all_action->setEnabled (have_tabs);
    m_close_others_action->setEnabled (have_tabs && m_tab_widget->count () > 1);
  }

  // empty_script determines whether we have to create an empty script
  // 1. At startup, when the editor has to be (really) visible
  //    (Here we can not use the visibility changed signal)
  // 2. When the editor becomes visible when octave is running
  void file_editor::empty_script (bool startup, bool visible)
  {
    QSettings *settings = resource_manager::get_settings ();
    if (settings->value ("useCustomFileEditor",false).toBool ())
      return;  // do not open an empty script in the external editor

    bool real_visible;

    if (startup)
      real_visible = isVisible ();
    else
      real_visible = visible;

    if (! real_visible || m_tab_widget->count () > 0)
      return;

    if (startup && ! isFloating ())
      {
        // check is editor is really visible or hidden between tabbed widgets
        QList<QTabBar *> tab_list = main_win ()->findChildren<QTabBar *>();

        bool in_tab = false;
        int i = 0;
        while ((i < tab_list.count ()) && (! in_tab))
          {
            QTabBar *tab = tab_list.at (i);
            i++;

            int j = 0;
            while ((j < tab->count ()) && (! in_tab))
              {
                // check all tabs for the editor
                if (tab->tabText (j) == windowTitle ())
                  {
                    // editor is in this tab widget
                    in_tab = true;
                    int top = tab->currentIndex ();
                    if (top > -1 && tab->tabText (top) == windowTitle ())
                      real_visible = true;  // and is the current tab
                    else
                      return; // not current tab -> not visible
                  }
                j++;
              }
          }
      }

    request_new_file ("");
  }

  void file_editor::restore_session (QSettings *settings)
  {
    //restore previous session
    if (! settings->value ("editor/restoreSession", true).toBool ())
      return;

    // get the data from the settings file
    QStringList sessionFileNames
      = settings->value ("editor/savedSessionTabs",
                         QStringList ()).toStringList ();

    QStringList session_encodings
      = settings->value ("editor/saved_session_encodings",
                         QStringList ()).toStringList ();

    QStringList session_index
      = settings->value ("editor/saved_session_tab_index",
                         QStringList ()).toStringList ();

    // fill a list of the struct and sort it (depending on index)
    QList<session_data> s_data;

    bool do_encoding = (session_encodings.count () == sessionFileNames.count ());
    bool do_index = (session_index.count () == sessionFileNames.count ());

    for (int n = 0; n < sessionFileNames.count (); ++n)
      {
        QFileInfo file = QFileInfo (sessionFileNames.at (n));
        if (! file.exists ())
          continue;

        session_data item = { 0, sessionFileNames.at (n), QString ()};
        if (do_index)
          item.index = session_index.at (n).toInt ();
        if (do_encoding)
          item.encoding = session_encodings.at (n);

        s_data << item;
      }

    qSort (s_data);

    // finally open the file with the desired encoding in the desired order
    for (int n = 0; n < s_data.count (); ++n)
      request_open_file (s_data.at (n).file_name, s_data.at (n).encoding);
  }

  void file_editor::focus (void)
  {
    if (m_no_focus)
      return;  // No focus for the editor if external open/close request

    octave_dock_widget::focus ();

    // set focus to current tab
    QWidget *fileEditorTab = m_tab_widget->currentWidget ();
    if (fileEditorTab)
      emit fetab_set_focus (fileEditorTab);
  }

  void file_editor::set_focus (QWidget *fet)
  {
    octave_dock_widget::focus ();

    // set focus to desired tab
    if (fet)
      m_tab_widget->setCurrentWidget (fet);
  }

  // function enabling/disabling the menu accelerators depending on the
  // focus of the editor
  void file_editor::enable_menu_shortcuts (bool enable)
  {
    QHash<QMenu*, QStringList>::const_iterator i = m_hash_menu_text.constBegin ();

    while (i != m_hash_menu_text.constEnd ())
      {
        i.key ()->setTitle (i.value ().at (! enable));
        ++i;
      }

    // when editor loses focus, enable the actions, which are always active
    // in the main window due to missing info on selected text and undo actions
    if (! enable && m_copy_action && m_undo_action)
      {
        m_copy_action->setEnabled (true);
        m_undo_action->setEnabled (true);
      }
  }

  bool file_editor::check_closing (void)
  {
    // When the application or the editor is closing and the user wants to close
    // all files in the latter case all editor tabs are checked whether
    // they need to be saved. During these ckecked the tabs are not closed
    // since the user might cancel closing octave during one of these saving
    // dialogs. Therefore, saving the session for restoring at next start
    // is not done before the application is definitely closing

    // Have all file editor tabs signal what their filenames are.
    m_editor_tab_map.clear ();
    emit fetab_file_name_query (nullptr);

    // Save all tabs with confirmation.
    file_editor_tab::reset_cancel ();
    emit fetab_check_modified_file ();

    // If there was a cancellation, make the already saved/discarded tabs
    // recovering from the exit by removing the read-only state and by
    // recovering the debugger breakpoints. Finally return false in order to
    // cancel closing the application or the editor
    if (file_editor_tab::was_cancelled ())
      {
        emit fetab_recover_from_exit ();
        return false;
      }

    // Here, the application or the editor will be closed -> store the session

    // Save open files for restoring in next session; this only is possible
    QSettings *settings = resource_manager::get_settings ();

    // save filenames (even if last session will not be restored next time)
    // together with encoding and the tab index
    QStringList fetFileNames;
    QStringList fet_encodings;
    QStringList fet_index;

    // save all open tabs before they are definitely closed
    for (auto p = m_editor_tab_map.cbegin ();
         p != m_editor_tab_map.cend (); p++)
      {
        QString file_name = p->first;   // get file name of tab
        if (! file_name.isEmpty ())      // do not append unnamed files
          {
            fetFileNames.append (file_name);
            fet_encodings.append (m_editor_tab_map[file_name].encoding);
            QString index;
            fet_index.append (index.setNum
                              (m_tab_widget->indexOf (m_editor_tab_map[file_name].fet_ID)));
          }
      }

    settings->setValue ("editor/savedSessionTabs", fetFileNames);
    settings->setValue ("editor/saved_session_encodings", fet_encodings);
    settings->setValue ("editor/saved_session_tab_index", fet_index);
    settings->sync ();

    // Finally close all the tabs and return indication that we can exit
    // the application or close the editor.
    // Closing and deleting the tabs makes the editor visible. In case it was
    // hidden before, this state has to be restored afterwards
    bool vis = isVisible ();

    for (int i = m_tab_widget->count () - 1; i >= 0; i--)
      {
        // backwards loop since m_tab_widget->count () changes during the loop
        delete m_tab_widget->widget (i);
        m_tab_widget->removeTab (i);
      }

    setVisible (vis);

    return true;
  }

  void file_editor::request_new_file (const QString& commands)
  {
    // Custom editor? If yes, we can only call the editor without passing
    // some initial contents and even without being sure a new file is opened
    if (call_custom_editor ())
      return;

    // New file isn't a file_editor_tab function since the file
    // editor tab has yet to be created and there is no object to
    // pass a signal to.  Hence, functionality is here.

    file_editor_tab *fileEditorTab = new file_editor_tab (m_ced);
    if (fileEditorTab)
      {
        add_file_editor_tab (fileEditorTab, "");  // new tab with empty title
        fileEditorTab->new_file (commands);       // title is updated here
        focus ();                                 // focus editor and new tab
      }
  }

  void file_editor::request_close_file (bool)
  {
    file_editor_tab *editor_tab
      = static_cast<file_editor_tab *> (m_tab_widget->currentWidget ());
    editor_tab->conditional_close ();
  }

  void file_editor::request_close_all_files (bool)
  {
    file_editor_tab *editor_tab;

    // loop over all tabs starting from last one otherwise deletion changes index
    for (int index = m_tab_widget->count ()-1; index >= 0; index--)
      {
        editor_tab = static_cast<file_editor_tab *> (m_tab_widget->widget (index));
        editor_tab->conditional_close ();
      }
  }

  void file_editor::request_close_other_files (bool)
  {
    file_editor_tab *editor_tab;
    QWidget *tabID = m_tab_widget->currentWidget ();

    // loop over all tabs starting from last one otherwise deletion changes index
    for (int index = m_tab_widget->count ()-1; index >= 0; index--)
      {
        if (tabID != m_tab_widget->widget (index))
          {
            editor_tab
              = static_cast<file_editor_tab *> (m_tab_widget->widget (index));
            editor_tab->conditional_close ();
          }
      }
  }

  // open a file from the mru list
  void file_editor::request_mru_open_file (QAction *action)
  {
    if (action)
      {
        request_open_file (action->data ().toStringList ().at (0),
                           action->data ().toStringList ().at (1));
      }
  }

  void file_editor::request_print_file (bool)
  {
    emit fetab_print_file (m_tab_widget->currentWidget ());
  }

  void file_editor::request_redo (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_REDO);
  }

  void file_editor::request_cut (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_CUT);
  }

  void file_editor::request_context_help (bool)
  {
    emit fetab_context_help (m_tab_widget->currentWidget (), false);
  }

  void file_editor::request_context_doc (bool)
  {
    emit fetab_context_help (m_tab_widget->currentWidget (), true);
  }

  void file_editor::request_context_edit (bool)
  {
    emit fetab_context_edit (m_tab_widget->currentWidget ());
  }

  void file_editor::request_save_file (bool)
  {
    emit fetab_save_file (m_tab_widget->currentWidget ());
  }

  void file_editor::request_save_file_as (bool)
  {
    emit fetab_save_file_as (m_tab_widget->currentWidget ());
  }

  void file_editor::request_run_file (bool)
  {
    emit fetab_run_file (m_tab_widget->currentWidget ());
  }

  void file_editor::request_context_run (bool)
  {
    emit fetab_context_run (m_tab_widget->currentWidget ());
  }

  void file_editor::request_toggle_bookmark (bool)
  {
    emit fetab_toggle_bookmark (m_tab_widget->currentWidget ());
  }

  void file_editor::request_next_bookmark (bool)
  {
    emit fetab_next_bookmark (m_tab_widget->currentWidget ());
  }

  void file_editor::request_previous_bookmark (bool)
  {
    emit fetab_previous_bookmark (m_tab_widget->currentWidget ());
  }

  void file_editor::request_remove_bookmark (bool)
  {
    emit fetab_remove_bookmark (m_tab_widget->currentWidget ());
  }

  void file_editor::request_move_match_brace (bool)
  {
    emit fetab_move_match_brace (m_tab_widget->currentWidget (), false);
  }

  void file_editor::request_sel_match_brace (bool)
  {
    emit fetab_move_match_brace (m_tab_widget->currentWidget (), true);
  }

  // FIXME: What should this do with conditional breakpoints?
  void file_editor::request_toggle_breakpoint (bool)
  {
    emit fetab_toggle_breakpoint (m_tab_widget->currentWidget ());
  }

  void file_editor::request_next_breakpoint (bool)
  {
    emit fetab_next_breakpoint (m_tab_widget->currentWidget ());
  }

  void file_editor::request_previous_breakpoint (bool)
  {
    emit fetab_previous_breakpoint (m_tab_widget->currentWidget ());
  }

  void file_editor::request_remove_breakpoint (bool)
  {
    emit fetab_remove_all_breakpoints (m_tab_widget->currentWidget ());
  }

  // slots for Edit->Commands actions
  void file_editor::request_delete_start_word (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_DELWORDLEFT);
  }

  void file_editor::request_delete_end_word (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_DELWORDRIGHT);
  }

  void file_editor::request_delete_start_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_DELLINELEFT);
  }

  void file_editor::request_delete_end_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_DELLINERIGHT);
  }

  void file_editor::request_delete_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_LINEDELETE);
  }

  void file_editor::request_copy_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_LINECOPY);
  }

  void file_editor::request_cut_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_LINECUT);
  }

  void file_editor::request_duplicate_selection (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_SELECTIONDUPLICATE);
  }

  void file_editor::request_transpose_line (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_LINETRANSPOSE);
  }

  void file_editor::request_comment_selected_text (bool)
  {
    emit fetab_comment_selected_text (m_tab_widget->currentWidget (), false);
  }

  void file_editor::request_uncomment_selected_text (bool)
  {
    emit fetab_uncomment_selected_text (m_tab_widget->currentWidget ());
  }

  void file_editor::request_comment_var_selected_text (bool)
  {
    emit fetab_comment_selected_text (m_tab_widget->currentWidget (), true);
  }

  // slots for Edit->Format actions
  void file_editor::request_upper_case (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_UPPERCASE);
  }

  void file_editor::request_lower_case (bool)
  {
    emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                  QsciScintillaBase::SCI_LOWERCASE);
  }

  void file_editor::request_indent_selected_text (bool)
  {
    emit fetab_indent_selected_text (m_tab_widget->currentWidget ());
  }

  void file_editor::request_unindent_selected_text (bool)
  {
    emit fetab_unindent_selected_text (m_tab_widget->currentWidget ());
  }

  void file_editor::request_smart_indent_line_or_selected_text ()
  {
    emit fetab_smart_indent_line_or_selected_text (m_tab_widget->currentWidget ());
  }

  void file_editor::request_conv_eol_windows (bool)
  {
    emit fetab_convert_eol (m_tab_widget->currentWidget (),
                            QsciScintilla::EolWindows);
  }
  void
  file_editor::request_conv_eol_unix (bool)
  {
    emit fetab_convert_eol (m_tab_widget->currentWidget (),
                            QsciScintilla::EolUnix);
  }

  void file_editor::request_conv_eol_mac (bool)
  {
    emit fetab_convert_eol (m_tab_widget->currentWidget (),
                            QsciScintilla::EolMac);
  }

  void file_editor::request_find (bool)
  {
    emit fetab_find (m_tab_widget->currentWidget (), m_fetab_actions);
  }

  void file_editor::request_find_next (bool)
  {
    emit fetab_find_next (m_tab_widget->currentWidget ());
  }

  void file_editor::request_find_previous (bool)
  {
    emit fetab_find_previous (m_tab_widget->currentWidget ());
  }

  void file_editor::request_goto_line (bool)
  {
    emit fetab_goto_line (m_tab_widget->currentWidget ());
  }

  void file_editor::request_completion (bool)
  {
    emit fetab_completion (m_tab_widget->currentWidget ());
  }

  void file_editor::handle_file_name_changed (const QString& fname,
                                              const QString& tip)
  {
    QObject *fileEditorTab = sender ();
    if (fileEditorTab)
      {
        for (int i = 0; i < m_tab_widget->count (); i++)
          {
            if (m_tab_widget->widget (i) == fileEditorTab)
              {
                m_tab_widget->setTabText (i, fname);
                m_tab_widget->setTabToolTip (i, tip);
              }
          }
      }
  }

  void file_editor::handle_tab_close_request (int index)
  {
    file_editor_tab *editor_tab
      = static_cast<file_editor_tab *> (m_tab_widget->widget (index));
    editor_tab->conditional_close ();
  }

  void
  file_editor::handle_tab_remove_request (void)
  {
    QObject *fileEditorTab = sender ();
    if (fileEditorTab)
      {
        for (int i = 0; i < m_tab_widget->count (); i++)
          {
            if (m_tab_widget->widget (i) == fileEditorTab)
              {
                m_tab_widget->removeTab (i);
                // Deleting sender is dodgy, but works because the signal
                // is the last item in the sender's routines.
                delete fileEditorTab;
                break;
              }
          }
      }
    check_actions ();

    focus ();     // focus stays in editor when tab is closed

  }

  void file_editor::handle_add_filename_to_list (const QString& fileName,
                                                 const QString& encoding, QWidget *ID)
  {
    // Should we allow multiple tabs for a single file?
    m_editor_tab_map[fileName].fet_ID = ID;
    m_editor_tab_map[fileName].encoding = encoding;
  }

  // context menu of edit area
  void file_editor::active_tab_changed (int index)
  {
    emit fetab_change_request (m_tab_widget->widget (index));
    focus ();
  }

  void file_editor::handle_editor_state_changed (bool copy_available,
                                                 bool is_octave_file)
  {
    // In case there is some scenario where traffic could be coming from
    // all the file editor tabs, just process info from the current active tab.
    if (sender () == m_tab_widget->currentWidget ())
      {
        if (m_copy_action)
          m_copy_action->setEnabled (copy_available);
        m_cut_action->setEnabled (copy_available);
        m_run_selection_action->setEnabled (copy_available);
        m_run_action->setEnabled (is_octave_file);

        setFocusProxy (m_tab_widget->currentWidget ());
      }
  }

  void file_editor::handle_mru_add_file (const QString& file_name,
                                         const QString& encoding)
  {
    int index;
    while ((index = m_mru_files.indexOf (file_name)) >= 0)
      {
        m_mru_files.removeAt (index);
        m_mru_files_encodings.removeAt (index);
      }

    m_mru_files.prepend (file_name);
    m_mru_files_encodings.prepend (encoding);

    mru_menu_update ();
  }

  void file_editor::check_conflict_save (const QString& saveFileName,
                                         bool remove_on_success)
  {
    // Check whether this file is already open in the editor.
    QWidget *tab = find_tab_widget (saveFileName);

    if (tab)
      {
        // Note: to overwrite the contents of some other file editor tab
        // with the same name requires identifying which file editor tab
        // that is (not too difficult) then close that tab.  Of course,
        // that could trigger another dialog box if the file editor tab
        // with the same name has modifications in it.  This could become
        // somewhat confusing to the user.  For now, opt to do nothing.

        // Create a NonModal message about error.
        QMessageBox *msgBox
          = new QMessageBox (QMessageBox::Critical, tr ("Octave Editor"),
                             tr ("File not saved! A file with the selected name\n%1\n"
                                 "is already open in the editor").
                             arg (saveFileName),
                             QMessageBox::Ok, nullptr);

        msgBox->setWindowModality (Qt::NonModal);
        msgBox->setAttribute (Qt::WA_DeleteOnClose);
        msgBox->show ();

        return;
      }

    QObject *saveFileObject = sender ();
    QWidget *saveFileWidget = nullptr;

    for (int i = 0; i < m_tab_widget->count (); i++)
      {
        if (m_tab_widget->widget (i) == saveFileObject)
          {
            saveFileWidget = m_tab_widget->widget (i);
            break;
          }
      }
    if (! saveFileWidget)
      {
        // Create a NonModal message about error.
        QMessageBox *msgBox
          = new QMessageBox (QMessageBox::Critical, tr ("Octave Editor"),
                             tr ("The associated file editor tab has disappeared."),
                             QMessageBox::Ok, nullptr);

        msgBox->setWindowModality (Qt::NonModal);
        msgBox->setAttribute (Qt::WA_DeleteOnClose);
        msgBox->show ();

        return;
      }

    // Can save without conflict, have the file editor tab do so.
    emit fetab_save_file (saveFileWidget, saveFileName, remove_on_success);
  }

  void file_editor::handle_insert_debugger_pointer_request (const QString& file,
                                                            int line)
  {
    request_open_file (file, QString (), line, true); // default encoding
  }

  void file_editor::handle_delete_debugger_pointer_request (const QString& file,
                                                            int line)
  {
    if (! file.isEmpty ())
      {
        // Check whether this file is already open in the editor.
        QWidget *tab = find_tab_widget (file);

        if (tab)
          {
            m_tab_widget->setCurrentWidget (tab);

            if (line > 0)
              emit fetab_delete_debugger_pointer (tab, line);

            emit fetab_set_focus (tab);
          }
      }
  }

  void file_editor::handle_update_breakpoint_marker_request (bool insert,
                                                             const QString& file,
                                                             int line,
                                                             const QString& cond)
  {
    request_open_file (file, QString (), line, false, true, insert, cond);
  }

  void file_editor::handle_edit_file_request (const QString& file)
  {
    request_open_file (file);
  }

  // Slot used for signals indicating that a file was changed/rename or
  // is going to be deleted/renamed
  void file_editor::handle_file_remove (const QString& old_name,
                                        const QString& new_name)
  {
    // Clear old lsit of files to reload
    m_tmp_closed_files.clear ();

    // Check if old name is a file or directory
    QFileInfo old (old_name);
    if (old.isDir ())
      {
        // Call the function which handles directories and return
        handle_dir_remove (old_name, new_name);
        return;
      }

    // Is old file open?
    file_editor_tab *editor_tab
      = static_cast<file_editor_tab *> (find_tab_widget (old_name));

    if (editor_tab)
      {
        // Yes, close it silently
        m_no_focus = true;  // Remember for not focussing editor
        editor_tab->file_has_changed (QString (), true);  // Close the tab
        m_no_focus = false;  // Back to normal

        m_tmp_closed_files << old_name;  // for reloading if error removing

        if (! new_name.isEmpty ())
          m_tmp_closed_files << new_name;  // store new name
        else
          m_tmp_closed_files << ""; // no new name, just removing this file

        // Get and store the related encoding
        for (auto p = m_editor_tab_map.cbegin ();
             p != m_editor_tab_map.cend (); p++)
          {
            if (editor_tab == p->second.fet_ID)
              {
                m_tmp_closed_files << p->second.encoding;
                break;
              }
          }
      }
  }

  // Slot for signal indicating that a file was renamed
  void file_editor::handle_file_renamed (bool load_new)
  {
    m_no_focus = true;  // Remember for not focussing editor
    for (int i = 0; i < m_tmp_closed_files.count (); i = i + 3)
      {
        if (! m_tmp_closed_files.at (i + load_new).isEmpty ())
          request_open_file (m_tmp_closed_files.at (i + load_new),
                             m_tmp_closed_files.at (i+2));
      }
    m_no_focus = false;  // Back to normal focus
  }

  void file_editor::notice_settings (const QSettings *settings)
  {
    int icon_size_settings = settings->value ("toolbar_icon_size",0).toInt ();
    QStyle *st = style ();
    int icon_size = st->pixelMetric (QStyle::PM_ToolBarIconSize);

    if (icon_size_settings == 1)
      icon_size = st->pixelMetric (QStyle::PM_LargeIconSize);
    else if (icon_size_settings == -1)
      icon_size = st->pixelMetric (QStyle::PM_SmallIconSize);

    m_tool_bar->setIconSize (QSize (icon_size,icon_size));

    int tab_width_min = settings->value ("editor/notebook_tab_width_min", 160)
                        .toInt ();
    int tab_width_max = settings->value ("editor/notebook_tab_width_max", 300)
                        .toInt ();

    if (settings->value ("editor/longWindowTitle", false).toBool ())
      {
        QString style_sheet = QString ("QTabBar::tab "
                                       "{min-width: %1px; max-width: %2px;}")
                              .arg (tab_width_min).arg (tab_width_max);
        m_tab_widget->setElideMode (Qt::ElideLeft);
        m_tab_widget->setStyleSheet (style_sheet);
      }
    else
      m_tab_widget->setElideMode (Qt::ElideNone);

    m_tab_widget->setUsesScrollButtons (true);

    bool show_it;
    show_it = settings->value ("editor/showLineNumbers",true).toBool ();
    m_show_linenum_action->setChecked (show_it);
    show_it = settings->value ("editor/show_white_space",false).toBool ();
    m_show_whitespace_action->setChecked (show_it);
    show_it = settings->value ("editor/show_eol_chars",false).toBool ();
    m_show_eol_action->setChecked (show_it);
    show_it = settings->value ("editor/show_indent_guides",false).toBool ();
    m_show_indguide_action->setChecked (show_it);
    show_it = settings->value ("editor/long_line_marker",true).toBool ();
    m_show_longline_action->setChecked (show_it);

    show_it = settings->value ("editor/show_toolbar",true).toBool ();
    m_show_toolbar_action->setChecked (show_it);
    m_tool_bar->setVisible (show_it);
    show_it = settings->value ("editor/show_edit_status_bar",true).toBool ();
    m_show_statusbar_action->setChecked (show_it);
    show_it = settings->value ("editor/show_hscroll_bar",true).toBool ();
    m_show_hscrollbar_action->setChecked (show_it);

    set_shortcuts ();

    // Relay signal to file editor tabs.
    emit fetab_settings_changed (settings);
  }

  void file_editor::set_shortcuts (void)
  {
    // Shortcuts also available in the main window, as well as the realted
    // ahotcuts, are defined in main_window and added to the editor

    // File menu
    shortcut_manager::set_shortcut (m_edit_function_action, "editor_file:edit_function");
    shortcut_manager::set_shortcut (m_save_action, "editor_file:save");
    shortcut_manager::set_shortcut (m_save_as_action, "editor_file:save_as");
    shortcut_manager::set_shortcut (m_close_action, "editor_file:close");
    shortcut_manager::set_shortcut (m_close_all_action, "editor_file:close_all");
    shortcut_manager::set_shortcut (m_close_others_action, "editor_file:close_other");
    shortcut_manager::set_shortcut (m_print_action, "editor_file:print");

    // Edit menu
    shortcut_manager::set_shortcut (m_redo_action, "editor_edit:redo");
    shortcut_manager::set_shortcut (m_cut_action, "editor_edit:cut");
    shortcut_manager::set_shortcut (m_find_action, "editor_edit:find_replace");
    shortcut_manager::set_shortcut (m_find_next_action, "editor_edit:find_next");
    shortcut_manager::set_shortcut (m_find_previous_action, "editor_edit:find_previous");

    shortcut_manager::set_shortcut (m_delete_start_word_action, "editor_edit:delete_start_word");
    shortcut_manager::set_shortcut (m_delete_end_word_action, "editor_edit:delete_end_word");
    shortcut_manager::set_shortcut (m_delete_start_line_action, "editor_edit:delete_start_line");
    shortcut_manager::set_shortcut (m_delete_end_line_action, "editor_edit:delete_end_line");
    shortcut_manager::set_shortcut (m_delete_line_action, "editor_edit:delete_line");
    shortcut_manager::set_shortcut (m_copy_line_action, "editor_edit:copy_line");
    shortcut_manager::set_shortcut (m_cut_line_action, "editor_edit:cut_line");
    shortcut_manager::set_shortcut (m_duplicate_selection_action, "editor_edit:duplicate_selection");
    shortcut_manager::set_shortcut (m_transpose_line_action, "editor_edit:transpose_line");
    shortcut_manager::set_shortcut (m_comment_selection_action, "editor_edit:comment_selection");
    shortcut_manager::set_shortcut (m_uncomment_selection_action, "editor_edit:uncomment_selection");
    shortcut_manager::set_shortcut (m_comment_var_selection_action, "editor_edit:comment_var_selection");

    shortcut_manager::set_shortcut (m_upper_case_action, "editor_edit:upper_case");
    shortcut_manager::set_shortcut (m_lower_case_action, "editor_edit:lower_case");
    shortcut_manager::set_shortcut (m_indent_selection_action, "editor_edit:indent_selection");
    shortcut_manager::set_shortcut (m_unindent_selection_action, "editor_edit:unindent_selection");
    shortcut_manager::set_shortcut (m_smart_indent_line_or_selection_action, "editor_edit:smart_indent_line_or_selection");
    shortcut_manager::set_shortcut (m_completion_action, "editor_edit:completion_list");
    shortcut_manager::set_shortcut (m_goto_line_action, "editor_edit:goto_line");
    shortcut_manager::set_shortcut (m_move_to_matching_brace, "editor_edit:move_to_brace");
    shortcut_manager::set_shortcut (m_sel_to_matching_brace, "editor_edit:select_to_brace");
    shortcut_manager::set_shortcut (m_toggle_bookmark_action, "editor_edit:toggle_bookmark");
    shortcut_manager::set_shortcut (m_next_bookmark_action, "editor_edit:next_bookmark");
    shortcut_manager::set_shortcut (m_previous_bookmark_action, "editor_edit:previous_bookmark");
    shortcut_manager::set_shortcut (m_remove_bookmark_action, "editor_edit:remove_bookmark");
    shortcut_manager::set_shortcut (m_preferences_action, "editor_edit:preferences");
    shortcut_manager::set_shortcut (m_styles_preferences_action, "editor_edit:styles_preferences");

    shortcut_manager::set_shortcut (m_conv_eol_windows_action, "editor_edit:conv_eol_winows");
    shortcut_manager::set_shortcut (m_conv_eol_unix_action,    "editor_edit:conv_eol_unix");
    shortcut_manager::set_shortcut (m_conv_eol_mac_action,     "editor_edit:conv_eol_mac");

    // View menu
    shortcut_manager::set_shortcut (m_show_linenum_action, "editor_view:show_line_numbers");
    shortcut_manager::set_shortcut (m_show_whitespace_action, "editor_view:show_white_spaces");
    shortcut_manager::set_shortcut (m_show_eol_action, "editor_view:show_eol_chars");
    shortcut_manager::set_shortcut (m_show_indguide_action, "editor_view:show_ind_guides");
    shortcut_manager::set_shortcut (m_show_longline_action, "editor_view:show_long_line");
    shortcut_manager::set_shortcut (m_show_toolbar_action, "editor_view:show_toolbar");
    shortcut_manager::set_shortcut (m_show_statusbar_action, "editor_view:show_statusbar");
    shortcut_manager::set_shortcut (m_show_hscrollbar_action, "editor_view:show_hscrollbar");
    shortcut_manager::set_shortcut (m_zoom_in_action, "editor_view:zoom_in");
    shortcut_manager::set_shortcut (m_zoom_out_action, "editor_view:zoom_out");
    shortcut_manager::set_shortcut (m_zoom_normal_action, "editor_view:zoom_normal");

    // Debug menu
    shortcut_manager::set_shortcut (m_toggle_breakpoint_action, "editor_debug:toggle_breakpoint");
    shortcut_manager::set_shortcut (m_next_breakpoint_action, "editor_debug:next_breakpoint");
    shortcut_manager::set_shortcut (m_previous_breakpoint_action, "editor_debug:previous_breakpoint");
    shortcut_manager::set_shortcut (m_remove_all_breakpoints_action, "editor_debug:remove_breakpoints");

    // Run menu
    shortcut_manager::set_shortcut (m_run_action, "editor_run:run_file");
    shortcut_manager::set_shortcut (m_run_selection_action, "editor_run:run_selection");

    // Help menu
    shortcut_manager::set_shortcut (m_context_help_action, "editor_help:help_keyword");
    shortcut_manager::set_shortcut (m_context_doc_action,  "editor_help:doc_keyword");

    // Tab navigation without menu entries
    shortcut_manager::set_shortcut (m_switch_left_tab_action, "editor_tabs:switch_left_tab");
    shortcut_manager::set_shortcut (m_switch_right_tab_action, "editor_tabs:switch_right_tab");
    shortcut_manager::set_shortcut (m_move_tab_left_action, "editor_tabs:move_tab_left");
    shortcut_manager::set_shortcut (m_move_tab_right_action, "editor_tabs:move_tab_right");

  }

  // This slot is a reimplementation of the virtual slot in octave_dock_widget.
  // We need this for creating an empty script when the editor has no open files
  // and is made visible
  void file_editor::handle_visibility (bool visible)
  {
    if (m_closed && visible)
      {
        m_closed = false;
        QSettings *settings = resource_manager::get_settings ();
        restore_session (settings);
      }

    empty_script (false, visible);

    if (visible && ! isFloating ())
      focus ();

  }

  void file_editor::update_octave_directory (const QString& dir)
  {
    m_ced = dir;
    emit fetab_set_directory (m_ced);  // for save dialog
  }

  void file_editor::copyClipboard (void)
  {
    if (editor_tab_has_focus ())
      emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                    QsciScintillaBase::SCI_COPY);
  }

  void file_editor::pasteClipboard (void)
  {
    if (editor_tab_has_focus ())
      emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                    QsciScintillaBase::SCI_PASTE);
  }

  void file_editor::selectAll (void)
  {
    if (editor_tab_has_focus ())
      emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                    QsciScintillaBase::SCI_SELECTALL);
  }

  void file_editor::do_undo (void)
  {
    if (editor_tab_has_focus ())
      emit fetab_scintilla_command (m_tab_widget->currentWidget (),
                                    QsciScintillaBase::SCI_UNDO);
  }

  // Open a file, if not already open, and mark the current execution location
  // and/or a breakpoint with condition cond.
  void file_editor::request_open_file (const QString& openFileName,
                                       const QString& encoding,
                                       int line, bool debug_pointer,
                                       bool breakpoint_marker, bool insert,
                                       const QString& cond)
  {
    if (call_custom_editor (openFileName, line))
      return;   // custom editor called

    if (openFileName.isEmpty ())
      {
        // This happens if edit is calles without an argument
        // Open eitor with empty edit area instead (as new file would do)
        request_new_file ("");
      }
    else
      {
        // Check whether this file is already open in the editor.
        QWidget *tab = find_tab_widget (openFileName);

        if (tab)
          {
            m_tab_widget->setCurrentWidget (tab);

            if (line > 0)
              {
                if (insert)
                  emit fetab_goto_line (tab, line);

                if (debug_pointer)
                  emit fetab_insert_debugger_pointer (tab, line);

                if (breakpoint_marker)
                  emit fetab_do_breakpoint_marker (insert, tab, line, cond);
              }

            if (! ((breakpoint_marker || debug_pointer) && is_editor_console_tabbed ()))
              {
                emit fetab_set_focus (tab);
                focus ();
              }
          }
        else
          {
            file_editor_tab *fileEditorTab = nullptr;
            // Reuse <unnamed> tab if it hasn't yet been modified.
            bool reusing = false;
            tab = find_tab_widget ("");
            if (tab)
              {
                fileEditorTab = static_cast<file_editor_tab *>(tab);
                if (fileEditorTab->qsci_edit_area ()->isModified ())
                  fileEditorTab = nullptr;
                else
                  reusing = true;
              }

            // If <unnamed> was absent or modified, create a new tab.
            if (! fileEditorTab)
              fileEditorTab = new file_editor_tab ();

            if (fileEditorTab)
              {
                fileEditorTab->set_encoding (encoding);
                QString result = fileEditorTab->load_file (openFileName);
                if (result == "")
                  {
                    // Supply empty title then have the file_editor_tab update
                    // with full or short name.
                    if (! reusing)
                      add_file_editor_tab (fileEditorTab, "");
                    fileEditorTab->update_window_title (false);
                    // file already loaded, add file to mru list here
                    QFileInfo file_info = QFileInfo (openFileName);
                    handle_mru_add_file (file_info.canonicalFilePath (),
                                         encoding);

                    if (line > 0)
                      {
                        if (insert)
                          emit fetab_goto_line (fileEditorTab, line);

                        if (debug_pointer)
                          emit fetab_insert_debugger_pointer (fileEditorTab,
                                                              line);
                        if (breakpoint_marker)
                          emit fetab_do_breakpoint_marker (insert, fileEditorTab,
                                                           line, cond);
                      }
                  }
                else
                  {
                    delete fileEditorTab;

                    if (QFile::exists (openFileName))
                      {
                        // File not readable:
                        // create a NonModal message about error.
                        QMessageBox *msgBox
                          = new QMessageBox (QMessageBox::Critical,
                                             tr ("Octave Editor"),
                                             tr ("Could not open file\n%1\nfor read: %2.").
                                             arg (openFileName).arg (result),
                                             QMessageBox::Ok, this);

                        msgBox->setWindowModality (Qt::NonModal);
                        msgBox->setAttribute (Qt::WA_DeleteOnClose);
                        msgBox->show ();
                      }
                    else
                      {
                        // File does not exist, should it be created?
                        bool create_file = true;
                        QMessageBox *msgBox;
                        QSettings *settings = resource_manager::get_settings ();

                        if (! settings->value ("editor/create_new_file", false).toBool ())
                          {
                            msgBox = new QMessageBox (QMessageBox::Question,
                                                      tr ("Octave Editor"),
                                                      tr ("File\n%1\ndoes not exist. "
                                                          "Do you want to create it?").arg (openFileName),
                                                      QMessageBox::NoButton,nullptr);
                            QPushButton *create_button =
                              msgBox->addButton (tr ("Create"), QMessageBox::YesRole);
                            msgBox->addButton (tr ("Cancel"), QMessageBox::RejectRole);
                            msgBox->setDefaultButton (create_button);
                            msgBox->exec ();

                            QAbstractButton *clicked_button = msgBox->clickedButton ();
                            if (clicked_button != create_button)
                              create_file = false;

                            delete msgBox;
                          }

                        if (create_file)
                          {
                            // create the file and call the editor again
                            QFile file (openFileName);
                            if (! file.open (QIODevice::WriteOnly))
                              {
                                // error opening the file
                                msgBox = new QMessageBox (QMessageBox::Critical,
                                                          tr ("Octave Editor"),
                                                          tr ("Could not open file\n%1\nfor write: %2.").
                                                          arg (openFileName).arg (file.errorString ()),
                                                          QMessageBox::Ok, this);

                                msgBox->setWindowModality (Qt::NonModal);
                                msgBox->setAttribute (Qt::WA_DeleteOnClose);
                                msgBox->show ();
                              }
                            else
                              {
                                file.close ();
                                request_open_file (openFileName);
                              }
                          }
                      }
                  }
              }

            if (! ((breakpoint_marker || debug_pointer) && is_editor_console_tabbed ()))
              {
                // really show editor and the current editor tab
                focus ();
                emit file_loaded_signal ();
              }
          }
      }
  }

  void file_editor::request_preferences (bool)
  {
    emit request_settings_dialog ("editor");
  }

  void file_editor::request_styles_preferences (bool)
  {
    emit request_settings_dialog ("editor_styles");
  }

  void file_editor::show_line_numbers (bool)
  {
    toggle_preference ("editor/showLineNumbers",true);
  }

  void file_editor::show_white_space (bool)
  {
    toggle_preference ("editor/show_white_space",false);
  }

  void file_editor::show_eol_chars (bool)
  {
    toggle_preference ("editor/show_eol_chars",false);
  }

  void file_editor::show_indent_guides (bool)
  {
    toggle_preference ("editor/show_indent_guides",false);
  }

  void file_editor::show_long_line (bool)
  {
    toggle_preference ("editor/long_line_marker",true);
  }

  void file_editor::show_toolbar (bool)
  {
    toggle_preference ("editor/show_toolbar",true);
  }

  void file_editor::show_statusbar (bool)
  {
    toggle_preference ("editor/show_edit_status_bar",true);
  }

  void file_editor::show_hscrollbar (bool)
  {
    toggle_preference ("editor/show_hscroll_bar",true);
  }

  void file_editor::zoom_in (bool)
  {
    emit fetab_zoom_in (m_tab_widget->currentWidget ());
  }

  void file_editor::zoom_out (bool)
  {
    emit fetab_zoom_out (m_tab_widget->currentWidget ());
  }

  void file_editor::zoom_normal (bool)
  {
    emit fetab_zoom_normal (m_tab_widget->currentWidget ());
  }

  void file_editor::create_context_menu (QMenu *menu)
  {
    // remove all standard actions from scintilla
    QList<QAction *> all_actions = menu->actions ();
    QAction *a;

    foreach (a, all_actions)
      menu->removeAction (a);

    // add editor's actions with icons and customized shortcuts
    menu->addAction (m_undo_action);
    menu->addAction (m_redo_action);
    menu->addSeparator ();
    menu->addAction (m_cut_action);
    menu->addAction (m_copy_action);
    menu->addAction (m_paste_action);
    menu->addSeparator ();
    menu->addAction (m_selectall_action);
    menu->addSeparator ();
    menu->addAction (m_find_files_action);
    menu->addAction (m_find_action);
    menu->addAction (m_find_next_action);
    menu->addAction (m_find_previous_action);
    menu->addSeparator ();
    menu->addMenu (m_edit_cmd_menu);
    menu->addMenu (m_edit_fmt_menu);
    menu->addMenu (m_edit_nav_menu);
    menu->addSeparator ();
    menu->addAction (m_run_selection_action);
  }

  void file_editor::edit_status_update (bool undo, bool redo)
  {
    if (m_undo_action)
      m_undo_action->setEnabled (undo);
    m_redo_action->setEnabled (redo);
  }

  // handler for the close event
  void file_editor::closeEvent (QCloseEvent *e)
  {
    QSettings *settings = resource_manager::get_settings ();
    if (settings->value ("editor/hiding_closes_files",false).toBool ())
      {
        if (check_closing ())
          {
            // all tabs are closed without cancelling,
            // store closing state for restoring session when shown again
            m_closed = true;
            e->accept ();
          }
        else
          e->ignore ();
      }
    else
      e->accept ();
  }

  void file_editor::dragEnterEvent (QDragEnterEvent *e)
  {
    if (e->mimeData ()->hasUrls ())
      {
        e->acceptProposedAction ();
      }
  }

  void file_editor::dropEvent (QDropEvent *e)
  {
    if (e->mimeData ()->hasUrls ())
      {
        foreach (QUrl url, e->mimeData ()->urls ())
          request_open_file (url.toLocalFile ());
      }
  }

  bool file_editor::is_editor_console_tabbed (void)
  {
    main_window *w = static_cast<main_window *>(main_win ());
    QList<QDockWidget *> w_list = w->tabifiedDockWidgets (this);
    QDockWidget *console =
      static_cast<QDockWidget *> (w->get_dock_widget_list ().at (0));

    for (int i = 0; i < w_list.count (); i++)
      {
        if (w_list.at (i) == console)
          return true;
      }

    return false;
  }

  void file_editor::construct (void)
  {
    QWidget *editor_widget = new QWidget (this);

    // FIXME: what was the intended purpose of this unused variable?
    // QStyle *editor_style = QApplication::style ();

    // Menu bar: do not set it native, required in MacOS and Ubuntu Unity (Qt5)
    // for a visible menu bar in the editor widget. This property is ignored
    // on other platforms.
    m_menu_bar = new QMenuBar (editor_widget);
    m_menu_bar->setNativeMenuBar (false);

    m_tool_bar = new QToolBar (editor_widget);
    m_tool_bar->setMovable (true);

    m_tab_widget = new file_editor_tab_widget (editor_widget);

    // the mru-list and an empty array of actions
    QSettings *settings = resource_manager::get_settings ();
    m_mru_files = settings->value ("editor/mru_file_list").toStringList ();
    m_mru_files_encodings = settings->value ("editor/mru_file_encodings")
                            .toStringList ();

    if (m_mru_files_encodings.count () != m_mru_files.count ())
      {
        // encodings don't have the same count -> do not use them!
        m_mru_files_encodings = QStringList ();
        for (int i = 0; i < m_mru_files.count (); i++)
          m_mru_files_encodings << QString ();
      }

    for (int i = 0; i < MaxMRUFiles; ++i)
      {
        m_mru_file_actions[i] = new QAction (this);
        m_mru_file_actions[i]->setVisible (false);
      }

    // menu bar

    // file menu

    m_fileMenu = add_menu (m_menu_bar, tr ("&File"));

    // new and open menus are inserted later by the main window
    m_mru_file_menu = new QMenu (tr ("&Recent Editor Files"), m_fileMenu);
    for (int i = 0; i < MaxMRUFiles; ++i)
      m_mru_file_menu->addAction (m_mru_file_actions[i]);
    m_fileMenu->addMenu (m_mru_file_menu);

    m_fileMenu->addSeparator ();

    m_edit_function_action
      = add_action (m_fileMenu,
                    tr ("&Edit Function"),
                    SLOT (request_context_edit (bool)));

    m_fileMenu->addSeparator ();

    m_save_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("document-save"),
                    tr ("&Save File"),
                    SLOT (request_save_file (bool)));

    m_save_as_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("document-save-as"),
                    tr ("Save File &As..."),
                    SLOT (request_save_file_as (bool)));

    m_fileMenu->addSeparator ();

    m_close_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("window-close",false),
                    tr ("&Close"),
                    SLOT (request_close_file (bool)));

    m_close_all_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("window-close",false),
                    tr ("Close All"),
                    SLOT (request_close_all_files (bool)));

    m_close_others_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("window-close",false),
                    tr ("Close Other Files"),
                    SLOT (request_close_other_files (bool)));

    m_fileMenu->addSeparator ();

    m_print_action
      = add_action (m_fileMenu,
                    resource_manager::icon ("document-print"),
                    tr ("Print..."),
                    SLOT (request_print_file (bool)));

    // edit menu (undo, copy, paste and select all later via main window)

    m_edit_menu = add_menu (m_menu_bar, tr ("&Edit"));

    m_redo_action
      = add_action (m_edit_menu,
                    resource_manager::icon ("edit-redo"),
                    tr ("&Redo"),
                    SLOT (request_redo (bool)));
    m_redo_action->setEnabled (false);

    m_edit_menu->addSeparator ();

    m_cut_action
      = add_action (m_edit_menu,
                    resource_manager::icon ("edit-cut"),
                    tr ("Cu&t"),
                    SLOT (request_cut (bool)));
    m_cut_action->setEnabled (false);

    m_find_action
      = add_action (m_edit_menu,
                    resource_manager::icon ("edit-find-replace"),
                    tr ("&Find and Replace..."),
                    SLOT (request_find (bool)));

    m_find_next_action
      = add_action (m_edit_menu,
                    tr ("Find &Next..."),
                    SLOT (request_find_next (bool)));

    m_find_previous_action
      = add_action (m_edit_menu,
                    tr ("Find &Previous..."),
                    SLOT (request_find_previous (bool)));

    m_edit_menu->addSeparator ();

    m_edit_cmd_menu = m_edit_menu->addMenu (tr ("&Commands"));

    m_delete_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Delete Line"),
                    SLOT (request_delete_line (bool)));

    m_copy_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Copy Line"),
                    SLOT (request_copy_line (bool)));

    m_cut_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Cut Line"),
                    SLOT (request_cut_line (bool)));

    m_edit_cmd_menu->addSeparator ();

    m_delete_start_word_action
      = add_action (m_edit_cmd_menu,
                    tr ("Delete to Start of Word"),
                    SLOT (request_delete_start_word (bool)));

    m_delete_end_word_action
      = add_action (m_edit_cmd_menu,
                    tr ("Delete to End of Word"),
                    SLOT (request_delete_end_word (bool)));

    m_delete_start_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Delete to Start of Line"),
                    SLOT (request_delete_start_line (bool)));

    m_delete_end_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Delete to End of Line"),
                    SLOT (request_delete_end_line (bool)));

    m_edit_cmd_menu->addSeparator ();

    m_duplicate_selection_action
      = add_action (m_edit_cmd_menu,
                    tr ("Duplicate Selection/Line"),
                    SLOT (request_duplicate_selection (bool)));

    m_transpose_line_action
      = add_action (m_edit_cmd_menu,
                    tr ("Transpose Line"),
                    SLOT (request_transpose_line (bool)));

    m_edit_cmd_menu->addSeparator ();

    m_completion_action
      = add_action (m_edit_cmd_menu,
                    tr ("&Show Completion List"),
                    SLOT (request_completion (bool)));

    m_edit_fmt_menu = m_edit_menu->addMenu (tr ("&Format"));

    m_upper_case_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Uppercase Selection"),
                    SLOT (request_upper_case (bool)));

    m_lower_case_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Lowercase Selection"),
                    SLOT (request_lower_case (bool)));

    m_edit_fmt_menu->addSeparator ();

    m_comment_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Comment"),
                    SLOT (request_comment_selected_text (bool)));

    m_uncomment_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Uncomment"),
                    SLOT (request_uncomment_selected_text (bool)));

    m_comment_var_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("Comment (Choosing String)"),
                    SLOT (request_comment_var_selected_text (bool)));

    m_edit_fmt_menu->addSeparator ();

    m_indent_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Indent Selection Rigidly"),
                    SLOT (request_indent_selected_text (bool)));

    m_unindent_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("&Unindent Selection Rigidly"),
                    SLOT (request_unindent_selected_text (bool)));

    m_smart_indent_line_or_selection_action
      = add_action (m_edit_fmt_menu,
                    tr ("Indent Code"),
                    SLOT (request_smart_indent_line_or_selected_text (void)));

    m_edit_fmt_menu->addSeparator ();

    m_conv_eol_windows_action
      = add_action (m_edit_fmt_menu,
                    tr ("Convert Line Endings to &Windows (CRLF)"),
                    SLOT (request_conv_eol_windows (bool)));

    m_conv_eol_unix_action
      = add_action (m_edit_fmt_menu,
                    tr ("Convert Line Endings to &Unix (LF)"),
                    SLOT (request_conv_eol_unix (bool)));

    m_conv_eol_mac_action
      = add_action (m_edit_fmt_menu,
                    tr ("Convert Line Endings to &Mac (CR)"),
                    SLOT (request_conv_eol_mac (bool)));

    m_edit_nav_menu = m_edit_menu->addMenu (tr ("Navi&gation"));

    m_goto_line_action
      = add_action (m_edit_nav_menu,
                    tr ("Go &to Line..."),
                    SLOT (request_goto_line (bool)));

    m_edit_cmd_menu->addSeparator ();

    m_move_to_matching_brace
      = add_action (m_edit_nav_menu,
                    tr ("Move to Matching Brace"),
                    SLOT (request_move_match_brace (bool)));

    m_sel_to_matching_brace
      = add_action (m_edit_nav_menu,
                    tr ("Select to Matching Brace"),
                    SLOT (request_sel_match_brace (bool)));

    m_edit_nav_menu->addSeparator ();

    m_next_bookmark_action
      = add_action (m_edit_nav_menu,
                    tr ("&Next Bookmark"),
                    SLOT (request_next_bookmark (bool)));

    m_previous_bookmark_action
      = add_action (m_edit_nav_menu,
                    tr ("Pre&vious Bookmark"),
                    SLOT (request_previous_bookmark (bool)));

    m_toggle_bookmark_action
      = add_action (m_edit_nav_menu,
                    tr ("Toggle &Bookmark"),
                    SLOT (request_toggle_bookmark (bool)));

    m_remove_bookmark_action
      = add_action (m_edit_nav_menu,
                    tr ("&Remove All Bookmarks"),
                    SLOT (request_remove_bookmark (bool)));

    m_edit_menu->addSeparator ();

    m_preferences_action
      = add_action (m_edit_menu,
                    resource_manager::icon ("preferences-system"),
                    tr ("&Preferences..."),
                    SLOT (request_preferences (bool)));

    m_styles_preferences_action
      = add_action (m_edit_menu,  resource_manager::icon ("preferences-system"),
                    tr ("&Styles Preferences..."),
                    SLOT (request_styles_preferences (bool)));

    // view menu

    QMenu *view_menu = add_menu (m_menu_bar, tr ("&View"));

    m_view_editor_menu = view_menu->addMenu (tr ("&Editor"));

    m_show_linenum_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Line Numbers"),
                    SLOT (show_line_numbers (bool)));
    m_show_linenum_action->setCheckable (true);

    m_show_whitespace_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Whitespace Characters"),
                    SLOT (show_white_space (bool)));
    m_show_whitespace_action->setCheckable (true);

    m_show_eol_action
      = add_action (m_view_editor_menu,
                    tr ("Show Line &Endings"),
                    SLOT (show_eol_chars (bool)));
    m_show_eol_action->setCheckable (true);

    m_show_indguide_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Indentation Guides"),
                    SLOT (show_indent_guides (bool)));
    m_show_indguide_action->setCheckable (true);

    m_show_longline_action
      = add_action (m_view_editor_menu,
                    tr ("Show Long Line &Marker"),
                    SLOT (show_long_line (bool)));
    m_show_longline_action->setCheckable (true);

    m_view_editor_menu->addSeparator ();

    m_show_toolbar_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Toolbar"),
                    SLOT (show_toolbar (bool)));
    m_show_toolbar_action->setCheckable (true);

    m_show_statusbar_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Statusbar"),
                    SLOT (show_statusbar (bool)));
    m_show_statusbar_action->setCheckable (true);

    m_show_hscrollbar_action
      = add_action (m_view_editor_menu,
                    tr ("Show &Horizontal Scrollbar"),
                    SLOT (show_hscrollbar (bool)));
    m_show_hscrollbar_action->setCheckable (true);

    view_menu->addSeparator ();

    m_zoom_in_action
      = add_action (view_menu, resource_manager::icon ("zoom-in"),
                    tr ("Zoom &In"),
                    SLOT (zoom_in (bool)));

    m_zoom_out_action
      = add_action (view_menu, resource_manager::icon ("zoom-out"),
                    tr ("Zoom &Out"),
                    SLOT (zoom_out (bool)));

    m_zoom_normal_action
      = add_action (view_menu,
                    tr ("&Normal Size"),
                    SLOT (zoom_normal (bool)));

    m_menu_bar->addMenu (view_menu);

    // debug menu

    m_debug_menu = add_menu (m_menu_bar, tr ("&Debug"));

    m_toggle_breakpoint_action
      = add_action (m_debug_menu,
                    resource_manager::icon ("bp-toggle"),
                    tr ("Toggle &Breakpoint"),
                    SLOT (request_toggle_breakpoint (bool)));

    m_next_breakpoint_action
      = add_action (m_debug_menu,
                    resource_manager::icon ("bp-next"),
                    tr ("&Next Breakpoint"),
                    SLOT (request_next_breakpoint (bool)));

    m_previous_breakpoint_action
      = add_action (m_debug_menu,
                    resource_manager::icon ("bp-prev"),
                    tr ("Pre&vious Breakpoint"),
                    SLOT (request_previous_breakpoint (bool)));

    m_remove_all_breakpoints_action
      = add_action (m_debug_menu,
                    resource_manager::icon ("bp-rm-all"),
                    tr ("&Remove All Breakpoints"),
                    SLOT (request_remove_breakpoint (bool)));

    m_debug_menu->addSeparator ();

    // The other debug actions will be added by the main window.

    // run menu

    QMenu *_run_menu = add_menu (m_menu_bar, tr ("&Run"));

    m_run_action
      = add_action (_run_menu,
                    resource_manager::icon ("system-run"),
                    tr ("Save File and Run"),
                    SLOT (request_run_file (bool)));

    m_run_selection_action
      = add_action (_run_menu,
                    tr ("Run &Selection"),
                    SLOT (request_context_run (bool)));
    m_run_selection_action->setEnabled (false);

    // help menu

    QMenu *_help_menu = add_menu (m_menu_bar, tr ("&Help"));

    m_context_help_action
      = add_action (_help_menu,
                    tr ("&Help on Keyword"),
                    SLOT (request_context_help (bool)));

    m_context_doc_action
      = add_action (_help_menu,
                    tr ("&Documentation on Keyword"),
                    SLOT (request_context_doc (bool)));

    // tab navigation (no menu, only actions; slots in tab_bar)

    m_switch_left_tab_action
      = add_action (nullptr, "", SLOT (switch_left_tab (void)),
                    m_tab_widget->get_tab_bar ());

    m_switch_right_tab_action
      = add_action (nullptr, "", SLOT (switch_right_tab (void)),
                    m_tab_widget->get_tab_bar ());

    m_move_tab_left_action
      = add_action (nullptr, "", SLOT (move_tab_left (void)),
                    m_tab_widget->get_tab_bar ());

    m_move_tab_right_action
      = add_action (nullptr, "", SLOT (move_tab_right (void)),
                    m_tab_widget->get_tab_bar ());

    // toolbar

    // popdown menu with mru files
    QToolButton *popdown_button = new QToolButton ();
    popdown_button->setToolTip (tr ("Recent Files"));
    popdown_button->setMenu (m_mru_file_menu);
    popdown_button->setPopupMode (QToolButton::InstantPopup);
    popdown_button->setToolButtonStyle (Qt::ToolButtonTextOnly);

    // new and open actions are inserted later from main window
    m_popdown_mru_action = m_tool_bar->addWidget (popdown_button);
    m_tool_bar->addAction (m_save_action);
    m_tool_bar->addAction (m_save_as_action);
    m_tool_bar->addAction (m_print_action);
    m_tool_bar->addSeparator ();
    // m_undo_action: later via main window
    m_tool_bar->addAction (m_redo_action);
    // m_copy_action: later via the main window
    m_tool_bar->addAction (m_cut_action);
    // m_paste_action: later via the main window
    m_tool_bar->addAction (m_find_action);
    //m_tool_bar->addAction (m_find_next_action);
    //m_tool_bar->addAction (m_find_previous_action);
    m_tool_bar->addSeparator ();
    m_tool_bar->addAction (m_run_action);
    m_tool_bar->addSeparator ();
    m_tool_bar->addAction (m_toggle_breakpoint_action);
    m_tool_bar->addAction (m_previous_breakpoint_action);
    m_tool_bar->addAction (m_next_breakpoint_action);
    m_tool_bar->addAction (m_remove_all_breakpoints_action);

    // layout
    QVBoxLayout *vbox_layout = new QVBoxLayout ();
    vbox_layout->addWidget (m_menu_bar);
    vbox_layout->addWidget (m_tool_bar);
    vbox_layout->addWidget (m_tab_widget);
    vbox_layout->setMargin (0);
    editor_widget->setLayout (vbox_layout);
    setWidget (editor_widget);

    // create the context menu of the tab bar
    tab_bar *bar = m_tab_widget->get_tab_bar ();
    QMenu *ctx_men = bar->get_context_menu ();
    ctx_men->addAction (m_close_action);
    ctx_men->addAction (m_close_all_action);
    ctx_men->addAction (m_close_others_action);

    // signals
    connect (this,
             SIGNAL (execute_command_in_terminal_signal (const QString&)),
             main_win (), SLOT (execute_command_in_terminal (const QString&)));

    connect (this, SIGNAL (request_settings_dialog (const QString&)),
             main_win (),
             SLOT (process_settings_dialog_request (const QString&)));

    connect (m_mru_file_menu, SIGNAL (triggered (QAction *)),
             this, SLOT (request_mru_open_file (QAction *)));

    mru_menu_update ();

    connect (m_tab_widget, SIGNAL (tabCloseRequested (int)),
             this, SLOT (handle_tab_close_request (int)));

    connect (m_tab_widget, SIGNAL (currentChanged (int)),
             this, SLOT (active_tab_changed (int)));

    resize (500, 400);
    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    set_title (tr ("Editor"));

    check_actions ();
  }

  void file_editor::add_file_editor_tab (file_editor_tab *f, const QString& fn)
  {
    m_tab_widget->addTab (f, fn);

    // signals from the qscintilla edit area
    connect (f->qsci_edit_area (), SIGNAL (status_update (bool, bool)),
             this, SLOT (edit_status_update (bool, bool)));

    connect (f->qsci_edit_area (), SIGNAL (show_doc_signal (const QString&)),
             main_win (), SLOT (handle_show_doc (const QString&)));

    connect (f->qsci_edit_area (), SIGNAL (create_context_menu_signal (QMenu *)),
             this, SLOT (create_context_menu (QMenu *)));

    connect (f->qsci_edit_area (),
             SIGNAL (execute_command_in_terminal_signal (const QString&)),
             main_win (), SLOT (execute_command_in_terminal (const QString&)));

    // Signals from the file editor_tab
    connect (f, SIGNAL (file_name_changed (const QString&, const QString&)),
             this, SLOT (handle_file_name_changed (const QString&,
                                                   const QString&)));

    connect (f, SIGNAL (editor_state_changed (bool, bool)),
             this, SLOT (handle_editor_state_changed (bool, bool)));

    connect (f, SIGNAL (tab_remove_request ()),
             this, SLOT (handle_tab_remove_request ()));

    connect (f, SIGNAL (add_filename_to_list (const QString&,
                                              const QString&, QWidget*)),
             this, SLOT (handle_add_filename_to_list (const QString&,
                                                      const QString&,
                                                      QWidget*)));

    connect (f, SIGNAL (editor_check_conflict_save (const QString&, bool)),
             this, SLOT (check_conflict_save (const QString&, bool)));

    connect (f, SIGNAL (mru_add_file (const QString&, const QString&)),
             this, SLOT (handle_mru_add_file (const QString&, const QString&)));

    connect (f, SIGNAL (run_file_signal (const QFileInfo&)),
             main_win (), SLOT (run_file_in_terminal (const QFileInfo&)));

    connect (f, SIGNAL (request_open_file (const QString&)),
             this, SLOT (request_open_file (const QString&)));

    connect (f, SIGNAL (edit_mfile_request (const QString&, const QString&,
                                            const QString&, int)),
             main_win (), SLOT (handle_edit_mfile_request (const QString&,
                                                           const QString&,
                                                           const QString&, int)));

    connect (f, SIGNAL (set_focus_editor_signal (QWidget*)),
             this, SLOT (set_focus (QWidget*)));

    // Signals from the file_editor non-trivial operations
    connect (this, SIGNAL (fetab_settings_changed (const QSettings *)),
             f, SLOT (notice_settings (const QSettings *)));

    connect (this, SIGNAL (fetab_change_request (const QWidget*)),
             f, SLOT (change_editor_state (const QWidget*)));

    connect (this, SIGNAL (fetab_file_name_query (const QWidget*)),
             f, SLOT (file_name_query (const QWidget*)));

    connect (this, SIGNAL (fetab_save_file (const QWidget*, const QString&,
                                            bool)),
             f, SLOT (save_file (const QWidget*, const QString&, bool)));

    connect (this, SIGNAL (fetab_check_modified_file (void)),
             f, SLOT (check_modified_file (void)));

    connect (f, SIGNAL (execute_command_in_terminal_signal (const QString&)),
             main_win (), SLOT (execute_command_in_terminal (const QString&)));

    // Signals from the file_editor trivial operations
    connect (this, SIGNAL (fetab_recover_from_exit (void)),
             f, SLOT (recover_from_exit (void)));

    connect (this, SIGNAL (fetab_set_directory (const QString&)),
             f, SLOT (set_current_directory (const QString&)));

    connect (this, SIGNAL (fetab_zoom_in (const QWidget*)),
             f, SLOT (zoom_in (const QWidget*)));
    connect (this, SIGNAL (fetab_zoom_out (const QWidget*)),
             f, SLOT (zoom_out (const QWidget*)));
    connect (this, SIGNAL (fetab_zoom_normal (const QWidget*)),
             f, SLOT (zoom_normal (const QWidget*)));

    connect (this, SIGNAL (fetab_context_help (const QWidget*, bool)),
             f, SLOT (context_help (const QWidget*, bool)));

    connect (this, SIGNAL (fetab_context_edit (const QWidget*)),
             f, SLOT (context_edit (const QWidget*)));

    connect (this, SIGNAL (fetab_save_file (const QWidget*)),
             f, SLOT (save_file (const QWidget*)));

    connect (this, SIGNAL (fetab_save_file_as (const QWidget*)),
             f, SLOT (save_file_as (const QWidget*)));

    connect (this, SIGNAL (fetab_print_file (const QWidget*)),
             f, SLOT (print_file (const QWidget*)));

    connect (this, SIGNAL (fetab_run_file (const QWidget*)),
             f, SLOT (run_file (const QWidget*)));

    connect (this, SIGNAL (fetab_context_run (const QWidget*)),
             f, SLOT (context_run (const QWidget*)));

    connect (this, SIGNAL (fetab_toggle_bookmark (const QWidget*)),
             f, SLOT (toggle_bookmark (const QWidget*)));

    connect (this, SIGNAL (fetab_next_bookmark (const QWidget*)),
             f, SLOT (next_bookmark (const QWidget*)));

    connect (this, SIGNAL (fetab_previous_bookmark (const QWidget*)),
             f, SLOT (previous_bookmark (const QWidget*)));

    connect (this, SIGNAL (fetab_remove_bookmark (const QWidget*)),
             f, SLOT (remove_bookmark (const QWidget*)));

    connect (this, SIGNAL (fetab_toggle_breakpoint (const QWidget*)),
             f, SLOT (toggle_breakpoint (const QWidget*)));

    connect (this, SIGNAL (fetab_next_breakpoint (const QWidget*)),
             f, SLOT (next_breakpoint (const QWidget*)));

    connect (this, SIGNAL (fetab_previous_breakpoint (const QWidget*)),
             f, SLOT (previous_breakpoint (const QWidget*)));

    connect (this, SIGNAL (fetab_remove_all_breakpoints (const QWidget*)),
             f, SLOT (remove_all_breakpoints (const QWidget*)));

    connect (this, SIGNAL (fetab_scintilla_command (const QWidget *,
                                                    unsigned int)),
             f, SLOT (scintilla_command (const QWidget *, unsigned int)));

    connect (this, SIGNAL (fetab_comment_selected_text (const QWidget*, bool)),
             f, SLOT (comment_selected_text (const QWidget*, bool)));

    connect (this, SIGNAL (fetab_uncomment_selected_text (const QWidget*)),
             f, SLOT (uncomment_selected_text (const QWidget*)));

    connect (this, SIGNAL (fetab_indent_selected_text (const QWidget*)),
             f, SLOT (indent_selected_text (const QWidget*)));

    connect (this, SIGNAL (fetab_unindent_selected_text (const QWidget*)),
             f, SLOT (unindent_selected_text (const QWidget*)));

    connect (this, SIGNAL (fetab_smart_indent_line_or_selected_text (const QWidget*)),
             f, SLOT (smart_indent_line_or_selected_text (const QWidget*)));

    connect (this,
             SIGNAL (fetab_convert_eol (const QWidget*, QsciScintilla::EolMode)),
             f, SLOT (convert_eol (const QWidget*, QsciScintilla::EolMode)));

    connect (this, SIGNAL (fetab_find (const QWidget*, QList<QAction *>)),
             f, SLOT (find (const QWidget*, QList<QAction *>)));

    connect (this, SIGNAL (fetab_find_next (const QWidget*)),
             f, SLOT (find_next (const QWidget*)));

    connect (this, SIGNAL (fetab_find_previous (const QWidget*)),
             f, SLOT (find_previous (const QWidget*)));

    connect (this, SIGNAL (fetab_goto_line (const QWidget*, int)),
             f, SLOT (goto_line (const QWidget*, int)));

    connect (this, SIGNAL (fetab_move_match_brace (const QWidget*, bool)),
             f, SLOT (move_match_brace (const QWidget*, bool)));

    connect (this, SIGNAL (fetab_completion (const QWidget*)),
             f, SLOT (show_auto_completion (const QWidget*)));

    connect (this, SIGNAL (fetab_set_focus (const QWidget*)),
             f, SLOT (set_focus (const QWidget*)));

    connect (this, SIGNAL (fetab_insert_debugger_pointer (const QWidget*, int)),
             f, SLOT (insert_debugger_pointer (const QWidget*, int)));

    connect (this, SIGNAL (fetab_delete_debugger_pointer (const QWidget*, int)),
             f, SLOT (delete_debugger_pointer (const QWidget*, int)));

    connect (this, SIGNAL (fetab_do_breakpoint_marker (bool, const QWidget*,
                                                       int, const QString&)),
             f, SLOT (do_breakpoint_marker (bool, const QWidget*, int,
                                            const QString&)));

    m_tab_widget->setCurrentWidget (f);

    check_actions ();
  }

  void file_editor::mru_menu_update (void)
  {
    int num_files = qMin (m_mru_files.size (), int (MaxMRUFiles));

    // configure and show active actions of mru-menu
    for (int i = 0; i < num_files; ++i)
      {
        QString text = QString ("&%1 %2").
          arg ((i+1) % int (MaxMRUFiles)).arg (m_mru_files.at (i));
        m_mru_file_actions[i]->setText (text);

        QStringList action_data;
        action_data << m_mru_files.at (i) << m_mru_files_encodings.at (i);
        m_mru_file_actions[i]->setData (action_data);

        m_mru_file_actions[i]->setVisible (true);
      }

    // hide unused mru-menu entries
    for (int j = num_files; j < MaxMRUFiles; ++j)
      m_mru_file_actions[j]->setVisible (false);

    // delete entries in string-list beyond MaxMRUFiles
    while (m_mru_files.size () > MaxMRUFiles)
      {
        m_mru_files.removeLast ();
        m_mru_files_encodings.removeLast ();
      }

    // save actual mru-list in settings
    QSettings *settings = resource_manager::get_settings ();

    settings->setValue ("editor/mru_file_list", m_mru_files);
    settings->setValue ("editor/mru_file_encodings", m_mru_files_encodings);
    settings->sync ();
  }

  bool file_editor::call_custom_editor (const QString& file_name, int line)
  {
    // Check if the user wants to use a custom file editor.
    QSettings *settings = resource_manager::get_settings ();

    if (settings->value ("useCustomFileEditor",false).toBool ())
      {
        // use the external editor interface for handling the call
        emit request_open_file_external (file_name, line);

        if (line < 0 && ! file_name.isEmpty ())
          handle_mru_add_file (QFileInfo (file_name).canonicalFilePath (),
                               QString ());

        return true;
      }

    return false;
  }

  void file_editor::toggle_preference (const QString& preference, bool def)
  {
    QSettings *settings = resource_manager::get_settings ();
    bool old = settings->value (preference,def).toBool ();
    settings->setValue (preference,! old);
    notice_settings (settings);
  }

  // Function for closing the files in a removed directory
  void file_editor::handle_dir_remove (const QString& old_name,
                                       const QString& new_name)
  {
    QDir old_dir (old_name);

    // Have all file editor tabs signal what their filenames are.
    m_editor_tab_map.clear ();
    emit fetab_file_name_query (nullptr);

    // Loop over all open files and pick those within old_dir
    for (auto p = m_editor_tab_map.cbegin ();
         p != m_editor_tab_map.cend (); p++)
      {
        QString rel_path_to_file = old_dir.relativeFilePath (p->first);
        if (rel_path_to_file.left (3) != QString ("../"))
          {
            // We directly go down from old_dir to reach our file: Our
            // file is included in the removed/renamed diectory.
            // Thus delete it.
            m_no_focus = true;  // Remember for not focussing editor
            file_editor_tab *editor_tab
              = static_cast<file_editor_tab *> (p->second.fet_ID);
            editor_tab->file_has_changed (QString (), true);  // Close
            m_no_focus = false;  // Back to normal

            // Store file for possible later reload
            m_tmp_closed_files << p->first;

            // Add the new file path and the encoding for later reloading
            // if new_name is given
            if (! new_name.isEmpty ())
              {
                QDir new_dir (new_name);
                m_tmp_closed_files << new_dir.absoluteFilePath (rel_path_to_file);
              }
            else
              m_tmp_closed_files << ""; // no new name, just removing this file

            m_tmp_closed_files << p->second.encoding; // store the encoding
          }
      }
  }

  bool file_editor::editor_tab_has_focus (void)
  {
    QWidget *foc_w = focusWidget ();
    if (foc_w && foc_w->inherits ("octave::octave_qscintilla"))
      return true;
    return false;
  }

  // Check whether this file is already open in the editor.
  QWidget * file_editor::find_tab_widget (const QString& file)
  {
    // Have all file editor tabs signal what their filenames are.
    m_editor_tab_map.clear ();
    emit fetab_file_name_query (nullptr);

    // Check all tabs for the given file name
    QWidget *retval = nullptr;

    for (auto p = m_editor_tab_map.cbegin ();
         p != m_editor_tab_map.cend (); p++)
      {
        QString tab_file = p->first;
        if (same_file (file.toStdString (), tab_file.toStdString ())
            || file == tab_file)     // needed as same_file ("","") is false.
          {
            retval = p->second.fet_ID;
            break;
          }
      }

    return retval;
  }

  QAction * file_editor::add_action (QMenu *menu, const QString& text,
                                     const char *member,
                                     QWidget *receiver)
  {
    return add_action (menu, QIcon (), text, member, receiver);
  }

  QAction * file_editor::add_action (QMenu *menu, const QIcon& icon,
                                     const QString& text, const char *member,
                                     QWidget *receiver)
  {
    QAction *a;
    QWidget *r = this;

    if (receiver != nullptr)
      r = receiver;

    if (menu)
      a = menu->addAction (icon, text, r, member);
    else
      {
        a = new QAction (this);
        connect (a, SIGNAL (triggered ()), r, member);
      }

    addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::WidgetWithChildrenShortcut);

    return a;
  }

  QMenu* file_editor::add_menu (QMenuBar *p, QString name)
  {
    QMenu *menu = p->addMenu (name);

    QString base_name = name;  // get a copy
    // replace intended '&' ("&&") by a temp. string
    base_name.replace ("&&", "___octave_amp_replacement___");
    // remove single '&' (shortcut)
    base_name.remove ("&");
    // restore intended '&'
    base_name.replace ("___octave_amp_replacement___", "&&");

    // remember names with and without shortcut
    m_hash_menu_text[menu] = QStringList () << name << base_name;

    return menu;
  }
}

#endif
