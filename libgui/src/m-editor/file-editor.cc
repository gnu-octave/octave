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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include <algorithm>

#include <QApplication>
#include <QClipboard>
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

#include "file-editor.h"
#include "gui-preferences-ed.h"
#include "gui-preferences-sc.h"
#include "gui-preferences-global.h"
#include "main-window.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "shortcut-manager.h"

#include "oct-env.h"

#include "event-manager.h"
#include "interpreter.h"
#include "oct-map.h"
#include "pt-eval.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Functions of the the reimplemented tab widget

file_editor_tab_widget::file_editor_tab_widget (QWidget *p, file_editor *fe)
: QTabWidget (p)
{
  tab_bar *bar = new tab_bar (this);

  connect (bar, &tab_bar::close_current_tab_signal,
           fe, &file_editor::request_close_file);

  this->setTabBar (bar);

  setTabsClosable (true);
  setUsesScrollButtons (true);
  setMovable (true);
}

tab_bar *file_editor_tab_widget::get_tab_bar (void) const
{
  return qobject_cast<tab_bar *> (tabBar ());
}

std::list<file_editor_tab *>
file_editor_tab_widget::tab_list (void) const
{
  std::list<file_editor_tab *> retval;
  for (int i = 0; i < count (); i++)
    retval.push_back (static_cast<file_editor_tab *> (widget (i)));
  return retval;
}

// File editor

file_editor::file_editor (QWidget *p, base_qobject& oct_qobj)
  : file_editor_interface (p, oct_qobj)
{
  // Set current editing directory before construction because loaded
  // files will change ced accordingly.
  m_ced = QDir::currentPath ();

  // Set actions that are later added by the main window to null,
  // preventing access to them when they are still undefined.
  m_undo_action = nullptr;
  m_copy_action = nullptr;
  m_paste_action = nullptr;
  m_selectall_action = nullptr;

  m_find_dialog = nullptr;

  m_closed = false;
  m_no_focus = false;
  m_editor_ready = false;

  m_copy_action_enabled = false;
  m_undo_action_enabled = false;
  m_current_tab_modified = false;

  construct ();

  setVisible (false);
  setAcceptDrops (true);
  setFocusPolicy (Qt::StrongFocus);
}

void file_editor::focusInEvent (QFocusEvent *e)
{
  // The focus is transferred to the active tab and its edit
  // area in this focus in event handler. This is to avoid
  // using focus proxies with conflicts in the proxy change
  // presumably introduced by bug
  // https://bugreports.qt.io/browse/QTBUG-61092
  reset_focus (); // Make sure editor tab with edit area get focus

  QDockWidget::focusInEvent (e);
}

// insert global actions, that should also be displayed in the editor window,
// into the editor's menu and/or toolbar
void file_editor::insert_global_actions (QList<QAction *> shared_actions)
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
  m_tool_bar->insertAction (m_redo_action, m_undo_action);
  m_edit_menu->insertAction (m_redo_action, m_undo_action);
  // select all
  m_selectall_action = shared_actions.at (SELECTALL_ACTION);
  m_edit_menu->insertAction (m_find_action, m_selectall_action);
  m_edit_menu->insertSeparator (m_find_action);
  // paste
  m_paste_action = shared_actions.at (PASTE_ACTION);
  m_tool_bar->insertAction (m_find_action, m_paste_action);
  m_edit_menu->insertAction (m_selectall_action, m_paste_action);
  m_edit_menu->insertSeparator (m_selectall_action);
  // copy
  m_copy_action = shared_actions.at (COPY_ACTION);
  m_tool_bar->insertAction (m_paste_action, m_copy_action);
  m_edit_menu->insertAction (m_paste_action, m_copy_action);
  // find files
  m_find_files_action = shared_actions.at (FIND_FILES_ACTION);
  m_edit_menu->insertAction (m_find_action, m_find_files_action);
}

void file_editor::handle_enter_debug_mode (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  QString sc_run = settings->sc_value (sc_edit_run_run_file);
  QString sc_cont = settings->sc_value (sc_main_debug_continue);

  if (sc_run == sc_cont)
    m_run_action->setShortcut (QKeySequence ());  // prevent ambiguous shortcuts

  m_run_action->setToolTip (tr ("Continue"));   // update tool tip

  emit enter_debug_mode_signal ();
}

void file_editor::handle_exit_debug_mode (void)
{
  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();
  scmgr.set_shortcut (m_run_action, sc_edit_run_run_file);
  m_run_action->setToolTip (tr ("Save File and Run"));  // update tool tip

  emit exit_debug_mode_signal ();
}

void file_editor::check_actions (void)
{
  // Do not include shared actions not only related to the editor
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

  m_run_action->setEnabled (have_tabs && m_is_octave_file);

  m_toggle_breakpoint_action->setEnabled (have_tabs && m_is_octave_file);
  m_next_breakpoint_action->setEnabled (have_tabs && m_is_octave_file);
  m_previous_breakpoint_action->setEnabled (have_tabs && m_is_octave_file);
  m_remove_all_breakpoints_action->setEnabled (have_tabs && m_is_octave_file);

  m_edit_function_action->setEnabled (have_tabs);
  m_save_action->setEnabled (have_tabs && m_current_tab_modified);
  m_save_as_action->setEnabled (have_tabs);
  m_close_action->setEnabled (have_tabs);
  m_close_all_action->setEnabled (have_tabs);
  m_close_others_action->setEnabled (have_tabs && m_tab_widget->count () > 1);
  m_sort_tabs_action->setEnabled (have_tabs && m_tab_widget->count () > 1);

  emit editor_tabs_changed_signal (have_tabs, m_is_octave_file);
}

// empty_script determines whether we have to create an empty script
// 1. At startup, when the editor has to be (really) visible
//    (Here we can not use the visibility changed signal)
// 2. When the editor becomes visible when octave is running
void file_editor::empty_script (bool startup, bool visible)
{

  if (startup)
    m_editor_ready = true;
  else
    {
      if (! m_editor_ready)
        return;  // not yet ready but got visibility changed signals
    }

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (settings->value (global_use_custom_editor.key,
                       global_use_custom_editor.def).toBool ())
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
      // check if editor is really visible or hidden between tabbed widgets
      QWidget *parent = parentWidget ();

      if (parent)
        {
          QList<QTabBar *> tab_list = parent->findChildren<QTabBar *>();

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
                      if (! (top > -1 && tab->tabText (top) == windowTitle ()))
                        return; // not current tab -> not visible
                    }
                  j++;
                }
            }
        }
    }

  request_new_file ("");
}

void file_editor::restore_session (gui_settings *settings)
{
  //restore previous session
  if (! settings->value (ed_restore_session).toBool ())
    return;

  // get the data from the settings file
  QStringList sessionFileNames
    = settings->value (ed_session_names).toStringList ();

  QStringList session_encodings
    = settings->value (ed_session_enc).toStringList ();

  QStringList session_index
    = settings->value (ed_session_ind).toStringList ();

  QStringList session_lines
    = settings->value (ed_session_lines).toStringList ();

  QStringList session_bookmarks
    = settings->value (ed_session_bookmarks).toStringList ();

  // fill a list of the struct and sort it (depending on index)
  QList<session_data> s_data;

  bool do_encoding = (session_encodings.count () == sessionFileNames.count ());
  bool do_index = (session_index.count () == sessionFileNames.count ());
  bool do_lines = (session_lines.count () == sessionFileNames.count ());
  bool do_bookmarks = (session_bookmarks.count () == sessionFileNames.count ());

  for (int n = 0; n < sessionFileNames.count (); ++n)
    {
      QFileInfo file = QFileInfo (sessionFileNames.at (n));
      if (! file.exists ())
        continue;

      session_data item = { 0, -1, sessionFileNames.at (n),
        QString (), QString (), QString ()};
      if (do_lines)
        item.line = session_lines.at (n).toInt ();
      if (do_index)
        item.index = session_index.at (n).toInt ();
      if (do_encoding)
        item.encoding = session_encodings.at (n);
      if (do_bookmarks)
        item.bookmarks = session_bookmarks.at (n);

      s_data << item;
    }

  std::sort (s_data.begin (), s_data.end ());

  // finally open the files with the desired encoding in the desired order
  for (int n = 0; n < s_data.count (); ++n)
    request_open_file (s_data.at (n).file_name, s_data.at (n).encoding,
                       s_data.at (n).line, false, false, true, "", -1,
                       s_data.at (n).bookmarks);
}

void file_editor::activate (void)
{
  if (m_no_focus)
    return;  // No focus for the editor if external open/close request

  octave_dock_widget::activate ();

  // set focus to current tab
  reset_focus ();
}

void file_editor::set_focus (QWidget *fet)
{
  setFocus ();

  // set focus to desired tab
  if (fet)
    m_tab_widget->setCurrentWidget (fet);
}

// function enabling/disabling the menu accelerators depending on the
// focus of the editor
void file_editor::enable_menu_shortcuts (bool enable)
{
  // Hide or show the find dialog together with the focus of the
  // editor widget depending on the overall visibility of the find dialog.
  // Do not change internal visibility state.
  if (m_find_dialog)
    m_find_dialog->set_visible (enable);

  // Take care of the shortcuts
  QHash<QMenu *, QStringList>::const_iterator i = m_hash_menu_text.constBegin ();

  while (i != m_hash_menu_text.constEnd ())
    {
      i.key ()->setTitle (i.value ().at (! enable));
      ++i;
    }

  // when editor loses focus, enable the actions, which are always active
  // in the main window due to missing info on selected text and undo actions
  if (m_copy_action && m_undo_action)
    {
      if (enable)
        {
          m_copy_action->setEnabled (m_copy_action_enabled);
          m_undo_action->setEnabled (m_undo_action_enabled);
        }
      else
        {
          m_copy_action_enabled = m_copy_action->isEnabled ();
          m_undo_action_enabled = m_undo_action->isEnabled ();
          m_copy_action->setEnabled (true);
          m_undo_action->setEnabled (true);
        }
    }
}

// Save open files for restoring in next session
// (even if last session will not be restored next time)
// together with encoding and the tab index
void file_editor::save_session (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QStringList fetFileNames;
  QStringList fet_encodings;
  QStringList fet_index;
  QStringList fet_lines;
  QStringList fet_bookmarks;

  std::list<file_editor_tab *> editor_tab_lst = m_tab_widget->tab_list ();

  for (auto editor_tab : editor_tab_lst)
    {
      QString file_name = editor_tab->file_name ();

      // Don't append unnamed files.

      if (! file_name.isEmpty ())
        {
          fetFileNames.append (file_name);
          fet_encodings.append (editor_tab->encoding ());

          QString index;
          fet_index.append (index.setNum (m_tab_widget->indexOf (editor_tab)));

          int l, c;
          editor_tab->qsci_edit_area ()->getCursorPosition (&l, &c);
          fet_lines.append (index.setNum (l + 1));

          fet_bookmarks.append (editor_tab->get_all_bookmarks ());
        }
    }

  settings->setValue (ed_session_names.key, fetFileNames);
  settings->setValue (ed_session_enc.key, fet_encodings);
  settings->setValue (ed_session_ind.key, fet_index);
  settings->setValue (ed_session_lines.key, fet_lines);
  settings->setValue (ed_session_bookmarks.key, fet_bookmarks);
  settings->sync ();
}

bool file_editor::check_closing (void)
{
  // When the application or the editor is closing and the user wants to
  // close all files, in the latter case all editor tabs are checked whether
  // they need to be saved.  During these checks tabs are not closed since
  // the user might cancel closing Octave during one of these saving dialogs.
  // Therefore, saving the session for restoring at next start is not done
  // before the application is definitely closing.

  // Save the session. Even is closing is cancelled, this would be
  // overwritten by the next attempt to close the editor
  save_session ();

  std::list<file_editor_tab *> fe_tab_lst = m_tab_widget->tab_list ();
  m_number_of_tabs = fe_tab_lst.size ();

  for (auto fe_tab : fe_tab_lst)
    {
      // Wait for all editor tabs to have saved their files if required

      connect (fe_tab, &file_editor_tab::tab_ready_to_close,
               this, &file_editor::handle_tab_ready_to_close,
               Qt::UniqueConnection);
    }

  m_closing_canceled = false;

  for (auto fe_tab : fe_tab_lst)
    {
      // If there was a cancellation, make the already saved/discarded tabs
      // recover from the exit by removing the read-only state and by
      // recovering the debugger breakpoints.  Finally return false in order
      // to cancel closing the application or the editor.

      if (fe_tab->check_file_modified (false) == QMessageBox::Cancel)
        {
          emit fetab_recover_from_exit ();

          m_closing_canceled = true;

          for (auto fet : fe_tab_lst)
            disconnect (fet, &file_editor_tab::tab_ready_to_close, 0, 0);

          return false;
        }
    }

  return true;
}

void file_editor::handle_tab_ready_to_close (void)
{
  if (m_closing_canceled)
    return;

  // FIXME: Why count down to zero here before doing anything?  Why
  // not remove and delete each tab that is ready to be closed, one
  // per invocation?

  m_number_of_tabs--;

  if (m_number_of_tabs > 0)
    return;

  // Here, the application or the editor will be closed -> store the session

  // Take care of the find dialog
  if (m_find_dialog)
    m_find_dialog->close ();

  // Finally close all the tabs and return indication that we can exit
  // the application or close the editor.
  // Closing and deleting the tabs makes the editor visible.  In case it was
  // hidden before, this state has to be restored afterwards.
  bool vis = isVisible ();

  std::list<file_editor_tab *> editor_tab_lst = m_tab_widget->tab_list ();
  for (auto editor_tab : editor_tab_lst)
    editor_tab->deleteLater ();

  m_tab_widget->clear ();

  setVisible (vis);
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

  file_editor_tab *fileEditorTab = make_file_editor_tab (m_ced);
  add_file_editor_tab (fileEditorTab, "");  // new tab with empty title
  fileEditorTab->new_file (commands);       // title is updated here
  activate ();                              // focus editor and new tab
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

void file_editor::copy_full_file_path (bool)
{
  file_editor_tab *editor_tab
    = static_cast<file_editor_tab *> (m_tab_widget->currentWidget ());

  if (editor_tab)
    QGuiApplication::clipboard ()->setText (editor_tab->file_name ());
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
  // The interpreter_event callback function below emits a signal.
  // Because we don't control when that happens, use a guarded pointer
  // so that the callback can abort if this object is no longer valid.

  QPointer<file_editor> this_fe (this);

  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // If THIS_FE is no longer valid, skip the entire callback
      // function.  With the way things are implemented now, we can't
      // run the contents of a file unless the file editor and the
      // corresponding file editor tab are still valid.

      if (this_fe.isNull ())
        return;

      // Act as though this action was entered at the command propmt
      // so that the interpreter will check for updated file time
      // stamps.
      Vlast_prompt_time.stamp ();

      tree_evaluator& tw = interp.get_evaluator ();

      if (tw.in_debug_repl ())
        emit request_dbcont_signal ();
      else
        emit fetab_run_file (m_tab_widget->currentWidget ());
    });
}

void file_editor::request_step_into_file ()
{
  emit fetab_run_file (m_tab_widget->currentWidget (), true);
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

// Slot for initially creating and showing the find dialog
void file_editor::request_find (bool)
{
  // Create the dialog
  find_create ();

  // Since find_create shows the dialog without activating the widget
  // (which is reuqired in other cases) do this manually here
  m_find_dialog->activateWindow ();

  // Initiate search text from possible selection and save the initial
  // data from the dialog on the defined structure
  m_find_dialog->init_search_text ();
}

// This method creates the find dialog.

void file_editor::find_create ()
{
  if (m_find_dialog)
    m_find_dialog->close ();

  if (isFloating ())
    m_find_dialog = new find_dialog (m_octave_qobj, this, this);
  else
    m_find_dialog = new find_dialog (m_octave_qobj, this, parentWidget ());

  // Add required actions
  m_find_dialog->addAction (m_find_next_action);
  m_find_dialog->addAction (m_find_previous_action);

  // Update edit area
  file_editor_tab *fet
    = static_cast<file_editor_tab *> (m_tab_widget->currentWidget ());
  m_find_dialog->update_edit_area (fet->qsci_edit_area ());

  // Icon is the same as the editor
  m_find_dialog->setWindowIcon (windowIcon ());

  // Position:  lower right of editor's position
  int xp = x () + frameGeometry ().width ();
  int yp = y () + frameGeometry ().height ();

  if (! isFloating ())
    {
      // Fix position if editor is docked

      QWidget *parent = parentWidget ();

      if  (parent)
        {
          xp = xp + parent->x ();
          yp = yp + parent->y ();
        }
    }

  if (yp < 0)
    yp = 0;

  // The size of the find dialog is considered in restore_settings
  // since its size might change depending on the options
  m_find_dialog->restore_settings (QPoint (xp, yp));

  // Set visible
  m_find_dialog->set_visible (true);
}

void file_editor::request_find_next (bool)
{
  if (m_find_dialog)
    m_find_dialog->find_next ();
}

void file_editor::request_find_previous (bool)
{
  if (m_find_dialog)
    m_find_dialog->find_prev ();
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
                                            const QString& tip,
                                            bool modified)
{
  QObject *fileEditorTab = sender ();
  if (fileEditorTab)
    {
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

      for (int i = 0; i < m_tab_widget->count (); i++)
        {
          if (m_tab_widget->widget (i) == fileEditorTab)
            {
              m_tab_widget->setTabText (i, fname);
              m_tab_widget->setTabToolTip (i, tip);

              m_save_action->setEnabled (modified);
              m_current_tab_modified = modified;

              if (modified)
                m_tab_widget->setTabIcon (i, rmgr.icon ("document-save"));
              else
                m_tab_widget->setTabIcon (i, QIcon ());
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

              // Deleting the sender (even with deleteLater) seems a
              // bit strange.  Is there a better way?
              fileEditorTab->deleteLater ();
              break;
            }
        }
    }
  check_actions ();

  activate ();     // focus stays in editor when tab is closed

}

// context menu of edit area
void file_editor::active_tab_changed (int index)
{
  emit fetab_change_request (m_tab_widget->widget (index));
  activate ();
}

void file_editor::handle_editor_state_changed (bool copy_available,
                                               bool is_octave_file,
                                               bool is_modified)
{
  // In case there is some scenario where traffic could be coming from
  // all the file editor tabs, just process info from the current active tab.
  if (sender () == m_tab_widget->currentWidget ())
    {
      m_save_action->setEnabled (is_modified);
      m_current_tab_modified = is_modified;

      if (m_copy_action)
        m_copy_action->setEnabled (copy_available);

      m_cut_action->setEnabled (copy_available);

      m_run_selection_action->setEnabled (copy_available);
      m_run_action->setEnabled (is_octave_file);
      m_is_octave_file = is_octave_file;

      emit editor_tabs_changed_signal (true, m_is_octave_file);
    }

  m_copy_action_enabled = m_copy_action->isEnabled ();
  m_undo_action_enabled = m_undo_action->isEnabled ();
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
  file_editor_tab *tab = find_tab_widget (saveFileName);

  if (tab)
    {
      // Note: to overwrite the contents of some other file editor tab
      // with the same name requires identifying which file editor tab
      // that is (not too difficult) then closing that tab.  Of course,
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
      file_editor_tab *tab = find_tab_widget (file);

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

// Slot used for signals indicating that a file was changed/renamed or
// is going to be deleted/renamed
void file_editor::handle_file_remove (const QString& old_name,
                                      const QString& new_name)
{
  // Clear old list of file data and declare a structure for file data
  m_tmp_closed_files.clear ();
  removed_file_data f_data;

  // Preprocessing old name(s)
  QString old_name_clean = old_name.trimmed ();
  int s = old_name_clean.size ();

  if (s > 1 && old_name_clean.at (0) == QChar ('\"')
      && old_name_clean.at (s - 1) == QChar ('\"'))
    old_name_clean = old_name_clean.mid (1, s - 2);

  QStringList old_names = old_name_clean.split ("\" \"");

  // Check if new name is a file or directory
  QFileInfo newf (new_name);
  bool new_is_dir = newf.isDir ();

  // Now loop over all old files/dirs (several files by movefile ())
  for (int i = 0; i < old_names.count (); i++)
    {
      // Check if old name is a file or directory
      QFileInfo old (old_names.at (i));

      if (old.isDir ())
        {
          // Call the function which handles directories and return
          handle_dir_remove (old_names.at (i), new_name);
        }
      else
        {
          // It is a single file.  Is it open?
          file_editor_tab *editor_tab = find_tab_widget (old_names.at (i));

          if (editor_tab)
            {

              editor_tab->enable_file_watcher (false);

              // For re-enabling tracking if error while removing/renaming
              f_data.editor_tab = editor_tab;
              // For renaming into new file (if new_file is not empty)
              if (new_is_dir)
                {
                  std::string ndir = new_name.toStdString ();
                  std::string ofile = old.fileName ().toStdString ();
                  f_data.new_file_name
                    = QString::fromStdString (sys::env::make_absolute (ofile, ndir));
                }
              else
                f_data.new_file_name = new_name;

              // Add file data to list
              m_tmp_closed_files << f_data;
            }
        }
    }
}

// Slot for signal indicating that a file was renamed
void file_editor::handle_file_renamed (bool load_new)
{
  m_no_focus = true;  // Remember for not focussing editor

  // Loop over all files that have to be handled.  Start at the end of the
  // list, otherwise the stored indexes are not correct.
  for (int i = m_tmp_closed_files.count () - 1; i >= 0; i--)
    {
      if (load_new)
        {
          // Close file (remove) or rename into new file (rename)
          if (m_tmp_closed_files.at (i).new_file_name.isEmpty ())
            m_tmp_closed_files.at (i).editor_tab->file_has_changed (QString (), true);
          else
            m_tmp_closed_files.at (i).editor_tab->set_file_name (
                                                                 m_tmp_closed_files.at (i).new_file_name);
        }
      else
        {
          // Something went wrong while renaming or removing:
          // Leave everything as it is but reactivate tracking
          m_tmp_closed_files.at (i).editor_tab->enable_file_watcher (true);
        }

    }

  m_no_focus = false;  // Back to normal focus

  // Clear the list of file data
  m_tmp_closed_files.clear ();
}

void file_editor::notice_settings (const gui_settings *settings)
{
  int size_idx = settings->value (global_icon_size).toInt ();
  size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

  QStyle *st = style ();
  int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
  m_tool_bar->setIconSize (QSize (icon_size, icon_size));

  // Tab position and rotation
  QTabWidget::TabPosition pos
    = static_cast<QTabWidget::TabPosition> (settings->value (ed_tab_position).toInt ());
  bool rotated = settings->value (ed_tabs_rotated).toBool ();

  m_tab_widget->setTabPosition (pos);

  if (rotated)
    m_tab_widget->setTabsClosable (false);  // No close buttons
  // FIXME: close buttons can not be correctly placed in rotated tabs

  // Get the tab bar and set the rotation
  int rotation = rotated;
  if (pos == QTabWidget::West)
    rotation = -rotation;

  tab_bar *bar = m_tab_widget->get_tab_bar ();
  bar->set_rotated (rotation);

  // Get suitable height of a tab related to font and icon size
  int height = 1.5*QFontMetrics (m_tab_widget->font ()).height ();
  int is = 1.5*m_tab_widget->iconSize ().height ();
  if (is > height)
    height = is;

  // Calculate possibly limited width and set the elide mode
  int chars = settings->value (ed_tabs_max_width).toInt ();
  int width = 9999;
  if (chars > 0)
    width = chars * QFontMetrics (m_tab_widget->font ()).averageCharWidth ();

  // Get tab bar size properties for style sheet depending on rotation
  QString width_str ("width");
  QString height_str ("height");
  if ((pos == QTabWidget::West) || (pos == QTabWidget::East))
    {
      width_str = QString ("height");
      height_str = QString ("width");
    }

  QString style_sheet
    = QString ("QTabBar::tab {max-" + height_str + ": %1px;\n"
               "max-" + width_str + ": %2px; }")
    .arg (height).arg (width);

#if defined (Q_OS_MAC)
  // FIXME: This is a workaround for missing tab close buttons on MacOS
  // in several Qt versions (https://bugreports.qt.io/browse/QTBUG-61092)
  if (! rotated)
    {
      QString icon = global_icon_paths.at (ICON_THEME_OCTAVE) + "widget-close.png";

      QString close_button_css_mac (
                                    "QTabBar::close-button"
                                    " { image: url(" + icon + ");"
                                    " padding: 4px;"
                                    "   subcontrol-position: bottom; }\n"
                                    "QTabBar::close-button:hover"
                                    "  { background-color: #cccccc; }");

      style_sheet = style_sheet + close_button_css_mac;
    }
#endif

  m_tab_widget->setStyleSheet (style_sheet);

  bool show_it;
  show_it = settings->value (ed_show_line_numbers).toBool ();
  m_show_linenum_action->setChecked (show_it);
  show_it = settings->value (ed_show_white_space).toBool ();
  m_show_whitespace_action->setChecked (show_it);
  show_it = settings->value (ed_show_eol_chars).toBool ();
  m_show_eol_action->setChecked (show_it);
  show_it = settings->value (ed_show_indent_guides).toBool ();
  m_show_indguide_action->setChecked (show_it);
  show_it = settings->value (ed_long_line_marker).toBool ();
  m_show_longline_action->setChecked (show_it);

  show_it = settings->value (ed_show_toolbar).toBool ();
  m_show_toolbar_action->setChecked (show_it);
  m_tool_bar->setVisible (show_it);
  show_it = settings->value (ed_show_edit_status_bar).toBool ();
  m_show_statusbar_action->setChecked (show_it);
  show_it = settings->value (ed_show_hscroll_bar).toBool ();
  m_show_hscrollbar_action->setChecked (show_it);

  set_shortcuts ();

  // Find dialog with the same icon as the editor
  if (m_find_dialog)
    m_find_dialog->setWindowIcon (windowIcon ());

  // Relay signal to file editor tabs.
  emit fetab_settings_changed (settings);
}

void file_editor::set_shortcuts (void)
{
  // Shortcuts also available in the main window, as well as the related
  // shortcuts, are defined in main_window and added to the editor

  shortcut_manager& scmgr = m_octave_qobj.get_shortcut_manager ();

  // File menu
  scmgr.set_shortcut (m_edit_function_action, sc_edit_file_edit_function);
  scmgr.set_shortcut (m_save_action, sc_edit_file_save);
  scmgr.set_shortcut (m_save_as_action, sc_edit_file_save_as);
  scmgr.set_shortcut (m_close_action, sc_edit_file_close);
  scmgr.set_shortcut (m_close_all_action, sc_edit_file_close_all);
  scmgr.set_shortcut (m_close_others_action, sc_edit_file_close_other);
  scmgr.set_shortcut (m_print_action, sc_edit_file_print);

  // Edit menu
  scmgr.set_shortcut (m_redo_action, sc_edit_edit_redo);
  scmgr.set_shortcut (m_cut_action, sc_edit_edit_cut);
  scmgr.set_shortcut (m_find_action, sc_edit_edit_find_replace);
  scmgr.set_shortcut (m_find_next_action, sc_edit_edit_find_next);
  scmgr.set_shortcut (m_find_previous_action, sc_edit_edit_find_previous);

  scmgr.set_shortcut (m_delete_start_word_action, sc_edit_edit_delete_start_word);
  scmgr.set_shortcut (m_delete_end_word_action, sc_edit_edit_delete_end_word);
  scmgr.set_shortcut (m_delete_start_line_action, sc_edit_edit_delete_start_line);
  scmgr.set_shortcut (m_delete_end_line_action, sc_edit_edit_delete_end_line);
  scmgr.set_shortcut (m_delete_line_action, sc_edit_edit_delete_line);
  scmgr.set_shortcut (m_copy_line_action, sc_edit_edit_copy_line);
  scmgr.set_shortcut (m_cut_line_action, sc_edit_edit_cut_line);
  scmgr.set_shortcut (m_duplicate_selection_action, sc_edit_edit_duplicate_selection);
  scmgr.set_shortcut (m_transpose_line_action, sc_edit_edit_transpose_line);
  scmgr.set_shortcut (m_comment_selection_action, sc_edit_edit_comment_selection);
  scmgr.set_shortcut (m_uncomment_selection_action, sc_edit_edit_uncomment_selection);
  scmgr.set_shortcut (m_comment_var_selection_action, sc_edit_edit_comment_var_selection);

  scmgr.set_shortcut (m_upper_case_action, sc_edit_edit_upper_case);
  scmgr.set_shortcut (m_lower_case_action, sc_edit_edit_lower_case);
  scmgr.set_shortcut (m_indent_selection_action, sc_edit_edit_indent_selection);
  scmgr.set_shortcut (m_unindent_selection_action, sc_edit_edit_unindent_selection);
  scmgr.set_shortcut (m_smart_indent_line_or_selection_action, sc_edit_edit_smart_indent_line_or_selection);
  scmgr.set_shortcut (m_completion_action, sc_edit_edit_completion_list);
  scmgr.set_shortcut (m_goto_line_action, sc_edit_edit_goto_line);
  scmgr.set_shortcut (m_move_to_matching_brace, sc_edit_edit_move_to_brace);
  scmgr.set_shortcut (m_sel_to_matching_brace, sc_edit_edit_select_to_brace);
  scmgr.set_shortcut (m_toggle_bookmark_action, sc_edit_edit_toggle_bookmark);
  scmgr.set_shortcut (m_next_bookmark_action, sc_edit_edit_next_bookmark);
  scmgr.set_shortcut (m_previous_bookmark_action, sc_edit_edit_previous_bookmark);
  scmgr.set_shortcut (m_remove_bookmark_action, sc_edit_edit_remove_bookmark);
  scmgr.set_shortcut (m_preferences_action, sc_edit_edit_preferences);
  scmgr.set_shortcut (m_styles_preferences_action, sc_edit_edit_styles_preferences);

  scmgr.set_shortcut (m_conv_eol_windows_action, sc_edit_edit_conv_eol_winows);
  scmgr.set_shortcut (m_conv_eol_unix_action,    sc_edit_edit_conv_eol_unix);
  scmgr.set_shortcut (m_conv_eol_mac_action,     sc_edit_edit_conv_eol_mac);

  // View menu
  scmgr.set_shortcut (m_show_linenum_action, sc_edit_view_show_line_numbers);
  scmgr.set_shortcut (m_show_whitespace_action, sc_edit_view_show_white_spaces);
  scmgr.set_shortcut (m_show_eol_action, sc_edit_view_show_eol_chars);
  scmgr.set_shortcut (m_show_indguide_action, sc_edit_view_show_ind_guides);
  scmgr.set_shortcut (m_show_longline_action, sc_edit_view_show_long_line);
  scmgr.set_shortcut (m_show_toolbar_action, sc_edit_view_show_toolbar);
  scmgr.set_shortcut (m_show_statusbar_action, sc_edit_view_show_statusbar);
  scmgr.set_shortcut (m_show_hscrollbar_action, sc_edit_view_show_hscrollbar);
  scmgr.set_shortcut (m_zoom_in_action, sc_edit_view_zoom_in);
  scmgr.set_shortcut (m_zoom_out_action, sc_edit_view_zoom_out);
  scmgr.set_shortcut (m_zoom_normal_action, sc_edit_view_zoom_normal);
  scmgr.set_shortcut (m_sort_tabs_action, sc_edit_view_sort_tabs);

  // Debug menu
  scmgr.set_shortcut (m_toggle_breakpoint_action, sc_edit_debug_toggle_breakpoint);
  scmgr.set_shortcut (m_next_breakpoint_action, sc_edit_debug_next_breakpoint);
  scmgr.set_shortcut (m_previous_breakpoint_action, sc_edit_debug_previous_breakpoint);
  scmgr.set_shortcut (m_remove_all_breakpoints_action, sc_edit_debug_remove_breakpoints);

  // Run menu
  scmgr.set_shortcut (m_run_action, sc_edit_run_run_file);
  scmgr.set_shortcut (m_run_selection_action, sc_edit_run_run_selection);

  // Help menu
  scmgr.set_shortcut (m_context_help_action, sc_edit_help_help_keyword);
  scmgr.set_shortcut (m_context_doc_action,  sc_edit_help_doc_keyword);

  // Tab navigation without menu entries
  scmgr.set_shortcut (m_switch_left_tab_action, sc_edit_tabs_switch_left_tab);
  scmgr.set_shortcut (m_switch_right_tab_action, sc_edit_tabs_switch_right_tab);
  scmgr.set_shortcut (m_move_tab_left_action, sc_edit_tabs_move_tab_left);
  scmgr.set_shortcut (m_move_tab_right_action, sc_edit_tabs_move_tab_right);

}

// This slot is a reimplementation of the virtual slot in octave_dock_widget.
// We need this for creating an empty script when the editor has no open
// files and is made visible.
void file_editor::handle_visibility (bool visible)
{
  octave_dock_widget::handle_visibility (visible);

  if (! m_editor_ready)
    return;

  if (m_closed && visible)
    {
      m_closed = false;
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      gui_settings *settings = rmgr.get_settings ();
      restore_session (settings);
    }

  empty_script (false, visible);
}

// This slot is a reimplementation of the virtual slot in octave_dock_widget.
// We need this for updating the parent of the find dialog
void file_editor::toplevel_change (bool)
{
  if (m_find_dialog)
    {
      // close current dialog
      m_find_dialog->close ();

      // re-create dialog with the new parent (editor or main-win)
      find_create ();
      m_find_dialog->activateWindow ();
    }
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
                                     const QString& cond, int index,
                                     const QString& bookmarks)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings->value (global_use_custom_editor).toBool ())
    {
      // Custom editor
      if (debug_pointer || breakpoint_marker)
        return;   // Do not call custom editor during debugging

      if (call_custom_editor (openFileName, line))
        return;   // Custom editor called
    }

  bool show_dbg_file
    = settings->value (ed_show_dbg_file).toBool ();

  if (openFileName.isEmpty ())
    {
      // This happens if edit is called without an argument
      // Open editor with empty edit area instead (as new file would do)
      request_new_file ("");
    }
  else
    {
      // Check whether this file is already open in the editor.
      file_editor_tab *tab = find_tab_widget (openFileName);

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

          if (show_dbg_file && ! ((breakpoint_marker || debug_pointer)
                                  && is_editor_console_tabbed ()))
            {
              emit fetab_set_focus (tab);
              activate ();
            }
        }
      else
        {
          if (! show_dbg_file && (breakpoint_marker  || debug_pointer))
            return;   // Do not open a file for showing dbg markers

          if (breakpoint_marker && ! insert)
            return;   // Never open a file when removing breakpoints

          file_editor_tab *fileEditorTab = nullptr;
          // Reuse <unnamed> tab if it hasn't yet been modified.
          bool reusing = false;
          tab = find_tab_widget ("");
          if (tab)
            {
              fileEditorTab = tab;
              if (fileEditorTab->qsci_edit_area ()->isModified ())
                fileEditorTab = nullptr;
              else
                reusing = true;
            }

          // If <unnamed> was absent or modified, create a new tab.
          if (! fileEditorTab)
            fileEditorTab = make_file_editor_tab ();

          fileEditorTab->set_encoding (encoding);
          QString result = fileEditorTab->load_file (openFileName);
          if (result == "")
            {
              // Supply empty title then have the file_editor_tab update
              // with full or short name.
              if (! reusing)
                add_file_editor_tab (fileEditorTab, "", index);
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
              if (! reusing)
                {
                  delete fileEditorTab;
                  fileEditorTab = nullptr;
                }

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

                  if (! settings->value (ed_create_new_file).toBool ())
                    {
                      msgBox = new QMessageBox (QMessageBox::Question,
                                                tr ("Octave Editor"),
                                                tr ("File\n%1\ndoes not exist. "
                                                    "Do you want to create it?").arg (openFileName),
                                                QMessageBox::NoButton, nullptr);
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

          if (! bookmarks.isEmpty ())
            {
              // Restore bookmarks
              for (const auto& bms : bookmarks.split (','))
                {
                  int bm = bms.toInt ();
                  if (fileEditorTab)
                    fileEditorTab->qsci_edit_area ()->markerAdd (bm, marker::bookmark);
                }
            }

          if (! ((breakpoint_marker || debug_pointer) && is_editor_console_tabbed ()))
            {
              // update breakpoint pointers, really show editor
              // and the current editor tab
              if (fileEditorTab)
                fileEditorTab->update_breakpoints ();
              activate ();
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
  toggle_preference (ed_show_line_numbers);
}

void file_editor::show_white_space (bool)
{
  toggle_preference (ed_show_white_space);
}

void file_editor::show_eol_chars (bool)
{
  toggle_preference (ed_show_eol_chars);
}

void file_editor::show_indent_guides (bool)
{
  toggle_preference (ed_show_indent_guides);
}

void file_editor::show_long_line (bool)
{
  toggle_preference (ed_long_line_marker);
}

void file_editor::show_toolbar (bool)
{
  toggle_preference (ed_show_toolbar);
}

void file_editor::show_statusbar (bool)
{
  toggle_preference (ed_show_edit_status_bar);
}

void file_editor::show_hscrollbar (bool)
{
  toggle_preference (ed_show_hscroll_bar);
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

  for (auto *a : all_actions)
    menu->removeAction (a);

  // add editor's actions with icons and customized shortcuts
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
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (settings->value (ed_hiding_closes_files).toBool ())
    {
      if (check_closing ())
        {
          // All tabs are closed without cancelling,
          // store closing state for restoring session when shown again.
          // Editor is closing when session data is stored in preferences
          m_closed = true;
          e->ignore ();
        }
      else
        {
          e->ignore ();
          return;
        }
    }
  else
    e->accept ();

  octave_dock_widget::closeEvent (e);
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
      for (const auto& url : e->mimeData ()->urls ())
        request_open_file (url.toLocalFile ());
    }
}

bool file_editor::is_editor_console_tabbed (void)
{
  // FIXME: is there a way to do this job that doesn't require casting
  // the parent to a main_window object?

  main_window *w = dynamic_cast<main_window *> (parentWidget ());

  if (w)
    {
      QList<QDockWidget *> w_list = w->tabifiedDockWidgets (this);
      QDockWidget *console =
        static_cast<QDockWidget *> (w->get_dock_widget_list ().at (0));

      for (int i = 0; i < w_list.count (); i++)
        {
          if (w_list.at (i) == console)
            return true;
        }
    }

  return false;
}

void file_editor::construct (void)
{
  QWidget *editor_widget = new QWidget (this);

  // FIXME: what was the intended purpose of this unused variable?
  // QStyle *editor_style = QApplication::style ();

  // Menu bar: do not set it native, required in macOS and Ubuntu Unity (Qt5)
  // for a visible menu bar in the editor widget.  This property is ignored
  // on other platforms.
  m_menu_bar = new QMenuBar (editor_widget);
  m_menu_bar->setNativeMenuBar (false);

  m_tool_bar = new QToolBar (editor_widget);
  m_tool_bar->setMovable (true);

  m_tab_widget = new file_editor_tab_widget (editor_widget, this);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  // the mru-list and an empty array of actions
  gui_settings *settings = rmgr.get_settings ();
  m_mru_files = settings->value (ed_mru_file_list).toStringList ();
  m_mru_files_encodings = settings->value (ed_mru_file_encodings)
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
    = add_action (m_fileMenu, rmgr.icon ("document-save"),
                  tr ("&Save File"), SLOT (request_save_file (bool)));

  m_save_as_action
    = add_action (m_fileMenu, rmgr.icon ("document-save-as"),
                  tr ("Save File &As..."),
                  SLOT (request_save_file_as (bool)));

  m_fileMenu->addSeparator ();

  m_close_action
    = add_action (m_fileMenu, rmgr.icon ("window-close", false),
                  tr ("&Close"), SLOT (request_close_file (bool)));

  m_close_all_action
    = add_action (m_fileMenu, rmgr.icon ("window-close", false),
                  tr ("Close All"), SLOT (request_close_all_files (bool)));

  m_close_others_action
    = add_action (m_fileMenu, rmgr.icon ("window-close", false),
                  tr ("Close Other Files"),
                  SLOT (request_close_other_files (bool)));

  m_fileMenu->addSeparator ();

  m_print_action
    = add_action (m_fileMenu, rmgr.icon ("document-print"),
                  tr ("Print..."), SLOT (request_print_file (bool)));

  // edit menu (undo, copy, paste and select all later via main window)

  m_edit_menu = add_menu (m_menu_bar, tr ("&Edit"));

  m_redo_action
    = add_action (m_edit_menu, rmgr.icon ("edit-redo"),
                  tr ("&Redo"), SLOT (request_redo (bool)));
  m_redo_action->setEnabled (false);

  m_edit_menu->addSeparator ();

  m_cut_action
    = add_action (m_edit_menu, rmgr.icon ("edit-cut"),
                  tr ("Cu&t"), SLOT (request_cut (bool)));
  m_cut_action->setEnabled (false);

  m_find_action
    = add_action (m_edit_menu, rmgr.icon ("edit-find-replace"),
                  tr ("&Find and Replace..."), SLOT (request_find (bool)));

  m_find_next_action
    = add_action (m_edit_menu, tr ("Find &Next..."),
                  SLOT (request_find_next (bool)));

  m_find_previous_action
    = add_action (m_edit_menu, tr ("Find &Previous..."),
                  SLOT (request_find_previous (bool)));

  m_edit_menu->addSeparator ();

  m_edit_cmd_menu = m_edit_menu->addMenu (tr ("&Commands"));

  m_delete_line_action
    = add_action (m_edit_cmd_menu, tr ("Delete Line"),
                  SLOT (request_delete_line (bool)));

  m_copy_line_action
    = add_action (m_edit_cmd_menu, tr ("Copy Line"),
                  SLOT (request_copy_line (bool)));

  m_cut_line_action
    = add_action (m_edit_cmd_menu, tr ("Cut Line"),
                  SLOT (request_cut_line (bool)));

  m_edit_cmd_menu->addSeparator ();

  m_delete_start_word_action
    = add_action (m_edit_cmd_menu, tr ("Delete to Start of Word"),
                  SLOT (request_delete_start_word (bool)));

  m_delete_end_word_action
    = add_action (m_edit_cmd_menu, tr ("Delete to End of Word"),
                  SLOT (request_delete_end_word (bool)));

  m_delete_start_line_action
    = add_action (m_edit_cmd_menu, tr ("Delete to Start of Line"),
                  SLOT (request_delete_start_line (bool)));

  m_delete_end_line_action
    = add_action (m_edit_cmd_menu, tr ("Delete to End of Line"),
                  SLOT (request_delete_end_line (bool)));

  m_edit_cmd_menu->addSeparator ();

  m_duplicate_selection_action
    = add_action (m_edit_cmd_menu, tr ("Duplicate Selection/Line"),
                  SLOT (request_duplicate_selection (bool)));

  m_transpose_line_action
    = add_action (m_edit_cmd_menu, tr ("Transpose Line"),
                  SLOT (request_transpose_line (bool)));

  m_edit_cmd_menu->addSeparator ();

  m_completion_action
    = add_action (m_edit_cmd_menu, tr ("&Show Completion List"),
                  SLOT (request_completion (bool)));

  m_edit_fmt_menu = m_edit_menu->addMenu (tr ("&Format"));

  m_upper_case_action
    = add_action (m_edit_fmt_menu, tr ("&Uppercase Selection"),
                  SLOT (request_upper_case (bool)));

  m_lower_case_action
    = add_action (m_edit_fmt_menu, tr ("&Lowercase Selection"),
                  SLOT (request_lower_case (bool)));

  m_edit_fmt_menu->addSeparator ();

  m_comment_selection_action
    = add_action (m_edit_fmt_menu, tr ("&Comment"),
                  SLOT (request_comment_selected_text (bool)));

  m_uncomment_selection_action
    = add_action (m_edit_fmt_menu, tr ("&Uncomment"),
                  SLOT (request_uncomment_selected_text (bool)));

  m_comment_var_selection_action
    = add_action (m_edit_fmt_menu, tr ("Comment (Choosing String)"),
                  SLOT (request_comment_var_selected_text (bool)));

  m_edit_fmt_menu->addSeparator ();

  m_indent_selection_action
    = add_action (m_edit_fmt_menu, tr ("&Indent Selection Rigidly"),
                  SLOT (request_indent_selected_text (bool)));

  m_unindent_selection_action
    = add_action (m_edit_fmt_menu, tr ("&Unindent Selection Rigidly"),
                  SLOT (request_unindent_selected_text (bool)));

  m_smart_indent_line_or_selection_action
    = add_action (m_edit_fmt_menu, tr ("Indent Code"),
                  SLOT (request_smart_indent_line_or_selected_text (void)));

  m_edit_fmt_menu->addSeparator ();

  m_conv_eol_windows_action
    = add_action (m_edit_fmt_menu,
                  tr ("Convert Line Endings to &Windows (CRLF)"),
                  SLOT (request_conv_eol_windows (bool)));

  m_conv_eol_unix_action
    = add_action (m_edit_fmt_menu, tr ("Convert Line Endings to &Unix (LF)"),
                  SLOT (request_conv_eol_unix (bool)));

  m_conv_eol_mac_action
    = add_action (m_edit_fmt_menu,
                  tr ("Convert Line Endings to Legacy &Mac (CR)"),
                  SLOT (request_conv_eol_mac (bool)));

  m_edit_nav_menu = m_edit_menu->addMenu (tr ("Navi&gation"));

  m_goto_line_action
    = add_action (m_edit_nav_menu, tr ("Go &to Line..."),
                  SLOT (request_goto_line (bool)));

  m_edit_cmd_menu->addSeparator ();

  m_move_to_matching_brace
    = add_action (m_edit_nav_menu, tr ("Move to Matching Brace"),
                  SLOT (request_move_match_brace (bool)));

  m_sel_to_matching_brace
    = add_action (m_edit_nav_menu, tr ("Select to Matching Brace"),
                  SLOT (request_sel_match_brace (bool)));

  m_edit_nav_menu->addSeparator ();

  m_next_bookmark_action
    = add_action (m_edit_nav_menu, tr ("&Next Bookmark"),
                  SLOT (request_next_bookmark (bool)));

  m_previous_bookmark_action
    = add_action (m_edit_nav_menu, tr ("Pre&vious Bookmark"),
                  SLOT (request_previous_bookmark (bool)));

  m_toggle_bookmark_action
    = add_action (m_edit_nav_menu, tr ("Toggle &Bookmark"),
                  SLOT (request_toggle_bookmark (bool)));

  m_remove_bookmark_action
    = add_action (m_edit_nav_menu, tr ("&Remove All Bookmarks"),
                  SLOT (request_remove_bookmark (bool)));

  m_edit_menu->addSeparator ();

  m_preferences_action
    = add_action (m_edit_menu, rmgr.icon ("preferences-system"),
                  tr ("&Preferences..."),
                  SLOT (request_preferences (bool)));

  m_styles_preferences_action
    = add_action (m_edit_menu, rmgr.icon ("preferences-system"),
                  tr ("&Styles Preferences..."),
                  SLOT (request_styles_preferences (bool)));

  // view menu

  QMenu *view_menu = add_menu (m_menu_bar, tr ("&View"));

  m_view_editor_menu = view_menu->addMenu (tr ("&Editor"));

  m_show_linenum_action
    = add_action (m_view_editor_menu, tr ("Show &Line Numbers"),
                  SLOT (show_line_numbers (bool)));
  m_show_linenum_action->setCheckable (true);

  m_show_whitespace_action
    = add_action (m_view_editor_menu, tr ("Show &Whitespace Characters"),
                  SLOT (show_white_space (bool)));
  m_show_whitespace_action->setCheckable (true);

  m_show_eol_action
    = add_action (m_view_editor_menu, tr ("Show Line &Endings"),
                  SLOT (show_eol_chars (bool)));
  m_show_eol_action->setCheckable (true);

  m_show_indguide_action
    = add_action (m_view_editor_menu, tr ("Show &Indentation Guides"),
                  SLOT (show_indent_guides (bool)));
  m_show_indguide_action->setCheckable (true);

  m_show_longline_action
    = add_action (m_view_editor_menu, tr ("Show Long Line &Marker"),
                  SLOT (show_long_line (bool)));
  m_show_longline_action->setCheckable (true);

  m_view_editor_menu->addSeparator ();

  m_show_toolbar_action
    = add_action (m_view_editor_menu, tr ("Show &Toolbar"),
                  SLOT (show_toolbar (bool)));
  m_show_toolbar_action->setCheckable (true);

  m_show_statusbar_action
    = add_action (m_view_editor_menu, tr ("Show &Statusbar"),
                  SLOT (show_statusbar (bool)));
  m_show_statusbar_action->setCheckable (true);

  m_show_hscrollbar_action
    = add_action (m_view_editor_menu, tr ("Show &Horizontal Scrollbar"),
                  SLOT (show_hscrollbar (bool)));
  m_show_hscrollbar_action->setCheckable (true);

  view_menu->addSeparator ();

  m_zoom_in_action
    = add_action (view_menu, rmgr.icon ("view-zoom-in"), tr ("Zoom &In"),
                  SLOT (zoom_in (bool)));

  m_zoom_out_action
    = add_action (view_menu, rmgr.icon ("view-zoom-out"), tr ("Zoom &Out"),
                  SLOT (zoom_out (bool)));

  m_zoom_normal_action
    = add_action (view_menu, rmgr.icon ("view-zoom-original"), tr ("&Normal Size"),
                  SLOT (zoom_normal (bool)));

  view_menu->addSeparator ();

  m_sort_tabs_action
    = add_action (view_menu, tr ("&Sort Tabs Alphabetically"),
                  SLOT (sort_tabs_alph (void)),
                  m_tab_widget->get_tab_bar ());

  m_menu_bar->addMenu (view_menu);

  // debug menu

  m_debug_menu = add_menu (m_menu_bar, tr ("&Debug"));

  m_toggle_breakpoint_action
    = add_action (m_debug_menu, rmgr.icon ("bp-toggle"),
                  tr ("Toggle &Breakpoint"),
                  SLOT (request_toggle_breakpoint (bool)));

  m_next_breakpoint_action
    = add_action (m_debug_menu, rmgr.icon ("bp-next"),
                  tr ("&Next Breakpoint"),
                  SLOT (request_next_breakpoint (bool)));

  m_previous_breakpoint_action
    = add_action (m_debug_menu, rmgr.icon ("bp-prev"),
                  tr ("Pre&vious Breakpoint"),
                  SLOT (request_previous_breakpoint (bool)));

  m_remove_all_breakpoints_action
    = add_action (m_debug_menu, rmgr.icon ("bp-rm-all"),
                  tr ("&Remove All Breakpoints"),
                  SLOT (request_remove_breakpoint (bool)));

  m_debug_menu->addSeparator ();

  // The other debug actions will be added by the main window.

  // run menu

  QMenu *_run_menu = add_menu (m_menu_bar, tr ("&Run"));

  m_run_action
    = add_action (_run_menu,
                  rmgr.icon ("system-run"),
                  tr ("Save File and Run / Continue"),
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
  popdown_button->setArrowType (Qt::DownArrow);
  popdown_button->setToolButtonStyle (Qt::ToolButtonTextOnly);

  // new and open actions are inserted later from main window
  m_popdown_mru_action = m_tool_bar->addWidget (popdown_button);
  m_tool_bar->addAction (m_save_action);
  m_tool_bar->addAction (m_save_as_action);
  m_tool_bar->addAction (m_print_action);
  m_tool_bar->addSeparator ();
  // m_undo_action: later via main window
  m_tool_bar->addAction (m_redo_action);
  m_tool_bar->addSeparator ();
  m_tool_bar->addAction (m_cut_action);
  // m_copy_action: later via the main window
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
  vbox_layout->setSpacing (0);
  editor_widget->setLayout (vbox_layout);
  setWidget (editor_widget);

  // Create the basic context menu of the tab bar with editor actions.
  // Actions for selecting an tab are added when the menu is activated.
  tab_bar *bar = m_tab_widget->get_tab_bar ();
  QMenu *ctx_men = bar->get_context_menu ();
  ctx_men->addSeparator ();
  ctx_men->addAction (m_close_action);
  ctx_men->addAction (m_close_all_action);
  ctx_men->addAction (m_close_others_action);
  ctx_men->addSeparator ();
  ctx_men->addAction (m_sort_tabs_action);
  add_action (ctx_men, tr ("Copy Full File &Path"),
              SLOT (copy_full_file_path (bool)), this);

  // signals
  connect (m_mru_file_menu, &QMenu::triggered,
           this, &file_editor::request_mru_open_file);

  mru_menu_update ();

  connect (m_tab_widget, &file_editor_tab_widget::tabCloseRequested,
           this, &file_editor::handle_tab_close_request);

  connect (m_tab_widget, &file_editor_tab_widget::currentChanged,
           this, &file_editor::active_tab_changed);

  resize (500, 400);
  set_title (tr ("Editor"));

  check_actions ();
}

// Slot when autocompletion list was cancelled
void file_editor::handle_autoc_cancelled (void)
{
  // List was cancelled but somehow still active and blocking the
  // edit area from accepting shortcuts. Only after another keypress
  // shortcuts and lists are working againnas expected. This is
  // probably caused by qt bug https://bugreports.qt.io/browse/QTBUG-83720
  // Hack: Accept the list, which is hidden but still active
  //       and undo the text insertion, if any

  file_editor_tab *f = reset_focus ();
  octave_qscintilla *qsci = f->qsci_edit_area ();

  int line, col;
  qsci->getCursorPosition (&line, &col);
  int l1 = qsci->lineLength (line); // Current line length

  // Accept autocompletion
  qsci->SendScintilla (QsciScintillaBase::SCI_AUTOCCOMPLETE);

  // Was text inserted? If yes, undo
  if (qsci->text (line).length () - l1)
    qsci->undo ();
}

file_editor_tab *file_editor::reset_focus (void)
{
  // Reset the focus of the tab and the related edit area
  file_editor_tab *f
    = static_cast<file_editor_tab *> (m_tab_widget->currentWidget ());
  emit fetab_set_focus (f);
  return f;
}

file_editor_tab *
file_editor::make_file_editor_tab (const QString& directory)
{
  file_editor_tab *f = new file_editor_tab (m_octave_qobj, directory);

  // signals from the qscintilla edit area
  connect (f->qsci_edit_area (), &octave_qscintilla::status_update,
           this, &file_editor::edit_status_update);

  connect (f->qsci_edit_area (), &octave_qscintilla::create_context_menu_signal,
           this, &file_editor::create_context_menu);

  connect (f->qsci_edit_area (),
           SIGNAL (SCN_AUTOCCOMPLETED (const char *, int, int, int)),
           this, SLOT (reset_focus (void)));

  connect (f->qsci_edit_area (), SIGNAL (SCN_AUTOCCANCELLED (void)),
           this, SLOT (handle_autoc_cancelled (void)));

  // signals from the qscintilla edit area
  connect (this, &file_editor::enter_debug_mode_signal,
           f->qsci_edit_area (), &octave_qscintilla::handle_enter_debug_mode);

  connect (this, &file_editor::exit_debug_mode_signal,
           f->qsci_edit_area (), &octave_qscintilla::handle_exit_debug_mode);

  // Signals from the file editor_tab
  connect (f, &file_editor_tab::autoc_closed,
           this, &file_editor::reset_focus);

  connect (f, &file_editor_tab::file_name_changed,
           this, &file_editor::handle_file_name_changed);

  connect (f, &file_editor_tab::editor_state_changed,
           this, &file_editor::handle_editor_state_changed);

  connect (f, &file_editor_tab::tab_remove_request,
           this, &file_editor::handle_tab_remove_request);

  connect (f, &file_editor_tab::editor_check_conflict_save,
           this, &file_editor::check_conflict_save);

  connect (f, &file_editor_tab::mru_add_file,
           this, &file_editor::handle_mru_add_file);

  connect (f, &file_editor_tab::request_open_file,
           this, [=] (const QString& fname, const QString& encoding) { request_open_file (fname, encoding); });

  connect (f, &file_editor_tab::edit_area_changed,
           this, &file_editor::edit_area_changed);

  connect (f, &file_editor_tab::set_focus_editor_signal,
           this, &file_editor::set_focus);

  // Signals from the file_editor or main-win non-trivial operations
  connect (this, &file_editor::fetab_settings_changed,
           f, [=] (const gui_settings *settings) { f->notice_settings (settings); });

  connect (this, &file_editor::fetab_change_request,
           f, &file_editor_tab::change_editor_state);

  connect (this, QOverload<const QWidget *, const QString&, bool>::of (&file_editor::fetab_save_file),
           f, QOverload<const QWidget *, const QString&, bool>::of (&file_editor_tab::save_file));

  // Signals from the file_editor trivial operations
  connect (this, &file_editor::fetab_recover_from_exit,
           f, &file_editor_tab::recover_from_exit);

  connect (this, &file_editor::fetab_set_directory,
           f, &file_editor_tab::set_current_directory);

  connect (this, &file_editor::fetab_zoom_in,
           f, &file_editor_tab::zoom_in);
  connect (this, &file_editor::fetab_zoom_out,
           f, &file_editor_tab::zoom_out);
  connect (this, &file_editor::fetab_zoom_normal,
           f, &file_editor_tab::zoom_normal);

  connect (this, &file_editor::fetab_context_help,
           f, &file_editor_tab::context_help);

  connect (this, &file_editor::fetab_context_edit,
           f, &file_editor_tab::context_edit);

  connect (this, QOverload<const QWidget *>::of (&file_editor::fetab_save_file),
           f, QOverload<const QWidget *>::of (&file_editor_tab::save_file));

  connect (this, &file_editor::fetab_save_file_as,
           f, QOverload<const QWidget *>::of (&file_editor_tab::save_file_as));

  connect (this, &file_editor::fetab_print_file,
           f, &file_editor_tab::print_file);

  connect (this, &file_editor::fetab_run_file,
           f, &file_editor_tab::run_file);

  connect (this, &file_editor::fetab_context_run,
           f, &file_editor_tab::context_run);

  connect (this, &file_editor::fetab_toggle_bookmark,
           f, &file_editor_tab::toggle_bookmark);

  connect (this, &file_editor::fetab_next_bookmark,
           f, &file_editor_tab::next_bookmark);

  connect (this, &file_editor::fetab_previous_bookmark,
           f, &file_editor_tab::previous_bookmark);

  connect (this, &file_editor::fetab_remove_bookmark,
           f, &file_editor_tab::remove_bookmark);

  connect (this, &file_editor::fetab_toggle_breakpoint,
           f, &file_editor_tab::toggle_breakpoint);

  connect (this, &file_editor::fetab_next_breakpoint,
           f, &file_editor_tab::next_breakpoint);

  connect (this, &file_editor::fetab_previous_breakpoint,
           f, &file_editor_tab::previous_breakpoint);

  connect (this, &file_editor::fetab_remove_all_breakpoints,
           f, &file_editor_tab::remove_all_breakpoints);

  connect (this, &file_editor::fetab_scintilla_command,
           f, &file_editor_tab::scintilla_command);

  connect (this, &file_editor::fetab_comment_selected_text,
           f, &file_editor_tab::comment_selected_text);

  connect (this, &file_editor::fetab_uncomment_selected_text,
           f, &file_editor_tab::uncomment_selected_text);

  connect (this, &file_editor::fetab_indent_selected_text,
           f, &file_editor_tab::indent_selected_text);

  connect (this, &file_editor::fetab_unindent_selected_text,
           f, &file_editor_tab::unindent_selected_text);

  connect (this, &file_editor::fetab_smart_indent_line_or_selected_text,
           f, &file_editor_tab::smart_indent_line_or_selected_text);

  connect (this, &file_editor::fetab_convert_eol,
           f, &file_editor_tab::convert_eol);

  connect (this, &file_editor::fetab_goto_line,
           f, &file_editor_tab::goto_line);

  connect (this, &file_editor::fetab_move_match_brace,
           f, &file_editor_tab::move_match_brace);

  connect (this, &file_editor::fetab_completion,
           f, &file_editor_tab::show_auto_completion);

  connect (this, &file_editor::fetab_set_focus,
           f, &file_editor_tab::set_focus);

  connect (this, &file_editor::fetab_insert_debugger_pointer,
           f, &file_editor_tab::insert_debugger_pointer);

  connect (this, &file_editor::fetab_delete_debugger_pointer,
           f, &file_editor_tab::delete_debugger_pointer);

  connect (this, &file_editor::fetab_do_breakpoint_marker,
           f, &file_editor_tab::do_breakpoint_marker);

  connect (this, &file_editor::update_gui_lexer_signal,
           f, &file_editor_tab::update_lexer_settings);

  // Convert other signals from the edit area and tab to editor signals.

  connect (f->qsci_edit_area (), &octave_qscintilla::execute_command_in_terminal_signal,
           this, &file_editor::execute_command_in_terminal_signal);

  connect (f->qsci_edit_area (), &octave_qscintilla::focus_console_after_command_signal,
           this, &file_editor::focus_console_after_command_signal);

  connect (f, &file_editor_tab::run_file_signal,
           this, &file_editor::run_file_signal);

  connect (f, &file_editor_tab::edit_mfile_request,
           this, &file_editor::edit_mfile_request);

  connect (f, &file_editor_tab::debug_quit_signal,
           this, &file_editor::debug_quit_signal);

  // Any interpreter_event signal from a file_editor_tab_widget is
  // handled the same as for the parent main_window object.

  connect (f, QOverload<const fcn_callback&>::of (&file_editor_tab::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&file_editor::interpreter_event));

  connect (f, QOverload<const meth_callback&>::of (&file_editor_tab::interpreter_event),
           this, QOverload<const meth_callback&>::of (&file_editor::interpreter_event));

  return f;
}

void file_editor::add_file_editor_tab (file_editor_tab *f, const QString& fn,
                                       int index)
{
  if (index == -1)
    m_tab_widget->addTab (f, fn);
  else
    m_tab_widget->insertTab (index, f, fn);

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
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  settings->setValue (ed_mru_file_list.key,  m_mru_files);
  settings->setValue (ed_mru_file_encodings.key,  m_mru_files_encodings);
  settings->sync ();
}

bool file_editor::call_custom_editor (const QString& file_name, int line)
{
  // Check if the user wants to use a custom file editor.
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings->value (global_use_custom_editor.key,
                       global_use_custom_editor.def).toBool ())
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

void file_editor::toggle_preference (const gui_pref& preference)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  bool old = settings->value (preference).toBool ();
  settings->setValue (preference.key, ! old);
  notice_settings (settings);
}

// Function for closing the files in a removed directory
void file_editor::handle_dir_remove (const QString& old_name,
                                     const QString& new_name)
{
  QDir old_dir (old_name);
  removed_file_data f_data;

  std::list<file_editor_tab *> editor_tab_lst = m_tab_widget->tab_list ();

  for (auto editor_tab : editor_tab_lst)
    {
      QString file_name = editor_tab->file_name ();

      if (file_name.isEmpty ())
        continue;   // Nothing to do, no valid file name

      // Get abs. file path and its path relative to the removed directory
      QString rel_path_to_file = old_dir.relativeFilePath (file_name);
      QString abs_path_to_file = old_dir.absoluteFilePath (file_name);

      // Test whether the file is located within the directory that will
      // be removed.  For this, two conditions must be met:
      // 1. The path of the file rel. to the dir is not equal to the
      //    its absolute one.
      //    If both are equal, then there is no relative path and removed
      //    directory and file are on different drives (e.g. on windows)
      // 2. The (real) relative path does not start with "../", i.e.,
      //    the file can be reached from the directory by descending only
      if ((rel_path_to_file != abs_path_to_file)
          && (rel_path_to_file.left (3) != QString ("../")))
        {
          // The currently considered file is included in the
          // removed/renamed diectory: remeber it
          if (editor_tab)
            {
              editor_tab->enable_file_watcher (false);
              f_data.editor_tab = editor_tab;

              // Add the new file path and the encoding for later reloading
              // if new_name is given
              if (! new_name.isEmpty ())
                {
                  QDir new_dir (new_name);
                  QString append_to_new_dir;
                  if (new_dir.exists ())
                    {
                      // The new directory already exists (movefile was used).
                      // This means, we have to add the name (not the path)
                      // of the old dir and the relative path to the file
                      // to new dir.
                      append_to_new_dir
                        = old_dir.dirName () + "/" + rel_path_to_file;
                    }
                  else
                    append_to_new_dir = rel_path_to_file;

                  f_data.new_file_name
                    = new_dir.absoluteFilePath (append_to_new_dir);
                }
              else
                f_data.new_file_name = ""; // no new name, just removing this file

              // Store data in list for later reloading
              m_tmp_closed_files << f_data;
            }
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
file_editor_tab *file_editor::find_tab_widget (const QString& file)
{
  std::string std_file = file.toStdString ();

  std::list<file_editor_tab *> fe_tab_lst = m_tab_widget->tab_list ();

  for (auto fe_tab : fe_tab_lst)
    {
      QString tab_file = fe_tab->file_name ();

      // We check file == tab_file because
      //
      //   same_file ("", "")
      //
      // is false

      if (same_file (std_file, tab_file.toStdString ()) || file == tab_file)
        return fe_tab;
    }

  return nullptr;
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

OCTAVE_END_NAMESPACE(octave)

#endif
