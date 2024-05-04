////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2024 The Octave Project Developers
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

#include <QCoreApplication>
#include <QSet>

#include "gui-preferences-sc.h"

// Note: Trying to shorten the uses of
//
//   QCoreApplication::translate (CONTEXT, STRING)
//
// by defining a macro won't work because lupdate won't know to look for
// the macro.

sc_pref sc_dock_widget_dock (QCoreApplication::translate ("shortcuts", "Undock/Dock Widget"), sc_dock_widget + ":dock", OCTAVE_QT_KEYCOMBINATION (CTRL_ALT, Qt::Key_D));
sc_pref sc_dock_widget_close (QCoreApplication::translate ("shortcuts", "Close Widget"), sc_dock_widget + ":close", OCTAVE_QT_KEYCOMBINATION (CTRL_ALT, Qt::Key_C));

// Main window menu

// file
sc_pref sc_main_file_new_file (QCoreApplication::translate ("shortcuts", "New File"), sc_main_file + ":new_file", QKeySequence::New);
sc_pref sc_main_file_new_function (QCoreApplication::translate ("shortcuts", "New Function"), sc_main_file + ":new_function", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_N));
sc_pref sc_main_file_new_figure (QCoreApplication::translate ("shortcuts", "New Figure"), sc_main_file + ":new_figure", QKeySequence::UnknownKey);
sc_pref sc_main_file_open_file (QCoreApplication::translate ("shortcuts", "Open File"), sc_main_file + ":open_file", QKeySequence::Open);
sc_pref sc_main_file_load_workspace (QCoreApplication::translate ("shortcuts", "Load Workspace"), sc_main_file + ":load_workspace", QKeySequence::UnknownKey);
sc_pref sc_main_file_save_workspace (QCoreApplication::translate ("shortcuts", "Save Workspace As"), sc_main_file + ":save_workspace", QKeySequence::UnknownKey);
sc_pref sc_main_file_exit (QCoreApplication::translate ("shortcuts", "Exit Octave"), sc_main_file + ":exit", QKeySequence::Quit);

// edit
sc_pref sc_main_edit_copy (QCoreApplication::translate ("shortcuts", "Copy"), sc_main_edit + ":copy", QKeySequence::Copy);
sc_pref sc_main_edit_paste (QCoreApplication::translate ("shortcuts", "Paste"), sc_main_edit + ":paste", QKeySequence::Paste);
sc_pref sc_main_edit_undo (QCoreApplication::translate ("shortcuts", "Undo"), sc_main_edit + ":undo", QKeySequence::Undo);
sc_pref sc_main_edit_select_all (QCoreApplication::translate ("shortcuts", "Select All"), sc_main_edit + ":select_all", QKeySequence::SelectAll);
sc_pref sc_main_edit_clear_clipboard (QCoreApplication::translate ("shortcuts", "Clear Clipboard"), sc_main_edit + ":clear_clipboard", QKeySequence::UnknownKey);
sc_pref sc_main_edit_find_in_files (QCoreApplication::translate ("shortcuts", "Find in Files"), sc_main_edit + ":find_in_files", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_F));
sc_pref sc_main_edit_clear_command_window (QCoreApplication::translate ("shortcuts", "Clear Command Window"), sc_main_edit + ":clear_command_window", QKeySequence::UnknownKey);
sc_pref sc_main_edit_clear_history (QCoreApplication::translate ("shortcuts", "Clear Command History"), sc_main_edit + ":clear_history", QKeySequence::UnknownKey);
sc_pref sc_main_edit_clear_workspace (QCoreApplication::translate ("shortcuts", "Clear Workspace"), sc_main_edit + ":clear_workspace", QKeySequence::UnknownKey);
sc_pref sc_main_edit_set_path (QCoreApplication::translate ("shortcuts", "Set Path"), sc_main_edit + ":set_path", QKeySequence::UnknownKey);
sc_pref sc_main_edit_preferences (QCoreApplication::translate ("shortcuts", "Preferences"), sc_main_edit + ":preferences", QKeySequence::UnknownKey);

// debug
sc_pref sc_main_debug_step_over (QCoreApplication::translate ("shortcuts", "Step"), sc_main_debug + ":step_over", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F10));
sc_pref sc_main_debug_step_into (QCoreApplication::translate ("shortcuts", "Step In"), sc_main_debug + ":step_into", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F11));
sc_pref sc_main_debug_step_out (QCoreApplication::translate ("shortcuts", "Step Out"), sc_main_debug + ":step_out", OCTAVE_QT_KEYCOMBINATION (PRE | Qt::ShiftModifier, Qt::Key_F11));
sc_pref sc_main_debug_continue (QCoreApplication::translate ("shortcuts", "Continue"), sc_main_debug + ":continue", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F5));
sc_pref sc_main_debug_quit (QCoreApplication::translate ("shortcuts", "Quit Debug Mode"), sc_main_debug + ":quit", OCTAVE_QT_KEYCOMBINATION (PRE | Qt::ShiftModifier, Qt::Key_F5));

// tools
sc_pref sc_main_tools_start_profiler (QCoreApplication::translate ("shortcuts", "Start/Stop Profiler Session"), sc_main_tools + ":start_profiler", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_P));
sc_pref sc_main_tools_resume_profiler (QCoreApplication::translate ("shortcuts", "Resume Profiler Session"), sc_main_tools + ":resume_profiler", QKeySequence::UnknownKey);
sc_pref sc_main_tools_show_profiler (QCoreApplication::translate ("shortcuts", "Show Profile Data"), sc_main_tools + ":show_profiler", OCTAVE_QT_KEYCOMBINATION (Qt::AltModifier | Qt::ShiftModifier, Qt::Key_P));


// window
sc_pref sc_main_window_show_command (QCoreApplication::translate ("shortcuts", "Show Command Window"), sc_main_window + ":show_command", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_0));
sc_pref sc_main_window_show_history (QCoreApplication::translate ("shortcuts", "Show Command History"), sc_main_window + ":show_history", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_1));
sc_pref sc_main_window_show_file_browser (QCoreApplication::translate ("shortcuts", "Show File Browser"), sc_main_window + ":show_file_browser", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_2));
sc_pref sc_main_window_show_workspace (QCoreApplication::translate ("shortcuts", "Show Workspace"), sc_main_window + ":show_workspace", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_3));
sc_pref sc_main_window_show_editor (QCoreApplication::translate ("shortcuts", "Show Editor"), sc_main_window + ":show_editor", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_4));
sc_pref sc_main_window_show_doc (QCoreApplication::translate ("shortcuts", "Show Documentation"), sc_main_window + ":show_doc", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_5));
sc_pref sc_main_window_show_variable_editor (QCoreApplication::translate ("shortcuts", "Show Variable Editor"), sc_main_window + ":show_variable_editor", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_SHIFT, Qt::Key_6));
sc_pref sc_main_window_command (QCoreApplication::translate ("shortcuts", "Command Window"), sc_main_window + ":command", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_0));
sc_pref sc_main_window_history (QCoreApplication::translate ("shortcuts", "Command History"), sc_main_window + ":history", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_1));
sc_pref sc_main_window_file_browser (QCoreApplication::translate ("shortcuts", "File Browser"), sc_main_window + ":file_browser", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_2));
sc_pref sc_main_window_workspace (QCoreApplication::translate ("shortcuts", "Workspace"), sc_main_window + ":workspace", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_3));
sc_pref sc_main_window_editor (QCoreApplication::translate ("shortcuts", "Editor"), sc_main_window + ":editor", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_4));
sc_pref sc_main_window_doc (QCoreApplication::translate ("shortcuts", "Documentation"), sc_main_window + ":doc", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_5));
sc_pref sc_main_window_variable_editor (QCoreApplication::translate ("shortcuts", "Variable Editor"), sc_main_window + ":variable_editor", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL, Qt::Key_6));
sc_pref sc_main_window_previous_dock (QCoreApplication::translate ("shortcuts", "Previous Widget"), sc_main_window + ":previous_widget", OCTAVE_QT_KEYCOMBINATION (PRE | CTRL_ALT, Qt::Key_P));
sc_pref sc_main_window_reset (QCoreApplication::translate ("shortcuts", "Reset Default Window Layout"), sc_main_window + ":reset", QKeySequence::UnknownKey);

// help
sc_pref sc_main_help_ondisk_doc (QCoreApplication::translate ("shortcuts", "Show On-disk Documentation"), sc_main_help + ":ondisk_doc", QKeySequence::UnknownKey);
sc_pref sc_main_help_online_doc (QCoreApplication::translate ("shortcuts", "Show Online Documentation"), sc_main_help + ":online_doc", QKeySequence::UnknownKey);
sc_pref sc_main_help_report_bug (QCoreApplication::translate ("shortcuts", "Report Bug"), sc_main_help + ":report_bug", QKeySequence::UnknownKey);
sc_pref sc_main_help_packages (QCoreApplication::translate ("shortcuts", "Octave Packages"), sc_main_help + ":packages", QKeySequence::UnknownKey);
sc_pref sc_main_help_contribute (QCoreApplication::translate ("shortcuts", "Contribute to Octave"), sc_main_help + ":contribute", QKeySequence::UnknownKey);
sc_pref sc_main_help_developer (QCoreApplication::translate ("shortcuts", "Octave Developer Resources"), sc_main_help + ":developer", QKeySequence::UnknownKey);
sc_pref sc_main_help_about (QCoreApplication::translate ("shortcuts", "About Octave"), sc_main_help + ":about", QKeySequence::UnknownKey);

// news
sc_pref sc_main_news_release_notes (QCoreApplication::translate ("shortcuts", "Release Notes"), sc_main_news + ":release_notes", QKeySequence::UnknownKey);
sc_pref sc_main_news_community_news (QCoreApplication::translate ("shortcuts", "Community News"), sc_main_news + ":community_news", QKeySequence::UnknownKey);

// Tab handling
// The following shortcuts are moved into a separate tab.  The key names
// are not change for preserving compatibility with older versions
sc_pref sc_edit_file_close (QCoreApplication::translate ("shortcuts", "Close Tab"), sc_edit_file_cl, QKeySequence::Close);
sc_pref sc_edit_file_close_all (QCoreApplication::translate ("shortcuts", "Close All Tabs"), sc_edit_file_cl + "_all", QKeySequence::UnknownKey);
sc_pref sc_edit_file_close_other (QCoreApplication::translate ("shortcuts", "Close Other Tabs"), sc_edit_file_cl + "_other", QKeySequence::UnknownKey);
sc_pref sc_edit_tabs_switch_left_tab (QCoreApplication::translate ("shortcuts", "Switch to Left Tab"), sc_edit_tabs + ":switch_left_tab", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_PageUp));
sc_pref sc_edit_tabs_switch_right_tab (QCoreApplication::translate ("shortcuts", "Switch to Right Tab"), sc_edit_tabs + ":switch_right_tab", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_PageDown));
sc_pref sc_edit_tabs_move_tab_left (QCoreApplication::translate ("shortcuts", "Move Tab Left"), sc_edit_tabs + ":move_tab_left", OCTAVE_QT_KEYCOMBINATION (Qt::AltModifier, Qt::Key_PageUp));
sc_pref sc_edit_tabs_move_tab_right (QCoreApplication::translate ("shortcuts", "Move Tab Right"), sc_edit_tabs + ":move_tab_right", OCTAVE_QT_KEYCOMBINATION (Qt::AltModifier, Qt::Key_PageDown));

// Zooming
sc_pref sc_edit_view_zoom_in (QCoreApplication::translate ("shortcuts", "Zoom In"), sc_edit_view_zoom + "_in", QKeySequence::ZoomIn);
sc_pref sc_edit_view_zoom_out (QCoreApplication::translate ("shortcuts", "Zoom Out"), sc_edit_view_zoom + "_out", QKeySequence::ZoomOut);
#if defined (Q_OS_MAC)
sc_pref sc_edit_view_zoom_normal (QCoreApplication::translate ("shortcuts", "Zoom Normal"), sc_edit_view_zoom + "_normal", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_Underscore));
#else
sc_pref sc_edit_view_zoom_normal (QCoreApplication::translate ("shortcuts", "Zoom Normal"), sc_edit_view_zoom + "_normal", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_Period));
#endif

// Actions of the editor

// file
sc_pref sc_edit_file_edit_function (QCoreApplication::translate ("shortcuts", "Edit Function"), sc_edit_file + ":edit_function", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_E));
sc_pref sc_edit_file_save (QCoreApplication::translate ("shortcuts", "Save File"), sc_edit_file + ":save", QKeySequence::Save);
sc_pref sc_edit_file_save_as (QCoreApplication::translate ("shortcuts", "Save File As"), sc_edit_file + ":save_as", QKeySequence::SaveAs);
sc_pref sc_edit_file_print (QCoreApplication::translate ("shortcuts", "Print"), sc_edit_file + ":print", QKeySequence::Print);

// edit
sc_pref sc_edit_edit_redo (QCoreApplication::translate ("shortcuts", "Redo"), sc_edit_edit + ":redo", QKeySequence::Redo);
sc_pref sc_edit_edit_cut (QCoreApplication::translate ("shortcuts", "Cut"), sc_edit_edit + ":cut", QKeySequence::Cut);
sc_pref sc_edit_edit_find_replace (QCoreApplication::translate ("shortcuts", "Find and Replace"), sc_edit_edit_find + "_replace", QKeySequence::Find);
sc_pref sc_edit_edit_find_next (QCoreApplication::translate ("shortcuts", "Find Next"), sc_edit_edit_find + "_next", QKeySequence::FindNext);
sc_pref sc_edit_edit_find_previous (QCoreApplication::translate ("shortcuts", "Find Previous"), sc_edit_edit_find + "_previous", QKeySequence::FindPrevious);
sc_pref sc_edit_edit_delete_start_word (QCoreApplication::translate ("shortcuts", "Delete to Start of Word"), sc_edit_edit + ":delete_start_word", QKeySequence::DeleteStartOfWord);
sc_pref sc_edit_edit_delete_end_word (QCoreApplication::translate ("shortcuts", "Delete to End of Word"), sc_edit_edit + ":delete_end_word", QKeySequence::DeleteEndOfWord);
sc_pref sc_edit_edit_delete_start_line (QCoreApplication::translate ("shortcuts", "Delete to Start of Line"), sc_edit_edit + ":delete_start_line", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_Backspace));
sc_pref sc_edit_edit_delete_end_line (QCoreApplication::translate ("shortcuts", "Delete to End of Line"), sc_edit_edit + ":delete_end_line", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_Delete));
sc_pref sc_edit_edit_delete_line (QCoreApplication::translate ("shortcuts", "Delete Line"), sc_edit_edit + ":delete_line", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_L));
sc_pref sc_edit_edit_copy_line (QCoreApplication::translate ("shortcuts", "Copy Line"), sc_edit_edit + ":copy_line", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_C));
sc_pref sc_edit_edit_cut_line (QCoreApplication::translate ("shortcuts", "Cut Line"), sc_edit_edit + ":cut_line", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_X));
sc_pref sc_edit_edit_duplicate_selection (QCoreApplication::translate ("shortcuts", "Duplicate Selection/Line"), sc_edit_edit + ":duplicate_selection", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_D));
sc_pref sc_edit_edit_transpose_line (QCoreApplication::translate ("shortcuts", "Transpose Line"), sc_edit_edit + ":transpose_line", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_T));
sc_pref sc_edit_edit_completion_list (QCoreApplication::translate ("shortcuts", "Show Completion List"), sc_edit_edit + ":completion_list", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_Space));

sc_pref sc_edit_edit_comment_selection (QCoreApplication::translate ("shortcuts", "Comment Selection"), sc_edit_edit + ":comment_selection", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_R));
sc_pref sc_edit_edit_uncomment_selection (QCoreApplication::translate ("shortcuts", "Uncomment Selection"), sc_edit_edit + ":uncomment_selection", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_R));
sc_pref sc_edit_edit_comment_var_selection (QCoreApplication::translate ("shortcuts", "Comment Selection (Choosing String)"), sc_edit_edit + ":comment_var_selection", OCTAVE_QT_KEYCOMBINATION (CTRL_ALT, Qt::Key_R));
sc_pref sc_edit_edit_upper_case (QCoreApplication::translate ("shortcuts", "Uppercase Selection"), sc_edit_edit + ":upper_case", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_U));
sc_pref sc_edit_edit_lower_case (QCoreApplication::translate ("shortcuts", "Lowercase Selection"), sc_edit_edit + ":lower_case", OCTAVE_QT_KEYCOMBINATION (CTRL_ALT, Qt::Key_U));

#if defined (Q_OS_MAC)
sc_pref sc_edit_edit_indent_selection (QCoreApplication::translate ("shortcuts", "Indent Selection Rigidly"), sc_edit_edit + ":indent_selection", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_Tab));
sc_pref sc_edit_edit_unindent_selection (QCoreApplication::translate ("shortcuts", "Unindent Selection Rigidly"), sc_edit_edit + ":unindent_selection", OCTAVE_QT_KEYCOMBINATION (PRE | Qt::ShiftModifier, Qt::Key_Tab));
#else
sc_pref sc_edit_edit_indent_selection (QCoreApplication::translate ("shortcuts", "Indent Selection Rigidly"), sc_edit_edit + ":indent_selection", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_Tab));
sc_pref sc_edit_edit_unindent_selection (QCoreApplication::translate ("shortcuts", "Unindent Selection Rigidly"), sc_edit_edit + ":unindent_selection", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_Tab));
#endif
sc_pref sc_edit_edit_smart_indent_line_or_selection (QCoreApplication::translate ("shortcuts", "Indent Code"), sc_edit_edit + ":smart_indent_line_or_selection", QKeySequence::UnknownKey);

sc_pref sc_edit_edit_conv_eol_winows (QCoreApplication::translate ("shortcuts", "Convert Line Endings to Windows"), sc_edit_edit + ":conv_eol_winows", QKeySequence::UnknownKey);
sc_pref sc_edit_edit_conv_eol_unix (QCoreApplication::translate ("shortcuts", "Convert Line Endings to Unix"), sc_edit_edit + ":conv_eol_unix", QKeySequence::UnknownKey);
sc_pref sc_edit_edit_conv_eol_mac (QCoreApplication::translate ("shortcuts", "Convert Line Endings to Mac"), sc_edit_edit + ":conv_eol_mac", QKeySequence::UnknownKey);

sc_pref sc_edit_edit_goto_line (QCoreApplication::translate ("shortcuts", "Goto Line"), sc_edit_edit + ":goto_line", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_L));
sc_pref sc_edit_edit_move_to_brace (QCoreApplication::translate ("shortcuts", "Move to Matching Brace"), sc_edit_edit + ":move_to_brace", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_M));
sc_pref sc_edit_edit_select_to_brace (QCoreApplication::translate ("shortcuts", "Select to Matching Brace"), sc_edit_edit + ":select_to_brace", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_M));
sc_pref sc_edit_edit_toggle_bookmark (QCoreApplication::translate ("shortcuts", "Toggle Bookmark"), sc_edit_edit + ":toggle_bookmark", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F7));
sc_pref sc_edit_edit_next_bookmark (QCoreApplication::translate ("shortcuts", "Next Bookmark"), sc_edit_edit + ":next_bookmark", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F2));
sc_pref sc_edit_edit_previous_bookmark (QCoreApplication::translate ("shortcuts", "Previous Bookmark"), sc_edit_edit + ":previous_bookmark", OCTAVE_QT_KEYCOMBINATION (PRE | Qt::ShiftModifier, Qt::Key_F2));
sc_pref sc_edit_edit_remove_bookmark (QCoreApplication::translate ("shortcuts", "Remove All Bookmark"), sc_edit_edit + ":remove_bookmark", QKeySequence::UnknownKey);

sc_pref sc_edit_edit_preferences (QCoreApplication::translate ("shortcuts", "Preferences"), sc_edit_edit + ":preferences", QKeySequence::UnknownKey);
sc_pref sc_edit_edit_styles_preferences (QCoreApplication::translate ("shortcuts", "Styles Preferences"), sc_edit_edit + ":styles_preferences", QKeySequence::UnknownKey);

// view
sc_pref sc_edit_view_show_line_numbers (QCoreApplication::translate ("shortcuts", "Show Line Numbers"), sc_edit_view + ":show_line_numbers", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_white_spaces (QCoreApplication::translate ("shortcuts", "Show Whitespace Characters"), sc_edit_view + ":show_white_spaces", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_eol_chars (QCoreApplication::translate ("shortcuts", "Show Line Endings"), sc_edit_view + ":show_eol_chars", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_ind_guides (QCoreApplication::translate ("shortcuts", "Show Indentation Guides"), sc_edit_view + ":show_ind_guides", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_long_line (QCoreApplication::translate ("shortcuts", "Show Long Line Marker"), sc_edit_view + ":show_long_line", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_toolbar (QCoreApplication::translate ("shortcuts", "Show Toolbar"), sc_edit_view + ":show_toolbar", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_statusbar (QCoreApplication::translate ("shortcuts", "Show Statusbar"), sc_edit_view + ":show_statusbar", QKeySequence::UnknownKey);
sc_pref sc_edit_view_show_hscrollbar (QCoreApplication::translate ("shortcuts", "Show Horizontal Scrollbar"), sc_edit_view + ":show_hscrollbar", QKeySequence::UnknownKey);
sc_pref sc_edit_view_sort_tabs (QCoreApplication::translate ("shortcuts", "Sort Tabs Alphabetically"), sc_edit_view + ":sort_tabs", QKeySequence::UnknownKey);

// debug
sc_pref sc_edit_debug_toggle_breakpoint (QCoreApplication::translate ("shortcuts", "Toggle Breakpoint"), sc_edit_debug + ":toggle_breakpoint", QKeySequence::UnknownKey);
sc_pref sc_edit_debug_next_breakpoint (QCoreApplication::translate ("shortcuts", "Next Breakpoint"), sc_edit_debug + ":next_breakpoint", QKeySequence::UnknownKey);
sc_pref sc_edit_debug_previous_breakpoint (QCoreApplication::translate ("shortcuts", "Previous Breakpoint"), sc_edit_debug + ":previous_breakpoint", QKeySequence::UnknownKey);
sc_pref sc_edit_debug_remove_breakpoints (QCoreApplication::translate ("shortcuts", "Remove All Breakpoints"), sc_edit_debug + ":remove_breakpoints", QKeySequence::UnknownKey);

// run
sc_pref sc_edit_run_run_file (QCoreApplication::translate ("shortcuts", "Run File"), sc_edit_run + ":run_file", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F5));
sc_pref sc_edit_run_run_selection (QCoreApplication::translate ("shortcuts", "Run Selection"), sc_edit_run + ":run_selection", OCTAVE_QT_KEYCOMBINATION (PRE, Qt::Key_F9));
sc_pref sc_edit_run_run_tests (QCoreApplication::translate ("shortcuts", "Run Tests"), sc_edit_run + ":run_file", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_F5));
sc_pref sc_edit_run_run_demos (QCoreApplication::translate ("shortcuts", "Run Demos"), sc_edit_run + ":run_file", OCTAVE_QT_KEYCOMBINATION (CTRL_SHIFT, Qt::Key_F5));

// help
sc_pref sc_edit_help_help_keyword (QCoreApplication::translate ("shortcuts", "Help on Keyword"), sc_edit_help + ":help_keyword", QKeySequence::HelpContents);
sc_pref sc_edit_help_doc_keyword (QCoreApplication::translate ("shortcuts", "Document on Keyword"), sc_edit_help + ":doc_keyword", OCTAVE_QT_KEYCOMBINATION (Qt::SHIFT, Qt::Key_F1));


// Documentation browser
sc_pref sc_doc_go_home (QCoreApplication::translate ("shortcuts", "Go to Homepage"), sc_doc + ":go_home", OCTAVE_QT_KEYCOMBINATION (Qt::AltModifier, Qt::Key_Home));
sc_pref sc_doc_go_back (QCoreApplication::translate ("shortcuts", "Go Back one Page"), sc_doc + ":go_back", QKeySequence::Back);
sc_pref sc_doc_go_next (QCoreApplication::translate ("shortcuts", "Go Forward one Page"), sc_doc + ":go_next", QKeySequence::Forward);
sc_pref sc_doc_bookmark (QCoreApplication::translate ("shortcuts", "Bookmark this Page"), sc_doc + ":bookmark", OCTAVE_QT_KEYCOMBINATION (CTRL, Qt::Key_D));

QString
get_shortcut_section (const QString& key)
{
  QString section;

  if (key.contains (':'))
    section = key.section (':', 0, 0, QString::SectionSkipEmpty);

  return section;
}
