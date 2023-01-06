////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if ! defined (octave_gui_preferences_sc_h)
#define octave_gui_preferences_sc_h 1

#include "gui-preferences.h"

// Define shortcuts

// The shortcut's default values are given as QKeySequence for being able
// to use platform independent standard keys (QKeySequence::StandardKey).
// However, converting key sequences into QVariants does not seem to be
// revertible.  In addition the related string (which is saved in the
// preferences file) can not be determined during compile time since the
// result depends on the platform (at least in case of standard key sequences
// like, e.g., QKeySequence::Copy)
// Therefore, these prefs for key sequences require a separate constant
// definition and value method for the settings class.

#if defined (Q_OS_MAC)
// Use CMD key as an equivalent of Ctrl key on other platforms
const Qt::KeyboardModifier CTRL = Qt::MetaModifier;
// Some of octave default shortcuts on windows/linux are already defined
// as system wide shortcuts on Mac Os X (almost all Function keys).
// Prefix those with Option (Alt) modifier to avoid conflicts.
const int PRE = Qt::AltModifier;
#else
const Qt::KeyboardModifier CTRL = Qt::ControlModifier;
const int PRE = Qt::NoModifier;
#endif

const Qt::KeyboardModifiers CTRL_SHIFT = CTRL | Qt::ShiftModifier;
const Qt::KeyboardModifiers CTRL_ALT = CTRL | Qt::AltModifier;

// Shortcuts not related to specific Menus

// Dock widgets
const QString sc_dock_widget ("dock_widget");
const sc_pref sc_dock_widget_dock (sc_dock_widget + ":dock", CTRL_ALT + Qt::Key_D);
const sc_pref sc_dock_widget_close (sc_dock_widget + ":close", CTRL_ALT + Qt::Key_C);

// Main window menu

// file
const QString sc_main_file ("main_file");
const sc_pref sc_main_file_new_file (sc_main_file + ":new_file", QKeySequence::New);
const sc_pref sc_main_file_new_function (sc_main_file + ":new_function", CTRL_SHIFT + Qt::Key_N);
const sc_pref sc_main_file_new_figure (sc_main_file + ":new_figure", QKeySequence::UnknownKey);
const sc_pref sc_main_file_open_file (sc_main_file + ":open_file", QKeySequence::Open);
const sc_pref sc_main_file_load_workspace (sc_main_file + ":load_workspace", QKeySequence::UnknownKey);
const sc_pref sc_main_file_save_workspace (sc_main_file + ":save_workspace", QKeySequence::UnknownKey);
const sc_pref sc_main_file_exit (sc_main_file + ":exit", QKeySequence::Quit);

// edit
const QString sc_main_edit ("main_edit");
const sc_pref sc_main_edit_copy (sc_main_edit + ":copy", QKeySequence::Copy);
const sc_pref sc_main_edit_paste (sc_main_edit + ":paste", QKeySequence::Paste);
const sc_pref sc_main_edit_undo (sc_main_edit + ":undo", QKeySequence::Undo);
const sc_pref sc_main_edit_select_all (sc_main_edit + ":select_all", QKeySequence::SelectAll);
const sc_pref sc_main_edit_clear_clipboard (sc_main_edit + ":clear_clipboard", QKeySequence::UnknownKey);
const sc_pref sc_main_edit_find_in_files (sc_main_edit + ":find_in_files", CTRL_SHIFT + Qt::Key_F);
const sc_pref sc_main_edit_clear_command_window (sc_main_edit + ":clear_command_window", QKeySequence::UnknownKey);
const sc_pref sc_main_edit_clear_history (sc_main_edit + ":clear_history", QKeySequence::UnknownKey);
const sc_pref sc_main_edit_clear_workspace (sc_main_edit + ":clear_workspace", QKeySequence::UnknownKey);
const sc_pref sc_main_edit_set_path (sc_main_edit + ":set_path", QKeySequence::UnknownKey);
const sc_pref sc_main_edit_preferences (sc_main_edit + ":preferences", QKeySequence::UnknownKey);

// debug
const QString sc_main_debug ("main_debug");
const sc_pref sc_main_debug_step_over (sc_main_debug + ":step_over", PRE + Qt::Key_F10);
const sc_pref sc_main_debug_step_into (sc_main_debug + ":step_into", PRE + Qt::Key_F11);
const sc_pref sc_main_debug_step_out (sc_main_debug + ":step_out", PRE + Qt::ShiftModifier + Qt::Key_F11);
const sc_pref sc_main_debug_continue (sc_main_debug + ":continue", PRE + Qt::Key_F5);
const sc_pref sc_main_debug_quit (sc_main_debug + ":quit", PRE + Qt::ShiftModifier + Qt::Key_F5);

// tools
const QString sc_main_tools ("main_tools");
const sc_pref sc_main_tools_start_profiler (sc_main_tools + ":start_profiler", CTRL_SHIFT + Qt::Key_P);
const sc_pref sc_main_tools_resume_profiler (sc_main_tools + ":resume_profiler", QKeySequence::UnknownKey);
const sc_pref sc_main_tools_show_profiler (sc_main_tools + ":show_profiler", Qt::AltModifier + Qt::ShiftModifier + Qt::Key_P);


// window
const QString sc_main_window ("main_window");
const sc_pref sc_main_window_show_command (sc_main_window + ":show_command", PRE + CTRL_SHIFT + Qt::Key_0);
const sc_pref sc_main_window_show_history (sc_main_window + ":show_history", PRE + CTRL_SHIFT + Qt::Key_1);
const sc_pref sc_main_window_show_file_browser (sc_main_window + ":show_file_browser", PRE + CTRL_SHIFT + Qt::Key_2);
const sc_pref sc_main_window_show_workspace (sc_main_window + ":show_workspace", PRE + CTRL_SHIFT + Qt::Key_3);
const sc_pref sc_main_window_show_editor (sc_main_window + ":show_editor", PRE + CTRL_SHIFT + Qt::Key_4);
const sc_pref sc_main_window_show_doc (sc_main_window + ":show_doc", PRE + CTRL_SHIFT + Qt::Key_5);
const sc_pref sc_main_window_show_variable_editor (sc_main_window + ":show_variable_editor", PRE + CTRL_SHIFT + Qt::Key_6);
const sc_pref sc_main_window_command (sc_main_window + ":command", PRE + CTRL + Qt::Key_0);
const sc_pref sc_main_window_history (sc_main_window + ":history", PRE + CTRL + Qt::Key_1);
const sc_pref sc_main_window_file_browser (sc_main_window + ":file_browser", PRE + CTRL + Qt::Key_2);
const sc_pref sc_main_window_workspace (sc_main_window + ":workspace", PRE + CTRL + Qt::Key_3);
const sc_pref sc_main_window_editor (sc_main_window + ":editor", PRE + CTRL + Qt::Key_4);
const sc_pref sc_main_window_doc (sc_main_window + ":doc", PRE + CTRL + Qt::Key_5);
const sc_pref sc_main_window_variable_editor (sc_main_window + ":variable_editor", PRE + CTRL + Qt::Key_6);
const sc_pref sc_main_window_previous_dock (sc_main_window + ":previous_widget", PRE + CTRL_ALT + Qt::Key_P);
const sc_pref sc_main_window_reset (sc_main_window + ":reset", QKeySequence::UnknownKey);

// help
const QString sc_main_help ("main_help");
const sc_pref sc_main_help_ondisk_doc (sc_main_help + ":ondisk_doc", QKeySequence::UnknownKey);
const sc_pref sc_main_help_online_doc (sc_main_help + ":online_doc", QKeySequence::UnknownKey);
const sc_pref sc_main_help_report_bug (sc_main_help + ":report_bug", QKeySequence::UnknownKey);
const sc_pref sc_main_help_packages (sc_main_help + ":packages", QKeySequence::UnknownKey);
const sc_pref sc_main_help_contribute (sc_main_help + ":contribute", QKeySequence::UnknownKey);
const sc_pref sc_main_help_developer (sc_main_help + ":developer", QKeySequence::UnknownKey);
const sc_pref sc_main_help_about (sc_main_help + ":about", QKeySequence::UnknownKey);

// news
const QString sc_main_news ("main_news");
const sc_pref sc_main_news_release_notes (sc_main_news + ":release_notes", QKeySequence::UnknownKey);
const sc_pref sc_main_news_community_news (sc_main_news + ":community_news", QKeySequence::UnknownKey);

// Tab handling
// The following shortcuts are moved into a separate tab.  The key names
// are not change for preserving compatibility with older versions
const QString sc_edit_file ("editor_file");
const QString sc_edit_file_cl (sc_edit_file + ":close");
const sc_pref sc_edit_file_close (sc_edit_file_cl, QKeySequence::Close);
const sc_pref sc_edit_file_close_all (sc_edit_file_cl + "_all", QKeySequence::UnknownKey);
const sc_pref sc_edit_file_close_other (sc_edit_file_cl + "_other", QKeySequence::UnknownKey);
const QString sc_edit_tabs ("editor_tabs");
const sc_pref sc_edit_tabs_switch_left_tab (sc_edit_tabs + ":switch_left_tab", CTRL + Qt::Key_PageUp);
const sc_pref sc_edit_tabs_switch_right_tab (sc_edit_tabs + ":switch_right_tab", CTRL + Qt::Key_PageDown);
const sc_pref sc_edit_tabs_move_tab_left (sc_edit_tabs + ":move_tab_left", Qt::AltModifier + Qt::Key_PageUp);
const sc_pref sc_edit_tabs_move_tab_right (sc_edit_tabs + ":move_tab_right", Qt::AltModifier + Qt::Key_PageDown);

// Zooming
const QString sc_edit_zoom ("editor_zoom"); // only a group name in the pref dialog
const QString sc_edit_view ("editor_view");
const QString sc_edit_view_zoom (sc_edit_view + ":zoom");
const sc_pref sc_edit_view_zoom_in (sc_edit_view_zoom + "_in", QKeySequence::ZoomIn);
const sc_pref sc_edit_view_zoom_out (sc_edit_view_zoom + "_out", QKeySequence::ZoomOut);
#if defined (Q_OS_MAC)
const sc_pref sc_edit_view_zoom_normal (sc_edit_view_zoom + "_normal", CTRL + Qt::Key_Underscore);
#else
const sc_pref sc_edit_view_zoom_normal (sc_edit_view_zoom + "_normal", CTRL + Qt::Key_Period);
#endif

// Actions of the editor

// file
const sc_pref sc_edit_file_edit_function (sc_edit_file + ":edit_function", CTRL + Qt::Key_E);
const sc_pref sc_edit_file_save (sc_edit_file + ":save", QKeySequence::Save);
const sc_pref sc_edit_file_save_as (sc_edit_file + ":save_as", QKeySequence::SaveAs);
const sc_pref sc_edit_file_print (sc_edit_file + ":print", QKeySequence::Print);

// edit
const QString sc_edit_find ("editor_find"); // only a group name in the pref dialog
const QString sc_edit_edit ("editor_edit");
const QString sc_edit_edit_find (sc_edit_edit + ":find");
const sc_pref sc_edit_edit_redo (sc_edit_edit + ":redo", QKeySequence::Redo);
const sc_pref sc_edit_edit_cut (sc_edit_edit + ":cut", QKeySequence::Cut);
const sc_pref sc_edit_edit_find_replace (sc_edit_edit_find + "_replace", QKeySequence::Find);
const sc_pref sc_edit_edit_find_next (sc_edit_edit_find + "_next", QKeySequence::FindNext);
const sc_pref sc_edit_edit_find_previous (sc_edit_edit_find + "_previous", QKeySequence::FindPrevious);
const sc_pref sc_edit_edit_delete_start_word (sc_edit_edit + ":delete_start_word", QKeySequence::DeleteStartOfWord);
const sc_pref sc_edit_edit_delete_end_word (sc_edit_edit + ":delete_end_word", QKeySequence::DeleteEndOfWord);
const sc_pref sc_edit_edit_delete_start_line (sc_edit_edit + ":delete_start_line", CTRL_SHIFT + Qt::Key_Backspace);
const sc_pref sc_edit_edit_delete_end_line (sc_edit_edit + ":delete_end_line", CTRL_SHIFT + Qt::Key_Delete);
const sc_pref sc_edit_edit_delete_line (sc_edit_edit + ":delete_line", CTRL_SHIFT + Qt::Key_L);
const sc_pref sc_edit_edit_copy_line (sc_edit_edit + ":copy_line", CTRL_SHIFT + Qt::Key_C);
const sc_pref sc_edit_edit_cut_line (sc_edit_edit + ":cut_line", CTRL_SHIFT + Qt::Key_X);
const sc_pref sc_edit_edit_duplicate_selection (sc_edit_edit + ":duplicate_selection", CTRL + Qt::Key_D);
const sc_pref sc_edit_edit_transpose_line (sc_edit_edit + ":transpose_line", CTRL + Qt::Key_T);
const sc_pref sc_edit_edit_completion_list (sc_edit_edit + ":completion_list", CTRL + Qt::Key_Space);

const sc_pref sc_edit_edit_comment_selection (sc_edit_edit + ":comment_selection", CTRL + Qt::Key_R);
const sc_pref sc_edit_edit_uncomment_selection (sc_edit_edit + ":uncomment_selection", CTRL_SHIFT + Qt::Key_R);
const sc_pref sc_edit_edit_comment_var_selection (sc_edit_edit + ":comment_var_selection", CTRL_ALT + Qt::Key_R);
const sc_pref sc_edit_edit_upper_case (sc_edit_edit + ":upper_case", CTRL + Qt::Key_U);
const sc_pref sc_edit_edit_lower_case (sc_edit_edit + ":lower_case", CTRL_ALT + Qt::Key_U);

#if defined (Q_OS_MAC)
const sc_pref sc_edit_edit_indent_selection (sc_edit_edit + ":indent_selection", PRE + Qt::Key_Tab);
const sc_pref sc_edit_edit_unindent_selection (sc_edit_edit + ":unindent_selection", PRE + Qt::ShiftModifier + Qt::Key_Tab);
#else
const sc_pref sc_edit_edit_indent_selection (sc_edit_edit + ":indent_selection", CTRL + Qt::Key_Tab);
const sc_pref sc_edit_edit_unindent_selection (sc_edit_edit + ":unindent_selection", CTRL_SHIFT + Qt::Key_Tab);
#endif
const sc_pref sc_edit_edit_smart_indent_line_or_selection (sc_edit_edit + ":smart_indent_line_or_selection", QKeySequence::UnknownKey);

const sc_pref sc_edit_edit_conv_eol_winows (sc_edit_edit + ":conv_eol_winows", QKeySequence::UnknownKey);
const sc_pref sc_edit_edit_conv_eol_unix (sc_edit_edit + ":conv_eol_unix", QKeySequence::UnknownKey);
const sc_pref sc_edit_edit_conv_eol_mac (sc_edit_edit + ":conv_eol_mac", QKeySequence::UnknownKey);

const sc_pref sc_edit_edit_goto_line (sc_edit_edit + ":goto_line", CTRL + Qt::Key_L);
const sc_pref sc_edit_edit_move_to_brace (sc_edit_edit + ":move_to_brace", CTRL + Qt::Key_M);
const sc_pref sc_edit_edit_select_to_brace (sc_edit_edit + ":select_to_brace", CTRL_SHIFT + Qt::Key_M);
const sc_pref sc_edit_edit_toggle_bookmark (sc_edit_edit + ":toggle_bookmark", PRE + Qt::Key_F7);
const sc_pref sc_edit_edit_next_bookmark (sc_edit_edit + ":next_bookmark", PRE + Qt::Key_F2);
const sc_pref sc_edit_edit_previous_bookmark (sc_edit_edit + ":previous_bookmark", PRE + Qt::SHIFT + Qt::Key_F2);
const sc_pref sc_edit_edit_remove_bookmark (sc_edit_edit + ":remove_bookmark", QKeySequence::UnknownKey);

const sc_pref sc_edit_edit_preferences (sc_edit_edit + ":preferences", QKeySequence::UnknownKey);
const sc_pref sc_edit_edit_styles_preferences (sc_edit_edit + ":styles_preferences", QKeySequence::UnknownKey);

// view
const sc_pref sc_edit_view_show_line_numbers (sc_edit_view + ":show_line_numbers", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_white_spaces (sc_edit_view + ":show_white_spaces", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_eol_chars (sc_edit_view + ":show_eol_chars", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_ind_guides (sc_edit_view + ":show_ind_guides", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_long_line (sc_edit_view + ":show_long_line", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_toolbar (sc_edit_view + ":show_toolbar", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_statusbar (sc_edit_view + ":show_statusbar", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_show_hscrollbar (sc_edit_view + ":show_hscrollbar", QKeySequence::UnknownKey);
const sc_pref sc_edit_view_sort_tabs (sc_edit_view + ":sort_tabs", QKeySequence::UnknownKey);

// debug
const QString sc_edit_debug ("editor_debug");
const sc_pref sc_edit_debug_toggle_breakpoint (sc_edit_debug + ":toggle_breakpoint", QKeySequence::UnknownKey);
const sc_pref sc_edit_debug_next_breakpoint (sc_edit_debug + ":next_breakpoint", QKeySequence::UnknownKey);
const sc_pref sc_edit_debug_previous_breakpoint (sc_edit_debug + ":previous_breakpoint", QKeySequence::UnknownKey);
const sc_pref sc_edit_debug_remove_breakpoints (sc_edit_debug + ":remove_breakpoints", QKeySequence::UnknownKey);

// run
const QString sc_edit_run ("editor_run");
const sc_pref sc_edit_run_run_file (sc_edit_run + ":run_file", PRE + Qt::Key_F5);
const sc_pref sc_edit_run_run_selection (sc_edit_run + ":run_selection", PRE + Qt::Key_F9);

// help
const QString sc_edit_help ("editor_help");
const sc_pref sc_edit_help_help_keyword (sc_edit_help + ":help_keyword", QKeySequence::HelpContents);
const sc_pref sc_edit_help_doc_keyword (sc_edit_help + ":doc_keyword", Qt::SHIFT + Qt::Key_F1);


// Documentation browser
const QString sc_doc ("doc_browser");
const sc_pref sc_doc_go_home (sc_doc + ":go_home", Qt::AltModifier + Qt::Key_Home);
const sc_pref sc_doc_go_back (sc_doc + ":go_back", QKeySequence::Back);
const sc_pref sc_doc_go_next (sc_doc + ":go_next", QKeySequence::Forward);
const sc_pref sc_doc_bookmark (sc_doc + ":bookmark", CTRL + Qt::Key_D);


// Other normal, shortcut related options

const gui_pref
sc_main_ctrld ("shortcuts/main_ctrld", QVariant (false));

const gui_pref
sc_prevent_rl_conflicts ("shortcuts/prevent_readline_conflicts", QVariant (false));
const gui_pref
sc_prevent_rl_conflicts_menu ("shortcuts/prevent_readline_conflicts_menu", QVariant (false));

#endif
