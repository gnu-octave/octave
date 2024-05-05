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


#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#  include <QKeyCombination>
#  define OCTAVE_QT_KEYCOMBINATION(mod, key) \
     QKeyCombination (mod, key).toCombined ()
#else
#  define OCTAVE_QT_KEYCOMBINATION(mod, key) \
     mod | key
#endif

#if defined (Q_OS_MAC)
// Use CMD key as an equivalent of Ctrl key on other platforms
const Qt::KeyboardModifier CTRL = Qt::MetaModifier;
// Some of octave default shortcuts on windows/linux are already defined
// as system wide shortcuts on Mac Os X (almost all Function keys).
// Prefix those with Option (Alt) modifier to avoid conflicts.
const Qt::KeyboardModifier PRE = Qt::AltModifier;
#else
const Qt::KeyboardModifier CTRL = Qt::ControlModifier;
const Qt::KeyboardModifier PRE = Qt::NoModifier;
#endif

const Qt::KeyboardModifiers CTRL_SHIFT = CTRL | Qt::ShiftModifier;
const Qt::KeyboardModifiers CTRL_ALT = CTRL | Qt::AltModifier;

const QString sc_group ("shortcuts");

// Shortcuts not related to specific Menus

// Dock widgets
const QString sc_dock_widget ("dock_widget");
extern sc_pref sc_dock_widget_dock;
extern sc_pref sc_dock_widget_close;

// Main window menu

// file
const QString sc_main_file ("main_file");
extern sc_pref sc_main_file_new_file;
extern sc_pref sc_main_file_new_function;
extern sc_pref sc_main_file_new_figure;
extern sc_pref sc_main_file_open_file;
extern sc_pref sc_main_file_load_workspace;
extern sc_pref sc_main_file_save_workspace;
extern sc_pref sc_main_file_exit;

// edit
const QString sc_main_edit ("main_edit");
extern sc_pref sc_main_edit_copy;
extern sc_pref sc_main_edit_paste;
extern sc_pref sc_main_edit_undo;
extern sc_pref sc_main_edit_select_all;
extern sc_pref sc_main_edit_clear_clipboard;
extern sc_pref sc_main_edit_find_in_files;
extern sc_pref sc_main_edit_clear_command_window;
extern sc_pref sc_main_edit_clear_history;
extern sc_pref sc_main_edit_clear_workspace;
extern sc_pref sc_main_edit_set_path;
extern sc_pref sc_main_edit_preferences;

// debug
const QString sc_main_debug ("main_debug");
extern sc_pref sc_main_debug_step_over;
extern sc_pref sc_main_debug_step_into;
extern sc_pref sc_main_debug_step_out;
extern sc_pref sc_main_debug_continue;
extern sc_pref sc_main_debug_quit;

// tools
const QString sc_main_tools ("main_tools");
extern sc_pref sc_main_tools_start_profiler;
extern sc_pref sc_main_tools_resume_profiler;
extern sc_pref sc_main_tools_show_profiler;


// window
const QString sc_main_window ("main_window");
extern sc_pref sc_main_window_show_command;
extern sc_pref sc_main_window_show_history;
extern sc_pref sc_main_window_show_file_browser;
extern sc_pref sc_main_window_show_workspace;
extern sc_pref sc_main_window_show_editor;
extern sc_pref sc_main_window_show_doc;
extern sc_pref sc_main_window_show_variable_editor;
extern sc_pref sc_main_window_command;
extern sc_pref sc_main_window_history;
extern sc_pref sc_main_window_file_browser;
extern sc_pref sc_main_window_workspace;
extern sc_pref sc_main_window_editor;
extern sc_pref sc_main_window_doc;
extern sc_pref sc_main_window_variable_editor;
extern sc_pref sc_main_window_previous_dock;
extern sc_pref sc_main_window_reset;

// help
const QString sc_main_help ("main_help");
extern sc_pref sc_main_help_ondisk_doc;
extern sc_pref sc_main_help_online_doc;
extern sc_pref sc_main_help_report_bug;
extern sc_pref sc_main_help_packages;
extern sc_pref sc_main_help_contribute;
extern sc_pref sc_main_help_developer;
extern sc_pref sc_main_help_about;

// news
const QString sc_main_news ("main_news");
extern sc_pref sc_main_news_release_notes;
extern sc_pref sc_main_news_community_news;

// Tab handling
// The following shortcuts are moved into a separate tab.  The key names
// are not change for preserving compatibility with older versions
const QString sc_edit_file ("editor_file");
const QString sc_edit_file_cl (sc_edit_file + ":close");
extern sc_pref sc_edit_file_close;
extern sc_pref sc_edit_file_close_all;
extern sc_pref sc_edit_file_close_other;
const QString sc_edit_tabs ("editor_tabs");
extern sc_pref sc_edit_tabs_switch_left_tab;
extern sc_pref sc_edit_tabs_switch_right_tab;
extern sc_pref sc_edit_tabs_move_tab_left;
extern sc_pref sc_edit_tabs_move_tab_right;

// Zooming
const QString sc_edit_zoom ("editor_zoom"); // only a group name in the pref dialog
const QString sc_edit_view ("editor_view");
const QString sc_edit_view_zoom (sc_edit_view + ":zoom");
extern sc_pref sc_edit_view_zoom_in;
extern sc_pref sc_edit_view_zoom_out;
#if defined (Q_OS_MAC)
extern sc_pref sc_edit_view_zoom_normal;
#else
extern sc_pref sc_edit_view_zoom_normal;
#endif

// Actions of the editor

// file
extern sc_pref sc_edit_file_edit_function;
extern sc_pref sc_edit_file_save;
extern sc_pref sc_edit_file_save_as;
extern sc_pref sc_edit_file_print;

// edit
const QString sc_edit_find ("editor_find"); // only a group name in the pref dialog
const QString sc_edit_edit ("editor_edit");
const QString sc_edit_edit_find (sc_edit_edit + ":find");
extern sc_pref sc_edit_edit_redo;
extern sc_pref sc_edit_edit_cut;
extern sc_pref sc_edit_edit_find_replace;
extern sc_pref sc_edit_edit_find_next;
extern sc_pref sc_edit_edit_find_previous;
extern sc_pref sc_edit_edit_delete_start_word;
extern sc_pref sc_edit_edit_delete_end_word;
extern sc_pref sc_edit_edit_delete_start_line;
extern sc_pref sc_edit_edit_delete_end_line;
extern sc_pref sc_edit_edit_delete_line;
extern sc_pref sc_edit_edit_copy_line;
extern sc_pref sc_edit_edit_cut_line;
extern sc_pref sc_edit_edit_duplicate_selection;
extern sc_pref sc_edit_edit_transpose_line;
extern sc_pref sc_edit_edit_completion_list;

extern sc_pref sc_edit_edit_comment_selection;
extern sc_pref sc_edit_edit_uncomment_selection;
extern sc_pref sc_edit_edit_comment_var_selection;
extern sc_pref sc_edit_edit_upper_case;
extern sc_pref sc_edit_edit_lower_case;

#if defined (Q_OS_MAC)
extern sc_pref sc_edit_edit_indent_selection;
extern sc_pref sc_edit_edit_unindent_selection;
#else
extern sc_pref sc_edit_edit_indent_selection;
extern sc_pref sc_edit_edit_unindent_selection;
#endif
extern sc_pref sc_edit_edit_smart_indent_line_or_selection;

extern sc_pref sc_edit_edit_conv_eol_winows;
extern sc_pref sc_edit_edit_conv_eol_unix;
extern sc_pref sc_edit_edit_conv_eol_mac;

extern sc_pref sc_edit_edit_goto_line;
extern sc_pref sc_edit_edit_move_to_brace;
extern sc_pref sc_edit_edit_select_to_brace;
extern sc_pref sc_edit_edit_toggle_bookmark;
extern sc_pref sc_edit_edit_next_bookmark;
extern sc_pref sc_edit_edit_previous_bookmark;
extern sc_pref sc_edit_edit_remove_bookmark;

extern sc_pref sc_edit_edit_preferences;
extern sc_pref sc_edit_edit_styles_preferences;

// view
extern sc_pref sc_edit_view_show_line_numbers;
extern sc_pref sc_edit_view_show_white_spaces;
extern sc_pref sc_edit_view_show_eol_chars;
extern sc_pref sc_edit_view_show_ind_guides;
extern sc_pref sc_edit_view_show_long_line;
extern sc_pref sc_edit_view_show_toolbar;
extern sc_pref sc_edit_view_show_statusbar;
extern sc_pref sc_edit_view_show_hscrollbar;
extern sc_pref sc_edit_view_sort_tabs;

// debug
const QString sc_edit_debug ("editor_debug");
extern sc_pref sc_edit_debug_toggle_breakpoint;
extern sc_pref sc_edit_debug_next_breakpoint;
extern sc_pref sc_edit_debug_previous_breakpoint;
extern sc_pref sc_edit_debug_remove_breakpoints;

// run
const QString sc_edit_run ("editor_run");
extern sc_pref sc_edit_run_run_file;
extern sc_pref sc_edit_run_run_selection;
extern sc_pref sc_edit_run_run_tests;
extern sc_pref sc_edit_run_run_demos;

// help
const QString sc_edit_help ("editor_help");
extern sc_pref sc_edit_help_help_keyword;
extern sc_pref sc_edit_help_doc_keyword;


// Documentation browser
const QString sc_doc ("doc_browser");
extern sc_pref sc_doc_go_home;
extern sc_pref sc_doc_go_back;
extern sc_pref sc_doc_go_next;
extern sc_pref sc_doc_bookmark;


// Other normal, shortcut related options

const gui_pref
sc_main_ctrld ("shortcuts/main_ctrld", QVariant (false));

const gui_pref
sc_prevent_rl_conflicts ("shortcuts/prevent_readline_conflicts", QVariant (false));
const gui_pref
sc_prevent_rl_conflicts_menu ("shortcuts/prevent_readline_conflicts_menu", QVariant (false));

extern QString get_shortcut_section (const QString& key);

#endif
