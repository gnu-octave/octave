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

#if ! defined (octave_gui_preferences_ed_h)
#define octave_gui_preferences_ed_h 1

#if defined (HAVE_QSCINTILLA)
#include <Qsci/qsciscintilla.h>
#endif

#include "gui-settings.h"
#include "localcharset-wrapper.h"

// Editor preferences

// Lexer

const gui_pref
ed_color_mode ("editor/color_mode", QVariant (0));

const int ed_max_lexer_styles = 64;
const int ed_max_style_number = 128;

// Code completion

const gui_pref
ed_code_completion_octave_builtins ("editor/codeCompletion_octave_builtins",
                                    QVariant (true));

const gui_pref
ed_code_completion_octave_functions ("editor/codeCompletion_octave_functions",
                                     QVariant (true));

const gui_pref
ed_code_completion_keywords ("editor/codeCompletion_keywords", QVariant (true));

const gui_pref
ed_code_completion_document ("editor/codeCompletion_document", QVariant (true));

const gui_pref
ed_code_completion_replace ("editor/codeCompletion_replace", QVariant (false));

const gui_pref
ed_code_completion_case ("editor/codeCompletion_case", QVariant (true));

const gui_pref
ed_code_completion ("editor/codeCompletion", QVariant (true));

const gui_pref
ed_code_completion_threshold ("editor/codeCompletion_threshold", QVariant (3));

// Code formatting

const gui_pref
ed_code_folding ("editor/code_folding", QVariant (true));

const gui_pref
ed_auto_indent ("editor/auto_indent", QVariant (true));

const gui_pref
ed_tab_indents_line ("editor/tab_indents_line", QVariant (false));

const gui_pref
ed_backspace_unindents_line ("editor/backspace_unindents_line",
                             QVariant (false));

const gui_pref
ed_show_indent_guides ("editor/show_indent_guides", QVariant (false));

const gui_pref
ed_indent_uses_tabs ("editor/indent_uses_tabs", QVariant (false));

const gui_pref
ed_indent_width ("editor/indent_width", QVariant (2));

const gui_pref
ed_tab_width ("editor/tab_width", QVariant (2));

const gui_pref
ed_auto_endif ("editor/auto_endif", QVariant (1));

// Long line handling

const gui_pref
ed_long_line_column ("editor/long_line_column", QVariant (80));

const gui_pref
ed_long_line_marker ("editor/long_line_marker", QVariant (true));

const gui_pref
ed_long_line_marker_line ("editor/long_line_marker_line", QVariant (true));

const gui_pref
ed_long_line_marker_background ("editor/long_line_marker_background",
                                QVariant (false));

const gui_pref
ed_wrap_lines ("editor/wrap_lines", QVariant (false));

const gui_pref
ed_break_lines ("editor/break_lines", QVariant (false));

const gui_pref
ed_break_lines_comments ("editor/break_lines_comments", QVariant (false));

// Other

const gui_pref
ed_highlight_all_occurrences ("editor/highlight_all_occurrences",
                              QVariant (true));

const gui_pref
ed_show_Line_numbers ("editor/showLineNumbers", QVariant (true));

const gui_pref
ed_line_numbers_size ("editor/line_numbers_size", QVariant ( 0));

const gui_pref
ed_show_edit_status_bar ("editor/show_edit_status_bar", QVariant (true));

const gui_pref
ed_highlight_current_line ("editor/highlightCurrentLine", QVariant (true));

const gui_pref
ed_highlight_current_line_color ("editor/highlightCurrentLineColor",
                                 QVariant (settings_color_no_change));

const gui_pref
ed_show_white_space ("editor/show_white_space", QVariant (false));

const gui_pref
ed_show_white_space_indent ("editor/show_white_space_indent", QVariant (false));

const gui_pref
ed_show_line_numbers ("editor/showLineNumbers", QVariant (true));

const gui_pref
ed_show_eol_chars ("editor/show_eol_chars", QVariant (false));

const gui_pref
ed_show_toolbar ("editor/show_toolbar", QVariant (true));

const gui_pref
ed_show_hscroll_bar ("editor/show_hscroll_bar", QVariant (true));

// Octave comment strings

const gui_pref
ed_comment_str_old ("editor/octave_comment_string", QVariant (0));

const gui_pref
ed_comment_str ("editor/oct_comment_str", QVariant (0));

const gui_pref
ed_uncomment_str ("editor/oct_uncomment_str", QVariant (1 + 2 + 4 + 8));

const QString
ed_last_comment_str ("editor/oct_last_comment_str");

const QStringList
ed_comment_strings = {
  "##",
  "#",
  "%",
  "%%",
  "%!"
};

const int ed_comment_strings_count = 5;

// Session data

const gui_pref
ed_restore_session ("editor/restoreSession", QVariant (true));

const gui_pref
ed_session_names ("editor/savedSessionTabs", QVariant (QStringList ()));

const gui_pref
ed_session_enc ("editor/saved_session_encodings", QVariant (QStringList ()));

const gui_pref
ed_session_ind ("editor/saved_session_tab_index", QVariant (QStringList ()));

const gui_pref
ed_session_lines ("editor/saved_session_lines", QVariant (QStringList ()));

const gui_pref
ed_session_bookmarks ("editor/saved_session_bookmarks", QVariant (QStringList ()));

// Tabs
const QStringList
ed_tab_position_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Top"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Bottom"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Left"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Right")
};

const gui_pref
ed_tab_position ("editor/tab_position", QVariant (QTabWidget::North));

const gui_pref
ed_tabs_rotated ("editor/tabs_rotated", QVariant (false));

const gui_pref
ed_tabs_max_width ("editor/tabs_max_width", QVariant (0));

// File handling

const gui_pref
ed_force_newline ("editor/force_newline", QVariant (true));

const gui_pref
ed_rm_trailing_spaces ("editor/rm_trailing_spaces", QVariant (true));

#if defined (HAVE_QSCINTILLA)
#if defined (Q_OS_WIN32)
const int os_eol_mode = QsciScintilla::EolWindows;
#else
const int os_eol_mode = QsciScintilla::EolUnix;
#endif
#else
const int os_eol_mode = 2;
#endif

const gui_pref
ed_default_eol_mode ("editor/default_eol_mode", QVariant (os_eol_mode));

const gui_pref
ed_show_dbg_file ("editor/show_dbg_file", QVariant (true));

const gui_pref
ed_default_enc ("editor/default_encoding", QVariant ("UTF-8"));

const gui_pref
ed_create_new_file ("editor/create_new_file", QVariant (false));

const gui_pref
ed_hiding_closes_files ("editor/hiding_closes_files", QVariant (false));

const gui_pref
ed_always_reload_changed_files ("editor/always_reload_changed_files",
                                QVariant (false));

const gui_pref
ed_mru_file_list ("editor/mru_file_list", QVariant ());

const gui_pref
ed_mru_file_encodings ("editor/mru_file_encodings", QVariant ());

// The find dialog

enum find_dialog_options
{
  FIND_DLG_MORE  = 1,
  FIND_DLG_CASE  = 2,
  FIND_DLG_START = 4,
  FIND_DLG_WRAP  = 8,
  FIND_DLG_REGX  = 16,
  FIND_DLG_WORDS = 32,
  FIND_DLG_BACK  = 64,
  FIND_DLG_SEL   = 128
};

// Dialog position, the default will be calculated from the editor's geometry
const gui_pref
ed_fdlg_pos ("editor/fdgl_pos", QVariant (QPoint (0, 0)));

const gui_pref
ed_fdlg_opts ("editor/fdgl_opts", QVariant (FIND_DLG_WRAP));

const gui_pref
ed_fdlg_search ("editor/fdgl_search", QVariant ());
const gui_pref
ed_fdlg_replace ("editor/fdgl_replace", QVariant ());

#endif
