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

#if ! defined (octave_gui_preferences_ed_h)
#define octave_gui_preferences_ed_h 1

#if defined (HAVE_QSCINTILLA)
#include <Qsci/qsciscintilla.h>
#endif

#include "gui-settings.h"

// Editor preferences

// Lexer

extern gui_pref ed_color_mode;

const int ed_max_lexer_styles = 64;
const int ed_max_style_number = 128;

// Code completion

extern gui_pref ed_code_completion_octave_builtins;

extern gui_pref ed_code_completion_octave_functions;

extern gui_pref ed_code_completion_keywords;

extern gui_pref ed_code_completion_document;

extern gui_pref ed_code_completion_replace;

extern gui_pref ed_code_completion_case;

extern gui_pref ed_code_completion;

extern gui_pref ed_code_completion_threshold;

// Code formatting

extern gui_pref ed_code_folding;

extern gui_pref ed_auto_indent;

extern gui_pref ed_tab_indents_line;

extern gui_pref ed_backspace_unindents_line;

extern gui_pref ed_show_indent_guides;

extern gui_pref ed_indent_uses_tabs;

extern gui_pref ed_indent_width;

extern gui_pref ed_tab_width;

extern gui_pref ed_auto_endif;

// Long line handling

extern gui_pref ed_long_line_column;

extern gui_pref ed_long_line_marker;

extern gui_pref ed_long_line_marker_line;

extern gui_pref ed_long_line_marker_background;

extern gui_pref ed_wrap_lines;

extern gui_pref ed_break_lines;

extern gui_pref ed_break_lines_comments;

// Other

extern gui_pref ed_highlight_all_occurrences;

extern gui_pref ed_show_Line_numbers;

extern gui_pref ed_line_numbers_size;

extern gui_pref ed_show_edit_status_bar;

extern gui_pref ed_highlight_current_line;

extern gui_pref ed_highlight_current_line_color;

extern gui_pref ed_show_white_space;

extern gui_pref ed_show_white_space_indent;

extern gui_pref ed_show_line_numbers;

extern gui_pref ed_show_eol_chars;

extern gui_pref ed_show_toolbar;

extern gui_pref ed_show_hscroll_bar;

// Octave comment strings

extern gui_pref ed_comment_str_old;

extern gui_pref ed_comment_str;

extern gui_pref ed_uncomment_str;

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

extern gui_pref ed_restore_session;

extern gui_pref ed_session_names;

extern gui_pref ed_session_enc;

extern gui_pref ed_session_ind;

extern gui_pref ed_session_lines;

extern gui_pref ed_session_bookmarks;

// Tabs
const QStringList
ed_tab_position_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Top"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Bottom"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Left"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Right")
};

extern gui_pref ed_tab_position;

extern gui_pref ed_tabs_rotated;

extern gui_pref ed_tabs_max_width;

// File handling

extern gui_pref ed_force_newline;

extern gui_pref ed_rm_trailing_spaces;

#if defined (HAVE_QSCINTILLA)
#if defined (Q_OS_WIN32)
const int os_eol_mode = QsciScintilla::EolWindows;
#else
const int os_eol_mode = QsciScintilla::EolUnix;
#endif
#else
const int os_eol_mode = 2;
#endif

extern gui_pref ed_default_eol_mode;

extern gui_pref ed_show_dbg_file;

extern gui_pref ed_default_enc;

extern gui_pref ed_create_new_file;

extern gui_pref ed_hiding_closes_files;

extern gui_pref ed_always_reload_changed_files;

extern gui_pref ed_mru_file_list;

extern gui_pref ed_mru_file_encodings;

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
extern gui_pref ed_fdlg_pos;

extern gui_pref ed_fdlg_opts;

extern gui_pref ed_fdlg_search;

extern gui_pref ed_fdlg_replace;

#endif
