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

#if ! defined (octave_gui_preferences_cs_h)
#define octave_gui_preferences_cs_h 1

#include "gui-preferences.h"

// Console preferences

extern gui_pref cs_font;

extern gui_pref cs_font_size;

const std::vector<std::string> cs_cursor_types =
{
  "ibeam",
  "block",
  "underline"
};

extern gui_pref cs_cursor;

extern gui_pref cs_cursor_blinking;

extern gui_pref cs_cursor_use_fgcol;

extern gui_pref cs_hist_buffer;

extern gui_pref cs_color_mode;

const unsigned int cs_colors_count = 4;

extern gui_pref cs_colors[];

const QStringList
cs_color_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Foreground"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Background"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Selection"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Cursor")
};

extern gui_pref cs_focus_cmd;

extern gui_pref cs_dbg_location;

#endif
