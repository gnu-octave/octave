////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2020 The Octave Project Developers
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

const gui_pref
cs_font ("terminal/fontName", QVariant ());

const gui_pref
cs_font_size ("terminal/fontSize", QVariant (10));

const std::vector<std::string> cs_cursor_types =
{
  "ibeam",
  "block",
  "underline"
};

const gui_pref
cs_cursor ("terminal/cursorType", QVariant ("ibeam"));

const gui_pref
cs_cursor_blinking ("terminal/cursorBlinking", QVariant (true));

const gui_pref
cs_cursor_use_fgcol ("terminal/cursorUseForegroundColor", QVariant (true));

const gui_pref
cs_hist_buffer ("terminal/history_buffer", QVariant (1000));

const unsigned int cs_colors_count = 4;
const gui_pref cs_colors[cs_colors_count] =
{
  {"terminal/color_f", QVariant (QColor(0,0,0))},
  {"terminal/color_b", QVariant (QColor(255,255,255))},
  {"terminal/color_s", QVariant (QColor(192,192,192))},
  {"terminal/color_c", QVariant (QColor(128,128,128))}
};
const QStringList
cs_color_names (QStringList ()
                << QT_TRANSLATE_NOOP ("octave::settings_dialog", "foreground")
                << QT_TRANSLATE_NOOP ("octave::settings_dialog", "background")
                << QT_TRANSLATE_NOOP ("octave::settings_dialog", "selection")
                << QT_TRANSLATE_NOOP ("octave::settings_dialog", "cursor"));

const gui_pref
cs_focus_cmd ("terminal/focus_after_command", QVariant (false));

const gui_pref
cs_dbg_location ("terminal/print_debug_location", QVariant (false));

#endif
