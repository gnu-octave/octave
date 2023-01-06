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
#include "gui-settings.h"

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

const gui_pref
cs_color_mode ("terminal/color_mode", QVariant (0));

const unsigned int cs_colors_count = 4;
const gui_pref cs_colors[2*cs_colors_count] =
{
  {"terminal/color_f" + settings_color_modes_ext[0], QVariant (QPalette::WindowText)},
  {"terminal/color_b" + settings_color_modes_ext[0], QVariant (QPalette::Base)},
  {"terminal/color_s" + settings_color_modes_ext[0], QVariant (QPalette::Highlight)},
  {"terminal/color_c" + settings_color_modes_ext[0], QVariant (QPalette::QPalette::WindowText)},
  {"terminal/color_f" + settings_color_modes_ext[1], QVariant ()}, // Default colors for 2nd mode empty,
  {"terminal/color_b" + settings_color_modes_ext[1], QVariant ()}, // since they are determined at runtime
  {"terminal/color_s" + settings_color_modes_ext[1], QVariant ()}, // by inverting the lightness of the
  {"terminal/color_c" + settings_color_modes_ext[1], QVariant ()}  // default colors in light mode
};
const QStringList
cs_color_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "foreground"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "background"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "selection"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "cursor")
};

const gui_pref
cs_focus_cmd ("terminal/focus_after_command", QVariant (false));

const gui_pref
cs_dbg_location ("terminal/print_debug_location", QVariant (false));

#endif
