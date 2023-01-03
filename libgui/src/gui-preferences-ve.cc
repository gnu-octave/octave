////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2022 The Octave Project Developers
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

#include "gui-preferences-ve.h"

#include <QApplication>

// Variable Editor preferences

gui_pref
ve_use_terminal_font ("variable_editor/use_terminal_font", QVariant (true));

gui_pref
ve_font_name ("variable_editor/font_name", QVariant ());

gui_pref
ve_font_size ("variable_editor/font_size", QVariant (10));

gui_pref
ve_column_width ("variable_editor/column_width", QVariant (100));

gui_pref
ve_row_height ("variable_editor/row_height", QVariant (10));

gui_pref
ve_alternate_rows ("variable_editor/alternate_rows", QVariant (false));

gui_pref
ve_color_mode ("variable_editor/color_mode", QVariant (0));

gui_pref ve_colors[2*ve_colors_count] =
{
  {"variable_editor/color_f" + settings_color_modes_ext[0], QVariant (QPalette::WindowText)},
  {"variable_editor/color_b" + settings_color_modes_ext[0], QVariant (QPalette::Base)},
  {"variable_editor/color_s" + settings_color_modes_ext[0], QVariant (QPalette::HighlightedText)},
  {"variable_editor/color_h" + settings_color_modes_ext[0], QVariant (QPalette::Highlight)},
  {"variable_editor/color_a" + settings_color_modes_ext[0], QVariant (QPalette::AlternateBase)},
  {"variable_editor/color_f" + settings_color_modes_ext[1], QVariant ()},
  {"variable_editor/color_b" + settings_color_modes_ext[1], QVariant ()},
  {"variable_editor/color_s" + settings_color_modes_ext[1], QVariant ()},
  {"variable_editor/color_h" + settings_color_modes_ext[1], QVariant ()},
  {"variable_editor/color_a" + settings_color_modes_ext[1], QVariant ()}
};
