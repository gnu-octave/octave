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

#if ! defined (octave_gui_preferences_ve_h)
#define octave_gui_preferences_ve_h 1

#include "gui-preferences.h"

#include <QApplication>

// Variable Editor preferences

const gui_pref
ve_use_terminal_font ("variable_editor/use_terminal_font", QVariant (true));

const gui_pref
ve_font_name ("variable_editor/font_name", QVariant ());

const gui_pref
ve_font_size ("variable_editor/font_size", QVariant (10));

const gui_pref
ve_column_width ("variable_editor/column_width", QVariant (100));

const gui_pref
ve_row_height ("variable_editor/row_height", QVariant (10));

const gui_pref
ve_alternate_rows ("variable_editor/alternate_rows", QVariant (false));

const gui_pref
ve_color_mode ("variable_editor/color_mode", QVariant (0));

const QString ve_color_chars ("fbsha");

const int ve_colors_count = 5;
const gui_pref ve_colors[2*ve_colors_count] =
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

const QStringList ve_color_names = {
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Foreground"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Background"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Selected Foreground"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Selected Background"),
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Alternate Background")
};

const QStringList ve_save_formats = {
  "ascii",
  "binary",
  "float-binary",
  "hdf5",
  "float-hdf5",
  "text",
  "mat7-binary",
  "mat-binary",
  "mat4-binary",
  "zip"
};

// The following list is a relation between save format and fiel extension.
// The format string are case insensitive.
const QStringList ve_save_formats_ext = {
  "-ascii",       "dat",
  "-hdf5",        "h5",
  "-text",        "txt",
  "-v7.3",        "mat",
  "-7.3",         "mat",
  "-v7",          "mat",
  "-7",           "mat",
  "-mat7-binary", "mat",
  "-v6",          "mat",
  "-6",           "mat",
  "-mat-binary",  "mat",
  "-v4",          "mat",
  "-4",           "mat",
  "-mat4-binary", "mat",
  "-binary",      "bin",   // after other fmt incl. "-binary"
  "-z",           "txt.gz" // gzipped -text
};

#endif
