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

#if ! defined (octave_gui_preferences_sd_h)
#define octave_gui_preferences_sd_h 1

#include "gui-preferences.h"

// Settings dialog

extern gui_pref sd_geometry;

extern gui_pref sd_last_tab;

extern gui_pref sd_last_editor_styles_tab;

// Textstrings for second color schemes
const QString settings_color_modes
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "Second color mode (light/dark)");

const QString settings_color_modes_tooltip
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "Switch to a second set of colors.\n"
                       "Useful for defining light/dark modes.\n"
                       "Discards non-applied current changes!");

const QString settings_reload_colors
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "&Reload default colors");

const QString settings_reload_colors_tooltip
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "Reload the default colors,\n"
                       "depends on currently selected mode.");

const QString settings_reload_styles
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "&Reload default styles");

const QString settings_reload_styles_tooltip
  = QT_TRANSLATE_NOOP ("octave::settings_dialog",
                       "Reload the default style values,\n"
                       "depends on currently selected mode.");

#endif
