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

#if ! defined (octave_gui_preferences_dw_h)
#define octave_gui_preferences_dw_h 1

#include "gui-preferences.h"

extern gui_pref dw_focus_follows_mouse;

extern gui_pref dw_title_custom_style;

extern gui_pref dw_title_3d;

extern gui_pref dw_title_fg_color;

extern gui_pref dw_title_fg_color_active;

extern gui_pref dw_title_bg_color;

extern gui_pref dw_title_bg_color_active;

extern gui_pref dw_icon_set;

const QHash <QString, QString> dw_icon_set_names
  = {
      // array of possible icon sets (name, path (complete for NONE))
      // the first entry here is the default!
      {"NONE",    ":/icons/octave/128x128/logo.png"},
      {"GRAPHIC", ":/icons/octave/128x128/graphic_logo_"},
      {"LETTER",  ":/icons/octave/128x128/letter_logo_"}
    };

// The following keys have to be used with .arg (objectName ())
extern gui_pref dw_float_geometry;

extern gui_pref dw_dock_geometry;

extern gui_pref dw_is_visible;

extern gui_pref dw_is_floating;

extern gui_pref dw_is_minimized;

#endif
