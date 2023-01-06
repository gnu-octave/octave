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

#if ! defined (octave_gui_preferences_dw_h)
#define octave_gui_preferences_dw_h 1

#include "gui-preferences.h"

const gui_pref
dw_focus_follows_mouse ("DockWidgets/focus_follows_mouse", QVariant (false));

const gui_pref
dw_title_custom_style ("DockWidgets/widget_title_custom_style",
                       QVariant (true));

const gui_pref dw_title_3d ("DockWidgets/widget_title_3d", QVariant (20));

const gui_pref dw_title_fg_color ("DockWidgets/title_fg_color",
                                  QVariant (QColor (0, 0, 0)));

const gui_pref dw_title_fg_color_active ("DockWidgets/title_fg_color_active",
                                         QVariant (QColor (255, 255, 255)));

const gui_pref dw_title_bg_color ("DockWidgets/title_bg_color",
                                  QVariant (QColor (192, 192, 192)));

const gui_pref dw_title_bg_color_active ("DockWidgets/title_bg_color_active",
                                         QVariant (QColor (128, 128, 128)));

const gui_pref dw_icon_set ("DockWidgets/widget_icon_set", QVariant ("NONE"));

const QHash <QString, QString> dw_icon_set_names
  = {
      // array of possible icon sets (name, path (complete for NONE))
      // the first entry here is the default!
      {"NONE",    ":/icons/octave/128x128/logo.png"},
      {"GRAPHIC", ":/icons/octave/128x128/graphic_logo_"},
      {"LETTER",  ":/icons/octave/128x128/letter_logo_"}
    };

// The following keys have to be used with .arg (objectName ())
const gui_pref dw_float_geometry ("DockWidgets/%1_floating_geometry",
                                  QVariant (QRect (50, 50, 480, 640)));

const gui_pref dw_dock_geometry ("DockWidgets/%1",
                                 QVariant (QRect (10, 10, 240, 320)));

const gui_pref dw_is_visible ("DockWidgets/%1Visible", QVariant (true));

const gui_pref dw_is_floating ("DockWidgets/%1Floating", QVariant (false));

const gui_pref dw_is_minimized ("DockWidgets/%1_minimized", QVariant (false));

#endif
