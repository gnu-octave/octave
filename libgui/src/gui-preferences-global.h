/*

Copyright (C) 2017-2019 Torsten <ttl-octave@mailbox.org>

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_gui_preferences_global_h)
#define octave_gui_preferences_global_h 1

#include "gui-preferences.h"

// Global preferences

// Get the default monospaced font
#if defined (Q_WS_X11)
const QString global_font_family = "Monospace";
#elif defined (Q_WS_WIN) || defined (Q_WS_MAC)
const QString global_font_family = "Courier";
#else
const QString global_font_family = "Courier";
#endif

const gui_pref
global_mono_font ("monospace_font", global_font_family);

// Compacter Style for some widgets

const QString
global_toolbar_style ("QToolBar {"
                      "spacing-top: 0px;"
                      "spacing-bottom: 0px;"
                      "margin-top: 0px;"
                      "margin-bottom: 0px;"
                      "padding-top: 0px;"
                      "padding-bottom: 0px;"
                      "border-top: 0px;"
                      "border-bottom: 0px;"
                      "}");

const QString
global_menubar_style ("QMenuBar {"
                      "spacing-top: 0px;"
                      "spacing-bottom: 0px;"
                      "margin-top: 0px;"
                      "margin-bottom: 0px;"
                      "padding-top: 0px;"
                      "padding-bottom: 0px;"
                      "}");

// Icon size (in preferences: values -1, 0, 1)

const QStyle::PixelMetric global_icon_sizes[3] =
{
  QStyle::PM_SmallIconSize,
  QStyle::PM_ToolBarIconSize,
  QStyle::PM_LargeIconSize
};

const gui_pref
global_icon_size ("toolbar_icon_size", QVariant (0));

const gui_pref
global_icon_theme ("use_system_icon_theme", QVariant (true));

// Style

const gui_pref
global_style ("style", QVariant ("default"));

// Other

const gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (true));

const gui_pref
global_cursor_blinking ("cursor_blinking", QVariant (true));

#endif
