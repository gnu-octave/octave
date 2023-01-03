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

#include "gui-preferences-global.h"

// Global preferences

gui_pref
global_skip_welcome_wizard ("global_skip_welcome_wizard", false);

gui_pref
global_mono_font ("monospace_font", global_font_family);

// Style

gui_pref
global_style ("style", QVariant ("default"));

// Icon size (in preferences: values -1, 0, 1)

gui_pref
global_icon_size ("toolbar_icon_size", QVariant (0));

gui_pref
global_icon_theme ("use_system_icon_theme", QVariant (true));

gui_pref
global_icon_theme_index ("icon_theme", QVariant (ICON_THEME_SYSTEM));

gui_pref
global_icon_fallbacks ("icon_fallbacks", QVariant (QStringList ()));

gui_pref
global_status_bar ("show_status_bar", QVariant (true));

#if defined (Q_OS_MAC)
// prevent native file dialogs on MAC by setting the default "false" and
// setting the flag for ignoring the pref to "true" (3rd argument)
gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (false), true);
#elif defined (Q_OS_WIN32)
gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (false));
#else
gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (true));
#endif

gui_pref
global_cursor_blinking ("cursor_blinking", QVariant (true));

gui_pref
global_language ("language", QVariant ("SYSTEM"));

gui_pref
global_ov_startup_dir ("octave_startup_dir", QVariant (QString ()));

gui_pref
global_restore_ov_dir ("restore_octave_dir", QVariant (false));

gui_pref
global_use_custom_editor ("useCustomFileEditor", QVariant (false));

#if defined (Q_OS_WIN32)
gui_pref
global_custom_editor ("customFileEditor", QVariant ("notepad++ -n%l %f"));
#else
gui_pref
global_custom_editor ("customFileEditor", QVariant ("emacs +%l %f"));
#endif

gui_pref
global_prompt_to_exit ("prompt_to_exit", QVariant (false));

// Proxy

gui_pref
global_proxy_host ("proxyHostName", QVariant (QString ()));

gui_pref
global_use_proxy ("useProxyServer", QVariant (false));

gui_pref
global_proxy_type ("proxyType", QVariant (QString ()));

gui_pref
global_proxy_port ("proxyPort", QVariant (80));

gui_pref
global_proxy_user ("proxyUserName", QVariant (QString ()));

gui_pref
global_proxy_pass ("proxyPassword", QVariant (QString ()));
