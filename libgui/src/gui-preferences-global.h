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

#if ! defined (octave_gui_preferences_global_h)
#define octave_gui_preferences_global_h 1

#include "gui-preferences.h"

// Constants

const QString gui_obj_name_main_window = "MainWindow";

// Global preferences

// Get the default monospaced font
#if defined (Q_OS_WIN)
const QString global_font_family = "Courier";
#elif defined (Q_OS_MAC)
const QString global_font_family = "Monaco";
#else
const QString global_font_family = "Monospace";
#endif

const gui_pref
global_mono_font ("monospace_font", global_font_family);

// Style

const gui_pref
global_style ("style", QVariant ("default"));

const QString
global_toolbar_style ("QToolBar {"
                      "margin-top: 0px;"
                      "margin-bottom: 0px;"
                      "padding-top: 0px;"
                      "padding-bottom: 0px;"
                      "border-top: 0px;"
                      "border-bottom: 0px;"
                      "}");

const QString
global_menubar_style ("QMenuBar {"
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

enum
{
  ICON_THEME_SYSTEM = 0,
  ICON_THEME_OCTAVE,
  ICON_THEME_TANGO,
  ICON_THEME_CURSORS
}  ;

const QStringList
global_icon_paths = {
  "",
  ":/icons/octave/128x128/",
  ":/icons/tango/128x128/",
  ":/icons/cursors/"
};

const gui_pref
global_icon_theme_index ("icon_theme", QVariant (ICON_THEME_SYSTEM));
const QStringList
global_all_icon_themes = {
  "",
  "octave",
  "tango",
  "cursors"
};

const QStringList
global_all_icon_theme_names = {
  "System",
  "Octave",
  "Tango"
};

const gui_pref
global_status_bar ("show_status_bar", QVariant (true));


enum
{
  EXTRA_STYLE_FUSION_DARK = 0
}  ;
const QStringList
global_extra_styles = {
  "Fusion-Dark"
};

#if defined (Q_OS_MAC)
// prevent native file dialogs on MAC by setting the default "false" and
// setting the flag for ignoring the pref to "true" (3rd argument)
const gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (false), true);
#elif defined (Q_OS_WIN32)
const gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (false));
#else
const gui_pref
global_use_native_dialogs ("use_native_file_dialogs", QVariant (true));
#endif

const gui_pref
global_cursor_blinking ("cursor_blinking", QVariant (true));

const gui_pref
global_language ("language", QVariant ("SYSTEM"));

const gui_pref
global_ov_startup_dir ("octave_startup_dir", QVariant (QString ()));
const gui_pref
global_restore_ov_dir ("restore_octave_dir", QVariant (false));

const gui_pref
global_use_custom_editor ("useCustomFileEditor", QVariant (false));

#if defined (Q_OS_WIN32)
const gui_pref
global_custom_editor ("customFileEditor", QVariant ("notepad++ -n%l %f"));
#else
const gui_pref
global_custom_editor ("customFileEditor", QVariant ("emacs +%l %f"));
#endif

const gui_pref
global_prompt_to_exit ("prompt_to_exit", QVariant (false));

// Proxy

const gui_pref
global_proxy_host ("proxyHostName", QVariant (QString ()));
const gui_pref
global_use_proxy ("useProxyServer", QVariant (false));
const gui_pref
global_proxy_type ("proxyType", QVariant (QString ()));
const gui_pref
global_proxy_port ("proxyPort", QVariant (80));
const gui_pref
global_proxy_user ("proxyUserName", QVariant (QString ()));
const gui_pref
global_proxy_pass ("proxyPassword", QVariant (QString ()));

const QStringList
global_proxy_all_types = {
  "HttpProxy",
  "Socks5Proxy",
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Environment Variables")
};

const QList<int> global_proxy_manual_types = { 0, 1 };

#endif
