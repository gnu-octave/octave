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

#if ! defined (octave_gui_preferences_global_h)
#define octave_gui_preferences_global_h 1

#include <QStyle>

#include "gui-preferences.h"

// Constants

const QString gui_obj_name_main_window = "MainWindow";

// Global preferences

extern gui_pref global_skip_welcome_wizard;

// Get the default monospaced font
#if defined (Q_OS_WIN)
const QString global_font_family = "Courier";
#elif defined (Q_OS_MAC)
const QString global_font_family = "Monaco";
#else
const QString global_font_family = "Monospace";
#endif

extern gui_pref global_mono_font;

// Style

extern gui_pref global_style;

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

extern gui_pref global_icon_size;

extern gui_pref global_icon_theme;

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

extern gui_pref global_icon_theme_index;

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

extern gui_pref global_icon_fallbacks;

extern gui_pref global_status_bar;

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
extern gui_pref global_use_native_dialogs;
#elif defined (Q_OS_WIN32)
extern gui_pref global_use_native_dialogs;
#else
extern gui_pref global_use_native_dialogs;
#endif

extern gui_pref global_cursor_blinking;

extern gui_pref global_language;

extern gui_pref global_ov_startup_dir;

extern gui_pref global_restore_ov_dir;

extern gui_pref global_use_custom_editor;

#if defined (Q_OS_WIN32)
extern gui_pref global_custom_editor;
#else
extern gui_pref global_custom_editor;
#endif

extern gui_pref global_prompt_to_exit;

// Proxy

extern gui_pref global_proxy_host;

extern gui_pref global_use_proxy;

extern gui_pref global_proxy_type;

extern gui_pref global_proxy_port;

extern gui_pref global_proxy_user;

extern gui_pref global_proxy_pass;

const QStringList
global_proxy_all_types = {
  "HttpProxy",
  "Socks5Proxy",
  QT_TRANSLATE_NOOP ("octave::settings_dialog", "Environment Variables")
};

const QList<int> global_proxy_manual_types = { 0, 1 };

#endif
