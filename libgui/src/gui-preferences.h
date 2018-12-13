/*

Copyright (C) 2017-2018 Torsten <mttl@mailbox.de>

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

#if ! defined (octave_editor_settings_h)
#define octave_editor_settings_h 1

//#if defined (HAVE_CONFIG_H)
//#  include "config.h"
//#endif

#include <QStringList>
#include <QVariant>
#include <QStyle>

// Structure for the definition of pairs: key and default value

struct gui_pref
{
  gui_pref (const QString& key_, QVariant def_): key (key_), def (def_) {}
  QString   key;  // the key name
  QVariant  def;  // the default value
};


// Global preferences

// Get the default monospaced font
#if defined (Q_WS_X11)
const QString global_font_family = "Monospace";
#elif defined (Q_WS_WIN) || defined (Q_WS_MAC)
const QString global_font_family = "Courier";
#else
const QString global_font_family = "Courier";
#endif
const gui_pref global_mono_font ("monospace_font", global_font_family);

// Icon size (in preferences: values -1, 0, 1)
const QStyle::PixelMetric global_icon_sizes[3] =
{
  QStyle::PM_SmallIconSize,
  QStyle::PM_ToolBarIconSize,
  QStyle::PM_LargeIconSize
};
const gui_pref global_icon_size ("toolbar_icon_size", QVariant (0));
const gui_pref global_icon_theme ("use_system_icon_theme", QVariant (true));

// Style
const gui_pref global_style ("style", QVariant ("default"));

// Console preferences

const gui_pref cs_font ("terminal/fontName", QVariant ());


// Variable Editor preferences

const gui_pref ve_font ("variable_editor/font_size", QVariant ());


// Editor preferences

// Octave comment strings
const gui_pref ed_comment_str_old ("editor/octave_comment_string", QVariant (0));
const gui_pref ed_comment_str ("editor/oct_comment_str", QVariant (0));
const gui_pref ed_uncomment_str ("editor/oct_uncomment_str", QVariant (1 + 2 + 4 + 8));
const QString ed_last_comment_str ("editor/oct_last_comment_str");
const QStringList ed_comment_strings (QStringList () << "##" << "#" << "%"<< "%%" << "%!");
const int ed_comment_strings_count = 5;


// Session data
const gui_pref ed_session_names ("editor/savedSessionTabs",
                                  QVariant (QStringList ()));
const gui_pref ed_session_enc ("editor/saved_session_encodings",
                                  QVariant (QStringList ()));
const gui_pref ed_session_ind ("editor/saved_session_tab_index",
                                  QVariant (QStringList ()));
const gui_pref ed_session_lines ("editor/saved_session_lines",
                                  QVariant (QStringList ()));


// File handling
const gui_pref ed_show_dbg_file ("editor/show_dbg_file", QVariant (true));
#if defined (Q_OS_WIN32)
const gui_pref ed_default_enc ("editor/default_encoding", QVariant ("SYSTEM"));
#else
const gui_pref ed_default_enc ("editor/default_encoding", QVariant ("UTF-8"));
#endif


// Files dock widget

const gui_pref fb_column_state ("filesdockwidget/column_state", QVariant ());
const gui_pref fb_show_ ("filesdockwidget/column_state", QVariant ());
const gui_pref fb_mru_list ("filesdockwidget/mru_dir_list", QVariant (QStringList ()));
const gui_pref fb_show_size ("filesdockwidget/showFileSize", QVariant (false));
const gui_pref fb_show_type ("filesdockwidget/showFileType", QVariant (false));
const gui_pref fb_show_date ("filesdockwidget/showLastModified", QVariant (false));
const gui_pref fb_show_hidden ("filesdockwidget/showHiddenFiles", QVariant (false));
const gui_pref fb_show_altcol ("filesdockwidget/useAlternatingRowColors", QVariant (true));
const gui_pref fb_sort_column ("filesdockwidget/sort_files_by_column", QVariant (0));
const gui_pref fb_sort_order ("filesdockwidget/sort_files_by_order", QVariant (Qt::AscendingOrder));
const gui_pref fb_sync_octdir ("filesdockwidget/sync_octave_directory", QVariant (true));
const gui_pref fb_restore_last_dir ("filesdockwidget/restore_last_dir", QVariant (false));
const gui_pref fb_startup_dir ("filesdockwidget/startup_dir", QVariant (QString ()));
const gui_pref fb_txt_file_ext ("filesdockwidget/txt_file_extensions",
                                QVariant ("m;c;cc;cpp;h;txt"));

// Workspace view

const gui_pref ws_enable_colors ("workspaceview/enable_colors", QVariant (false));
const gui_pref ws_hide_tool_tips ("workspaceview/hide_tools_tips", QVariant (false));

#endif
