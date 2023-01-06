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

#if ! defined (octave_gui_preferences_fb_h)
#define octave_gui_preferences_fb_h 1

#include "gui-preferences.h"

// Files dock widget

const gui_pref
fb_column_state ("filesdockwidget/column_state", QVariant ());

const gui_pref
fb_mru_list ("filesdockwidget/mru_dir_list", QVariant (QStringList ()));

const gui_pref
fb_show_size ("filesdockwidget/showFileSize", QVariant (false));

const gui_pref
fb_show_type ("filesdockwidget/showFileType", QVariant (false));

const gui_pref
fb_show_date ("filesdockwidget/showLastModified", QVariant (false));

const gui_pref
fb_show_hidden ("filesdockwidget/showHiddenFiles", QVariant (false));

const gui_pref
fb_show_altcol ("filesdockwidget/useAlternatingRowColors", QVariant (true));

const gui_pref
fb_sort_column ("filesdockwidget/sort_files_by_column", QVariant (0));

const gui_pref
fb_sort_order ("filesdockwidget/sort_files_by_order",
               QVariant (Qt::AscendingOrder));

const gui_pref
fb_sync_octdir ("filesdockwidget/sync_octave_directory", QVariant (true));

const gui_pref
fb_restore_last_dir ("filesdockwidget/restore_last_dir", QVariant (false));

const gui_pref
fb_startup_dir ("filesdockwidget/startup_dir", QVariant (QString ()));

const gui_pref
fb_txt_file_ext ("filesdockwidget/txt_file_extensions",
                 QVariant ("m;c;cc;cpp;h;txt"));

#endif
