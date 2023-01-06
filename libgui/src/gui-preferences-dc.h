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

#if ! defined (octave_gui_preferences_dc_h)
#define octave_gui_preferences_dc_h 1

#include "gui-preferences.h"

// documentation properties

const QString
dc_bookmark_file ("octave-doc-bookmarks.xbel");
const gui_pref
dc_bookmark_filter_active ("documentation_widget/filter_active", QVariant (false));

const gui_pref
dc_bookmark_filter_shown ("documentation_widget/filter_shown", QVariant (true));

const gui_pref
dc_bookmark_filter_mru ("documentation_widget/bookmark_filter_mru", QVariant ());

// Constants for the xbel file format
const QLatin1String dc_xbel_doctype ("<!DOCTYPE xbel>");
const QLatin1String dc_xbel_attr_href ("href");
const QLatin1String dc_xbel_attr_folded ("folded");
const QLatin1String dc_xbel_attr_version ("version");
const QLatin1String dc_xbel_value_version ("1.0");
const QLatin1String dc_xbel_value_yes ("yes");
const QLatin1String dc_xbel_value_no ("no");
const QLatin1String dc_xbel_name_title ("title");
const QLatin1String dc_xbel_name_folder ("folder");
const QLatin1String dc_xbel_name_bookmark ("bookmark");
const QLatin1String dc_xbel_name_format ("xbel");

// Zoom level
const gui_pref
dc_browser_zoom_level ("documentation_widget/browser_zoom_level", QVariant (0));

#endif
