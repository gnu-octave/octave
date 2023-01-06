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

#if ! defined (octave_gui_preferences_ff_h)
#define octave_gui_preferences_ff_h 1

#include "gui-preferences.h"

// Find files dialog preferences

const gui_pref
ff_file_name ("findfiles/file_name", QVariant ("*"));

const gui_pref
ff_start_dir ("findfiles/start_dir", QVariant (""));

const gui_pref
ff_recurse_dirs ("findfiles/recurse_dirs", QVariant (false));

const gui_pref
ff_include_dirs ("findfiles/include_dirs", QVariant (false));

const gui_pref
ff_name_case ("findfiles/name_case", QVariant (false));

const gui_pref
ff_check_text ("findfiles/check_text", QVariant (false));

const gui_pref
ff_contains_text ("findfiles/contains_text", QVariant (""));

const gui_pref
ff_content_case ("findfiles/content_case", QVariant (false));

const gui_pref
ff_column_state ("findfiles/column_state", QVariant ());

const gui_pref
ff_sort_files_by_column ("findfiles/sort_files_by_column", QVariant (0));

const gui_pref
ff_sort_files_by_order ("findfiles/sort_files_by_order",
                        QVariant (Qt::AscendingOrder));


#endif
