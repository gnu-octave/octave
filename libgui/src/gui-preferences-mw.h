////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#if ! defined (octave_gui_preference_mw_h)
#define octave_gui_preference_mw_h 1

#include "gui-preferences.h"

// Main window preferences

// Geometry wihtout default since default layout is built programmatically
const gui_pref
mw_geometry ("MainWindow/geometry", QVariant (QByteArray ()));

// State wihtout default since default layout is built programmatically
const gui_pref
mw_state ("MainWindow/windowState", QVariant (QByteArray ()));

const gui_pref
mw_dir_list ("MainWindow/current_directory_list", QVariant (QStringList ()));

#endif
