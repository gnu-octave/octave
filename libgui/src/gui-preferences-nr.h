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

#if ! defined (octave_gui_preferences_nr_h)
#define octave_gui_preferences_nr_h 1

#include "gui-preferences.h"
#include <QDateTime>

// News Reader properties

const gui_pref
nr_last_time ("news/last_time_checked", QVariant (QDateTime ()));

const gui_pref
nr_last_news ("news/last_news_item", QVariant (0));

const gui_pref
nr_allow_connection ("news/allow_web_connection", QVariant (false));

#endif
