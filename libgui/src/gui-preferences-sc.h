/*

Copyright (C) 2017-2019 Torsten Lilge <ttl-octave@mailbox.de>

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

#if ! defined (octave_gui_preferences_sc_h)
#define octave_gui_preferences_sc_h 1

#include "gui-preferences.h"

// Define shortcuts

// The shortcut's default values are given as QKeySequence for being able
// to use platform independent standard keys (QKeySequence::StandardKey).
// However, converting key sequences into QVariants does not seem to
// revertable. In addition the related string (which is saved in the
// preferences file) can not be determined during compile time since the
// result depends on the platform (at least in case of standard key sequences
// like, e.g., QKeySequence::Copy)
// Therefore, these prefs for key sequences require a separate constant
// definition and value method for the settings class.



const sc_pref sc_main_edit_copy ("shortcuts/main_edit:copy", Qt::CTRL + Qt::Key_C);

// Other normal, shortcut related options

const gui_pref
sc_main_ctrld ("shortcuts/main_ctrld", QVariant (false));
#endif
