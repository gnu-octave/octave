/*

Copyright (C) 2019 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QSettings>

#include "gui-settings.h"

namespace octave
{

  QVariant gui_settings::sc_value (const sc_pref& pref) const
  {
    QKeySequence key_seq = QKeySequence ();

    // Check, which of the elements for the default value in the sc_pref
    // structure has a valid value and take this as default. If both
    // elements are not valid, leave the key sequence empty
    if (pref.def)
      key_seq = QKeySequence (pref.def);
    else if (pref.def_std != QKeySequence::UnknownKey)
      key_seq = QKeySequence (pref.def_std);

    // Get the value from the settings where the key sequences are stored
    // as strings
    return value (pref.key, key_seq.toString ());
  }

  // Additional gui_settings functions will be added later.

}
