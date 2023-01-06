////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "errwarn.h"
#include "oct-hdf5.h"

OCTAVE_BEGIN_NAMESPACE(octave)

bool
check_hdf5_types (bool warn)
{
  static bool checked = false;
  static bool ok = false;

  if (! checked)
    {
#if defined (HAVE_HDF5)

      ok = sizeof (octave_hdf5_id) >= sizeof (hid_t);

      if (warn && ! ok)
        warning_with_id
        ("Octave:internal",
         "the size of octave_hdf5_id is smaller than the size of HDF5 hid_t");

      ok = sizeof (octave_hdf5_err) >= sizeof (herr_t);

      if (warn && ! ok)
        warning_with_id
        ("Octave:internal",
         "the size of octave_hdf5_err is smaller than the size of HDF5 herr_t");
#else

      octave_unused_parameter (warn);

      warn_disabled_feature ("check_hdf5_id_type", "HDF5");

#endif

      checked = true;
    }

  return ok;
}

OCTAVE_END_NAMESPACE(octave)
