////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
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

#if ! defined (octave_ov_oncleanup_h)
#define octave_ov_oncleanup_h 1

#include "octave-config.h"

#include <iosfwd>

#include "ov-base.h"
#include "ov-struct.h"
#include "ov.h"


class octave_oncleanup : public octave_base_value
{
public:

  octave_oncleanup (void) = default;

  octave_oncleanup (const octave_value& m_fcn);

  octave_base_value * clone (void) const
  {
    if (m_fcn.is_defined ())
      error ("onCleanup: internal error: cloning nonempty object");

    return empty_clone ();
  }

  octave_base_value * empty_clone (void) const
  {
    return new octave_oncleanup ();
  }

  ~octave_oncleanup (void);

  bool is_defined (void) const { return true; }

  bool isobject (void) const { return true; } // do we want this?

  octave_map map_value (void) const { return scalar_map_value (); }

  octave_scalar_map scalar_map_value (void) const;

  dim_vector dims (void) const
  {
    static dim_vector dv (1, 1);
    return dv;
  }

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  void call_object_destructor (void);

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

protected:

  octave_value m_fcn;
};

#endif
