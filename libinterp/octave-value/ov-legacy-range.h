////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_ov_legacy_range_h)
#define octave_ov_legacy_range_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <memory>
#include <string>

#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

class Range;

class octave_value_list;

// Legacy Range values.

// Provide enough of the old octave_range class to allow Range objects
// to be loaded from files.  After loading, they are converted to some
// other type by a call to octave_value::maybe_mutate in
// load_save_system::load_vars so there should no longer be any values
// of this type used by the interpreter.  The action of maybe_mutate is
// performed by octave_legacy_range::try_narrowing_conversion.

class
octave_legacy_range : public octave_base_value
{
public:

  octave_legacy_range ();

  octave_legacy_range (const Range& r);

  octave_legacy_range (const octave_legacy_range& r);

  // No assignment.

  octave_legacy_range& operator = (const octave_legacy_range&) = delete;

  ~octave_legacy_range () = default;

  octave_base_value * clone () const
  {
    return new octave_legacy_range (*this);
  }

  // A range is really just a special kind of real matrix object.  In
  // the places where we need to call empty_clone, it makes more sense
  // to create an empty matrix (0x0) instead of an empty range (1x0).
  octave_base_value * empty_clone () const { return new octave_matrix (); }

  type_conv_info numeric_conversion_function () const;

  octave_base_value * try_narrowing_conversion ();

  bool is_defined () const { return true; }

  bool is_legacy_object () const { return true; }

  bool is_constant () const { return true; }

  bool load_ascii (std::istream& is);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

private:

  std::unique_ptr<Range> m_range;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
