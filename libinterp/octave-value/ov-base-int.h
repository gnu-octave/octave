////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2023 The Octave Project Developers
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

#if ! defined (octave_ov_base_int_h)
#define octave_ov_base_int_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-scalar.h"
#include "ov-typeinfo.h"

// base int matrix values.

template <typename T>
class
OCTINTERP_API
octave_base_int_matrix : public octave_base_matrix<T>
{
public:

  octave_base_int_matrix (void) : octave_base_matrix<T> () { }

  octave_base_int_matrix (const T& nda) : octave_base_matrix<T> (nda) { }

  ~octave_base_int_matrix (void) = default;

  octave_base_value * clone (void) const
  { return new octave_base_int_matrix (*this); }

  octave_base_value * empty_clone (void) const
  { return new octave_base_int_matrix (); }

  OCTINTERP_API octave_base_value * try_narrowing_conversion (void);

  bool isreal (void) const { return true; }

  //  void increment (void) { matrix += 1; }

  //  void decrement (void) { matrix -= 1; }

  OCTINTERP_API void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  OCTINTERP_API octave_value
  convert_to_str_internal (bool, bool, char type) const;

  OCTINTERP_API octave_value as_double (void) const;
  OCTINTERP_API octave_value as_single (void) const;

  OCTINTERP_API octave_value as_int8 (void) const;
  OCTINTERP_API octave_value as_int16 (void) const;
  OCTINTERP_API octave_value as_int32 (void) const;
  OCTINTERP_API octave_value as_int64 (void) const;

  OCTINTERP_API octave_value as_uint8 (void) const;
  OCTINTERP_API octave_value as_uint16 (void) const;
  OCTINTERP_API octave_value as_uint32 (void) const;
  OCTINTERP_API octave_value as_uint64 (void) const;

  OCTINTERP_API std::string
  edit_display (const float_display_format& fmt,
                octave_idx_type i, octave_idx_type j) const;

  OCTINTERP_API bool save_ascii (std::ostream& os);

  OCTINTERP_API bool load_ascii (std::istream& is);

  OCTINTERP_API bool save_binary (std::ostream& os, bool);

  OCTINTERP_API bool
  load_binary (std::istream& is, bool swap, octave::mach_info::float_format);

protected:

  OCTINTERP_API bool
  save_hdf5_internal (octave_hdf5_id loc_id, octave_hdf5_id save_type,
                      const char *name, bool);

  OCTINTERP_API bool
  load_hdf5_internal (octave_hdf5_id loc_id, octave_hdf5_id save_type,
                      const char *name);
};

// base int scalar values.

template <typename T>
class
OCTINTERP_API
octave_base_int_scalar : public octave_base_scalar<T>
{
public:

  octave_base_int_scalar (void) : octave_base_scalar<T> () { }

  octave_base_int_scalar (const T& s) : octave_base_scalar<T> (s) { }

  ~octave_base_int_scalar (void) = default;

  octave_base_value * clone (void) const
  { return new octave_base_int_scalar (*this); }
  octave_base_value * empty_clone (void) const
  { return new octave_base_int_scalar (); }

  octave_base_value * try_narrowing_conversion (void) { return nullptr; }

  bool isreal (void) const { return true; }

  bool is_real_scalar (void) const { return true; }

  //  void increment (void) { scalar += 1; }

  //  void decrement (void) { scalar -= 1; }

  OCTINTERP_API octave_value
  convert_to_str_internal (bool, bool, char type) const;

  OCTINTERP_API octave_value as_double (void) const;
  OCTINTERP_API octave_value as_single (void) const;

  OCTINTERP_API octave_value as_int8 (void) const;
  OCTINTERP_API octave_value as_int16 (void) const;
  OCTINTERP_API octave_value as_int32 (void) const;
  OCTINTERP_API octave_value as_int64 (void) const;

  OCTINTERP_API octave_value as_uint8 (void) const;
  OCTINTERP_API octave_value as_uint16 (void) const;
  OCTINTERP_API octave_value as_uint32 (void) const;
  OCTINTERP_API octave_value as_uint64 (void) const;

  OCTINTERP_API std::string
  edit_display (const float_display_format& fmt,
                octave_idx_type i, octave_idx_type j) const;

  OCTINTERP_API bool save_ascii (std::ostream& os);

  OCTINTERP_API bool load_ascii (std::istream& is);

  OCTINTERP_API bool save_binary (std::ostream& os, bool);

  OCTINTERP_API bool load_binary (std::istream& is, bool swap,
                                  octave::mach_info::float_format);
protected:

  OCTINTERP_API bool
  save_hdf5_internal (octave_hdf5_id loc_id, octave_hdf5_id save_type,
                      const char *name, bool);

  OCTINTERP_API bool
  load_hdf5_internal (octave_hdf5_id loc_id, octave_hdf5_id save_type,
                      const char *name);
};

#endif
