////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2024 The Octave Project Developers
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

extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<int8NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<int16NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<int32NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<int64NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<uint8NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<uint16NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<uint32NDArray>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_matrix<uint64NDArray>;

template <typename T>
class OCTINTERP_TEMPLATE_API octave_base_int_matrix : public octave_base_matrix<T>
{
public:

  octave_base_int_matrix () : octave_base_matrix<T> () { }

  octave_base_int_matrix (const T& nda) : octave_base_matrix<T> (nda) { }

  ~octave_base_int_matrix () = default;

  octave_base_value * clone () const
  { return new octave_base_int_matrix (*this); }

  octave_base_value * empty_clone () const
  { return new octave_base_int_matrix (); }

  OCTINTERP_API octave_base_value * try_narrowing_conversion ();

  OCTINTERP_OVERRIDABLE_FUNC_API bool isreal () const { return true; }

  //  void increment () { matrix += 1; }

  //  void decrement () { matrix -= 1; }

  OCTINTERP_API void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  OCTINTERP_API octave_value
  convert_to_str_internal (bool, bool, char type) const;

  OCTINTERP_API octave_value as_double () const;
  OCTINTERP_API octave_value as_single () const;

  OCTINTERP_API octave_value as_int8 () const;
  OCTINTERP_API octave_value as_int16 () const;
  OCTINTERP_API octave_value as_int32 () const;
  OCTINTERP_API octave_value as_int64 () const;

  OCTINTERP_API octave_value as_uint8 () const;
  OCTINTERP_API octave_value as_uint16 () const;
  OCTINTERP_API octave_value as_uint32 () const;
  OCTINTERP_API octave_value as_uint64 () const;

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

extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_int8>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_int16>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_int32>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_int64>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_uint8>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_uint16>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_uint32>;
extern template class OCTINTERP_EXTERN_TEMPLATE_API octave_base_scalar<octave_uint64>;

template <typename T>
class OCTINTERP_TEMPLATE_API octave_base_int_scalar : public octave_base_scalar<T>
{
public:

  OCTINTERP_OVERRIDABLE_FUNC_API
  octave_base_int_scalar () : octave_base_scalar<T> () { }

  OCTINTERP_OVERRIDABLE_FUNC_API
  octave_base_int_scalar (const T& s) : octave_base_scalar<T> (s) { }

  OCTINTERP_OVERRIDABLE_FUNC_API ~octave_base_int_scalar () = default;

  OCTINTERP_OVERRIDABLE_FUNC_API octave_base_value * clone () const
  { return new octave_base_int_scalar (*this); }
  OCTINTERP_OVERRIDABLE_FUNC_API octave_base_value * empty_clone () const
  { return new octave_base_int_scalar (); }

  OCTINTERP_OVERRIDABLE_FUNC_API octave_base_value *
  try_narrowing_conversion ()
  { return nullptr; }

  OCTINTERP_OVERRIDABLE_FUNC_API bool is_maybe_function () const
  { return false; }

  OCTINTERP_OVERRIDABLE_FUNC_API bool isreal () const
  { return true; }

  OCTINTERP_OVERRIDABLE_FUNC_API bool is_real_scalar () const
  { return true; }

  //  void increment () { scalar += 1; }

  //  void decrement () { scalar -= 1; }

  OCTINTERP_API octave_value
  convert_to_str_internal (bool, bool, char type) const;

  OCTINTERP_API octave_value as_double () const;
  OCTINTERP_API octave_value as_single () const;

  OCTINTERP_API octave_value as_int8 () const;
  OCTINTERP_API octave_value as_int16 () const;
  OCTINTERP_API octave_value as_int32 () const;
  OCTINTERP_API octave_value as_int64 () const;

  OCTINTERP_API octave_value as_uint8 () const;
  OCTINTERP_API octave_value as_uint16 () const;
  OCTINTERP_API octave_value as_uint32 () const;
  OCTINTERP_API octave_value as_uint64 () const;

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
