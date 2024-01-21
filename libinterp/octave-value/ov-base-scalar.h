////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if ! defined (octave_ov_base_scalar_h)
#define octave_ov_base_scalar_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "lo-mappers.h"
#include "lo-utils.h"
#include "str-vec.h"
#include "MatrixType.h"

#include "ov-base.h"
#include "ov-typeinfo.h"

// Real scalar values.

template <typename ST>
class OCTINTERP_API octave_base_scalar : public octave_base_value
{
public:

  typedef ST scalar_type;

  OCTINTERP_API octave_base_scalar ()
    : octave_base_value (), scalar () { }

  OCTINTERP_API octave_base_scalar (const ST& s)
    : octave_base_value (), scalar (s) { }

  OCTINTERP_API octave_base_scalar (const octave_base_scalar& s)
    : octave_base_value (), scalar (s.scalar) { }

  OCTINTERP_API ~octave_base_scalar () = default;

  OCTINTERP_API octave_value squeeze () const { return scalar; }

  OCTINTERP_API octave_value full_value () const { return scalar; }

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  OCTINTERP_API octave_value
  subsref (const std::string& type, const std::list<octave_value_list>& idx);

  OCTINTERP_API octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx, int)
  { return subsref (type, idx); }

  OCTINTERP_API octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  OCTINTERP_API bool is_constant () const { return true; }

  OCTINTERP_API bool is_defined () const { return true; }

  OCTINTERP_API dim_vector dims () const;

  OCTINTERP_API octave_idx_type numel () const { return 1; }

  OCTINTERP_API int ndims () const { return 2; }

  OCTINTERP_API octave_idx_type nnz () const { return (scalar != ST () ? 1 : 0); }

  OCTINTERP_API octave_value permute (const Array<int>&, bool = false) const;

  OCTINTERP_API octave_value reshape (const dim_vector& new_dims) const;

  OCTINTERP_API std::size_t byte_size () const { return sizeof (ST); }

  OCTINTERP_API octave_value all (int = 0) const { return (scalar != ST ()); }

  OCTINTERP_API octave_value any (int = 0) const { return (scalar != ST ()); }

  OCTINTERP_API octave_value diag (octave_idx_type k = 0) const;

  OCTINTERP_API octave_value diag (octave_idx_type m, octave_idx_type n) const;

  OCTINTERP_API octave_value sort (octave_idx_type, sortmode) const
  { return octave_value (scalar); }

  OCTINTERP_API octave_value
  sort (Array<octave_idx_type>& sidx, octave_idx_type, sortmode) const
  {
    sidx.resize (dim_vector (1, 1));
    sidx(0) = 0;
    return octave_value (scalar);
  }

  OCTINTERP_API sortmode issorted (sortmode mode = UNSORTED) const
  { return mode == UNSORTED ? ASCENDING : mode; }

  OCTINTERP_API Array<octave_idx_type> sort_rows_idx (sortmode) const
  {
    return Array<octave_idx_type> (dim_vector (1, 1),
                                   static_cast<octave_idx_type> (0));
  }

  OCTINTERP_API sortmode is_sorted_rows (sortmode mode = UNSORTED) const
  { return mode == UNSORTED ? ASCENDING : mode; }

  OCTINTERP_API MatrixType matrix_type () const
  { return MatrixType::Diagonal; }

  OCTINTERP_API MatrixType matrix_type (const MatrixType&) const
  { return matrix_type (); }

  OCTINTERP_API bool is_maybe_function () const { return false; }

  OCTINTERP_API bool is_scalar_type () const { return true; }

  OCTINTERP_API bool isnumeric () const { return true; }

  OCTINTERP_API bool is_true () const;

  OCTINTERP_API void print (std::ostream& os, bool pr_as_read_syntax = false);

  OCTINTERP_API void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  OCTINTERP_API bool
  print_name_tag (std::ostream& os, const std::string& name) const;

  OCTINTERP_API void short_disp (std::ostream& os) const;

  OCTINTERP_API float_display_format get_edit_display_format () const;

  OCTINTERP_API std::string
  edit_display (const float_display_format& fmt,
                octave_idx_type i, octave_idx_type j) const;

  // This function exists to support the MEX interface.
  // You should not use it anywhere else.
  OCTINTERP_API const void * mex_get_data () const { return &scalar; }

  OCTINTERP_API const ST& scalar_ref () const { return scalar; }

  OCTINTERP_API ST& scalar_ref () { return scalar; }

  OCTINTERP_API octave_value fast_elem_extract (octave_idx_type n) const;

  OCTINTERP_API bool
  fast_elem_insert_self (void *where, builtin_type_t btyp) const;

protected:

  // The value of this scalar.
  ST scalar;
};

#endif
