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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "ov-lazy-idx.h"
#include "ops.h"
#include "ov-scalar.h"
#include "ls-oct-text.h"
#include "ls-oct-binary.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_lazy_index, "lazy_index", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_lazy_index& v = dynamic_cast<const octave_lazy_index&> (a);

  return v.full_value ().clone ();
}

octave_base_value::type_conv_info
octave_lazy_index::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
         octave_matrix::static_type_id ());
}

octave_base_value *
octave_lazy_index::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  switch (m_index.length (0))
    {
    case 1:
      retval = new octave_scalar (static_cast<double> (m_index(0) + 1));
      break;

    case 0:
      retval = new octave_matrix (NDArray (m_index.orig_dimensions ()));
      break;

    default:
      break;
    }

  return retval;
}

octave_value
octave_lazy_index::fast_elem_extract (octave_idx_type n) const
{
  return double (m_index.checkelem (n) + 1);
}

octave_value
octave_lazy_index::reshape (const dim_vector& new_dims) const
{
  return octave::idx_vector (m_index.as_array ().reshape (new_dims),
                             m_index.extent (0));
}

octave_value
octave_lazy_index::permute (const Array<int>& vec, bool inv) const
{
  // If the conversion has already been made, forward the operation.
  if (m_value.is_defined ())
    return m_value.permute (vec, inv);
  else
    return octave::idx_vector (m_index.as_array ().permute (vec, inv),
                               m_index.extent (0));
}

octave_value
octave_lazy_index::squeeze (void) const
{
  return octave::idx_vector (m_index.as_array ().squeeze (),
                             m_index.extent (0));
}

octave_value
octave_lazy_index::sort (octave_idx_type dim, sortmode mode) const
{
  const dim_vector odims = m_index.orig_dimensions ();
  // index_vector can employ a more efficient sorting algorithm.
  if (mode == ASCENDING && odims.ndims () == 2
      && (dim >= 0 && dim <= 1) && odims(1-dim) == 1)
    return index_vector ().sorted ();
  else
    return octave::idx_vector (m_index.as_array ().sort (dim, mode),
                               m_index.extent (0));
}

octave_value
octave_lazy_index::sort (Array<octave_idx_type>& sidx, octave_idx_type dim,
                         sortmode mode) const
{
  const dim_vector odims = m_index.orig_dimensions ();
  // index_vector can employ a more efficient sorting algorithm.
  if (mode == ASCENDING && odims.ndims () == 2
      && (dim >= 0 && dim <= 1) && odims(1-dim) == 1)
    return index_vector ().sorted (sidx);
  else
    return octave::idx_vector (m_index.as_array ().sort (sidx, dim, mode),
                               m_index.extent (0));
}

sortmode
octave_lazy_index::issorted (sortmode mode) const
{
  if (m_index.is_range ())
    {
      // Avoid the array conversion.
      octave_idx_type inc = m_index.increment ();
      if (inc == 0)
        return (mode == UNSORTED ? ASCENDING : mode);
      else if (inc > 0)
        return (mode == DESCENDING ? UNSORTED : ASCENDING);
      else
        return (mode == ASCENDING ? UNSORTED : DESCENDING);
    }
  else
    return m_index.as_array ().issorted (mode);
}

Array<octave_idx_type>
octave_lazy_index::sort_rows_idx (sortmode mode) const
{
  return m_index.as_array ().sort_rows_idx (mode);
}

sortmode
octave_lazy_index::is_sorted_rows (sortmode mode) const
{
  return m_index.as_array ().is_sorted_rows (mode);
}

octave_value
octave_lazy_index::as_double (void) const
{
  return array_value ();
}

octave_value
octave_lazy_index::as_single (void) const
{
  return float_array_value ();
}

octave_value
octave_lazy_index::as_int8 (void) const
{
  return int8_array_value  ();
}

octave_value
octave_lazy_index::as_int16 (void) const
{
  return int16_array_value ();
}

octave_value
octave_lazy_index::as_int32 (void) const
{
  return int32_array_value ();
}

octave_value
octave_lazy_index::as_int64 (void) const
{
  return int64_array_value ();
}

octave_value
octave_lazy_index::as_uint8 (void) const
{
  return uint8_array_value ();
}

octave_value
octave_lazy_index::as_uint16 (void) const
{
  return uint16_array_value ();
}

octave_value
octave_lazy_index::as_uint32 (void) const
{
  return uint32_array_value ();
}

octave_value
octave_lazy_index::as_uint64 (void) const
{
  return uint64_array_value ();
}

static const std::string value_save_tag ("index_value");

bool octave_lazy_index::save_ascii (std::ostream& os)
{
  return save_text_data (os, make_value (), value_save_tag, false, 0);
}

bool octave_lazy_index::load_ascii (std::istream& is)
{
  bool dummy;

  std::string nm = read_text_data (is, "", dummy, m_value, 0);
  if (nm != value_save_tag)
    error ("lazy_index: corrupted data on load");

  m_index = m_value.index_vector ();

  return true;
}

bool octave_lazy_index::save_binary (std::ostream& os, bool save_as_floats)
{
  return save_binary_data (os, make_value (), value_save_tag,
                           "", false, save_as_floats);
}

bool octave_lazy_index::load_binary (std::istream& is, bool swap,
                                     octave::mach_info::float_format fmt)
{
  bool dummy;
  std::string doc;

  std::string nm = read_binary_data (is, swap, fmt, "", dummy, m_value, doc);
  if (nm != value_save_tag)
    error ("lazy_index: corrupted data on load");

  m_index = m_value.index_vector ();

  return true;
}

/*
%!shared x, y
%! x = find ([-1, 0, -2, 1, 3, -4] < 0);
%! y = [1, 3, 6];
%!assert (typeinfo (x), "lazy_index")
%!assert (double (x), y)
%!assert (single (x), single (y))
%!assert (int8 (x), int8 (y))
%!assert (int16 (x), int16 (y))
%!assert (int32 (x), int32 (y))
%!assert (int64 (x), int64 (y))
%!assert (uint8 (x), uint8 (y))
%!assert (uint16 (x), uint16 (y))
%!assert (uint32 (x), uint32 (y))
%!assert (uint64 (x), uint64 (y))
*/
