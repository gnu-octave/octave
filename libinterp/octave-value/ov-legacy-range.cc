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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <istream>
#include <ostream>
#include <sstream>

#include "lo-ieee.h"
#include "lo-utils.h"

#include "variables.h"
#include "error.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "ov-legacy-range.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_legacy_range, "range", "double");

octave_legacy_range::octave_legacy_range (void)
  : octave_base_value (), range () { }

octave_legacy_range::octave_legacy_range (const Range& r)
  : octave_base_value (), range (r)
{
  if (range.numel () < 0 && range.numel () != -2)
    error ("invalid range");
}

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_legacy_range& v = dynamic_cast<const octave_legacy_range&> (a);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_legacy_range::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
         octave_matrix::static_type_id ());
}

octave_base_value *
octave_legacy_range::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  switch (range.numel ())
    {
    case 1:
      retval = new octave_scalar (range.base ());
      break;

    case 0:
      retval = new octave_matrix (Matrix (1, 0));
      break;

    case -2:
      retval = new octave_matrix (range.matrix_value ());
      break;

    default:
      {
        if (range.increment () == 0)
          retval = new octave_matrix (range.matrix_value ());
        else
          retval = new octave_range
          (octave::range<double> (range.base (), range.increment (),
                                  range.limit (), range.numel ()));
      }
      break;
    }

  return retval;
}

// Skip white space and comments on stream IS.

static void
skip_comments (std::istream& is)
{
  char c = '\0';
  while (is.get (c))
    {
      if (c == ' ' || c == '\t' || c == '\n')
        ; // Skip whitespace on way to beginning of next line.
      else
        break;
    }

  octave::skip_until_newline (is, false);
}

bool
octave_legacy_range::load_ascii (std::istream& is)
{
  // # base, limit, range comment added by save ().
  skip_comments (is);

  double base, limit, inc;
  is >> base >> limit >> inc;

  if (! is)
    error ("load: failed to load range constant");

  if (inc != 0)
    range = Range (base, limit, inc);
  else
    range = Range (base, inc, static_cast<octave_idx_type> (limit));

  return true;
}

bool
octave_legacy_range::load_binary (std::istream& is, bool swap,
                                  octave::mach_info::float_format /* fmt */)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;
  double bas, lim, inc;
  if (! is.read (reinterpret_cast<char *> (&bas), 8))
    return false;
  if (swap)
    swap_bytes<8> (&bas);
  if (! is.read (reinterpret_cast<char *> (&lim), 8))
    return false;
  if (swap)
    swap_bytes<8> (&lim);
  if (! is.read (reinterpret_cast<char *> (&inc), 8))
    return false;
  if (swap)
    swap_bytes<8> (&inc);
  if (inc != 0)
    range = Range (bas, lim, inc);
  else
    range = Range (bas, inc, static_cast<octave_idx_type> (lim));

  return true;
}

#if defined (HAVE_HDF5)

// The following subroutines creates an HDF5 representation of the way
// we will store Octave range types (triplets of floating-point numbers).
// NUM_TYPE is the HDF5 numeric type to use for storage (e.g.
// H5T_NATIVE_DOUBLE to save as 'double').  Note that any necessary
// conversions are handled automatically by HDF5.

static hid_t
hdf5_make_range_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 3);

  H5Tinsert (type_id, "base", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "limit", 1 * sizeof (double), num_type);
  H5Tinsert (type_id, "increment", 2 * sizeof (double), num_type);

  return type_id;
}

#endif

bool
octave_legacy_range::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

#if defined (HAVE_HDF5_18)
  hid_t data_hid = H5Dopen (loc_id, name, octave_H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t range_type = hdf5_make_range_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, range_type))
    {
      H5Tclose (range_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Tclose (range_type);
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      return false;
    }

  double rangevals[3];
  if (H5Dread (data_hid, range_type, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, rangevals)
      >= 0)
    {
      retval = true;
      octave_idx_type nel;
      if (hdf5_get_scalar_attr (data_hid, H5T_NATIVE_IDX,
                                "OCTAVE_RANGE_NELEM", &nel))
        range = Range (rangevals[0], rangevals[2], nel);
      else
        {
          if (rangevals[2] != 0)
            range = Range (rangevals[0], rangevals[1], rangevals[2]);
          else
            range = Range (rangevals[0], rangevals[2],
                           static_cast<octave_idx_type> (rangevals[1]));
        }
    }

  H5Tclose (range_type);
  H5Sclose (space_hid);
  H5Dclose (data_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif
