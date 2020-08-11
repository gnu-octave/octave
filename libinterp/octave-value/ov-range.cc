////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
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

#include "dNDArray.h"
#include "fNDArray.h"
#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"
#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#include "lo-ieee.h"
#include "lo-utils.h"

#include "defun.h"
#include "variables.h"
#include "errwarn.h"
#include "mxarray.h"
#include "ops.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-utils.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_range, "range", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_range& v = dynamic_cast<const octave_range&> (a);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_range::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

octave_base_value *
octave_range::try_narrowing_conversion (void)
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
      break;
    }

  return retval;
}

octave_value
octave_range::subsref (const std::string& type,
                       const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

octave_value
octave_range::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  if (idx.length () == 1 && ! resize_ok)
    {
      octave_value retval;

      // The range can handle a single subscript.

      try
        {
          idx_vector i = idx(0).index_vector ();

          if (i.is_scalar () && i(0) < range.numel ())
            retval = range.elem (i(0));
          else
            retval = range.index (i);
        }
      catch (octave::index_exception& e)
        {
          // More info may be added later before displaying error.

          e.set_pos_if_unset (1, 1);
          throw;
        }

      return retval;
    }
  else
    {
      octave_value tmp (new octave_matrix (range.matrix_value ()));

      return tmp.do_index_op (idx, resize_ok);
    }
}

idx_vector
octave_range::index_vector (bool require_integers) const
{
  if (idx_cache)
    return *idx_cache;
  else
    {
      if (require_integers || range.all_elements_are_ints ())
        return set_idx_cache (idx_vector (range));
      else
        {
          warning_with_id ("Octave:noninteger-range-as-index",
                           "non-integer range used as index");

          return octave_value (matrix_value ()).round ().index_vector ();
        }
    }
}

double
octave_range::double_value (bool) const
{
  octave_idx_type nel = range.numel ();

  if (nel == 0)
    err_invalid_conversion ("range", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "range", "real scalar");

  return range.base ();
}

float
octave_range::float_value (bool) const
{
  octave_idx_type nel = range.numel ();

  if (nel == 0)
    err_invalid_conversion ("range", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "range", "real scalar");

  return range.base ();
}

charNDArray
octave_range::char_array_value (bool) const
{
  const Matrix matrix = range.matrix_value ();
  charNDArray retval (dims ());

  octave_idx_type nel = numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    retval.elem (i) = static_cast<char> (matrix.elem (i));

  return retval;
}

octave_value
octave_range::all (int dim) const
{
  // FIXME: this is a potential waste of memory.

  Matrix m = range.matrix_value ();

  return m.all (dim);
}

octave_value
octave_range::any (int dim) const
{
  // FIXME: this is a potential waste of memory.

  Matrix m = range.matrix_value ();

  return m.any (dim);
}

octave_value
octave_range::diag (octave_idx_type k) const
{
  return
    (k == 0
       ? octave_value (DiagMatrix (DiagArray2<double> (range.matrix_value ())))
       : octave_value (range.diag (k)));
}

octave_value
octave_range::diag (octave_idx_type m, octave_idx_type n) const
{
  Matrix mat = range.matrix_value ();

  return mat.diag (m, n);
}

// Return true if this range has all true elements (non-zero, not NaN/NA).
// A range cannot have NaN/NA.
bool
octave_range::is_true (void) const
{
  bool retval = false;

  if (! range.isempty ())
    {
      if (dims ().numel () > 1)
        warn_array_as_logical (dims ());

      Range r = range_value ();
      double base = r.base ();
      double limit = r.limit ();

      // Can't be zero if we start and finish on the same size of 0
      if (((base > 0 && limit > 0) || (base < 0 && limit < 0)) && numel () > 0)
        retval = true;
      else
        {
          /*
          // This tells us whether one element is 0, if arithmetic is exact.
          double steps_to_zero = base / r.inc ();

          retval = (steps_to_zero != floor (steps_to_zero));
          */

          // FIXME: this is a waste of memory.
          Matrix m ((range.matrix_value ().all ()).all ());

          retval = ! m.isempty () && m(0, 0) != 0.0;
        }
    }

  return retval;
}

Complex
octave_range::complex_value (bool) const
{
  octave_idx_type nel = range.numel ();

  if (nel == 0)
    err_invalid_conversion ("range", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "range", "complex scalar");

  return Complex (range.base (), 0);
}

FloatComplex
octave_range::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  octave_idx_type nel = range.numel ();

  if (nel == 0)
    err_invalid_conversion ("range", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "range", "complex scalar");

  retval = range.base ();

  return retval;
}

boolNDArray
octave_range::bool_array_value (bool warn) const
{
  Matrix m = range.matrix_value ();

  if (m.any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();
  if (warn && m.any_element_not_one_or_zero ())
    warn_logical_conversion ();

  return boolNDArray (m);
}

octave_value
octave_range::resize (const dim_vector& dv, bool fill) const
{
  NDArray retval = array_value ();
  if (fill)
    retval.resize (dv, 0);
  else
    retval.resize (dv);
  return retval;
}

octave_value
octave_range::convert_to_str_internal (bool pad, bool force, char type) const
{
  octave_value tmp (range.matrix_value ());
  return tmp.convert_to_str (pad, force, type);
}

octave_value
octave_range::as_double (void) const
{
  return range;
}

octave_value
octave_range::as_single (void) const
{
  return FloatMatrix (range.matrix_value ());
}

octave_value
octave_range::as_int8 (void) const
{
  return int8NDArray (range.matrix_value ());
}

octave_value
octave_range::as_int16 (void) const
{
  return int16NDArray (range.matrix_value ());
}

octave_value
octave_range::as_int32 (void) const
{
  return int32NDArray (range.matrix_value ());
}

octave_value
octave_range::as_int64 (void) const
{
  return int64NDArray (range.matrix_value ());
}

octave_value
octave_range::as_uint8 (void) const
{
  return uint8NDArray (range.matrix_value ());
}

octave_value
octave_range::as_uint16 (void) const
{
  return uint16NDArray (range.matrix_value ());
}

octave_value
octave_range::as_uint32 (void) const
{
  return uint32NDArray (range.matrix_value ());
}

octave_value
octave_range::as_uint64 (void) const
{
  return uint64NDArray (range.matrix_value ());
}

void
octave_range::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_range::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, range, pr_as_read_syntax,
                         current_print_indent_level ());
}

bool
octave_range::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  octave_idx_type n = range.numel ();

  indent (os);

  if (n == 0 || n == 1)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      if (! Vcompact_format)
        newline (os);

      retval = true;
    }

  return retval;
}

void
octave_range::short_disp (std::ostream& os) const
{
  octave_idx_type len = range.numel ();

  if (len == 0)
    os << "[]";
  else
    {
      os << range.base () << ':';

      if (len > 1)
        {
          if (range.inc () != 1)
            os << range.inc () << ':';

          os << range.limit ();
        }
    }
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

  skip_until_newline (is, false);
}

float_display_format
octave_range::get_edit_display_format (void) const
{
  return make_format (range_value ());
}

std::string
octave_range::edit_display (const float_display_format& fmt,
                            octave_idx_type, octave_idx_type j) const
{
  std::ostringstream buf;
  octave_print_internal (buf, fmt, range.elem (j));
  return buf.str ();
}

bool
octave_range::save_ascii (std::ostream& os)
{
  Range r = range_value ();
  double base = r.base ();
  double limit = r.limit ();
  double inc = r.inc ();
  octave_idx_type len = r.numel ();

  if (inc != 0)
    os << "# base, limit, increment\n";
  else
    os << "# base, length, increment\n";

  octave::write_value<double> (os, base);
  os << ' ';
  if (inc != 0)
    octave::write_value<double> (os, limit);
  else
    os << len;
  os << ' ';
  octave::write_value<double> (os, inc);
  os << "\n";

  return true;
}

bool
octave_range::load_ascii (std::istream& is)
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
octave_range::save_binary (std::ostream& os, bool /* save_as_floats */)
{
  char tmp = LS_DOUBLE;
  os.write (reinterpret_cast<char *> (&tmp), 1);
  Range r = range_value ();
  double bas = r.base ();
  double lim = r.limit ();
  double inc = r.inc ();
  if (inc == 0)
    lim = r.numel ();

  os.write (reinterpret_cast<char *> (&bas), 8);
  os.write (reinterpret_cast<char *> (&lim), 8);
  os.write (reinterpret_cast<char *> (&inc), 8);

  return true;
}

bool
octave_range::load_binary (std::istream& is, bool swap,
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
octave_range::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                         bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hsize_t dimens[3];
  hid_t space_hid, type_hid, data_hid;
  space_hid = type_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, nullptr);
  if (space_hid < 0) return false;

  type_hid = hdf5_make_range_type (H5T_NATIVE_DOUBLE);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }

  Range r = range_value ();
  double range_vals[3];
  range_vals[0] = r.base ();
  range_vals[1] = (r.inc () != 0 ? r.limit () : r.numel ());
  range_vals[2] = r.inc ();

  if (H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                octave_H5P_DEFAULT, range_vals)
      >= 0)
    {
      octave_idx_type nel = r.numel ();
      retval = hdf5_add_scalar_attr (data_hid, H5T_NATIVE_IDX,
                                     "OCTAVE_RANGE_NELEM", &nel) >= 0;
    }
  else
    retval = false;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");
#endif

  return retval;
}

bool
octave_range::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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

mxArray *
octave_range::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, dims (), mxREAL);

  mxDouble *pd = static_cast<mxDouble *> (retval->get_data ());

  mwSize nel = numel ();

  Matrix m = matrix_value ();

  const double *pdata = m.data ();

  for (mwSize i = 0; i < nel; i++)
    pd[i] = pdata[i];

  return retval;
}

octave_value
octave_range::fast_elem_extract (octave_idx_type n) const
{
  return (n < range.numel ()) ? octave_value (range.elem (n))
                              : octave_value ();
}
