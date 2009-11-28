/*

Copyright (C) 1996, 1997, 1998, 2000, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"
#include "lo-utils.h"

#include "gripes.h"
#include "ops.h"
#include "oct-obj.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_range);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_range, "range", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_range&);

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
  octave_base_value *retval = 0;

  switch (range.nelem ())
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
  // FIXME -- this doesn't solve the problem of
  //
  //   a = 1:5; a(1, 1, 1)
  //
  // and similar constructions.  Hmm...

  // FIXME -- using this constructor avoids possibly narrowing
  // the range to a scalar value.  Need a better solution to this
  // problem.

  octave_value tmp (new octave_matrix (range.matrix_value ()));

  return tmp.do_index_op (idx, resize_ok);
}

double
octave_range::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  octave_idx_type nel = range.nelem ();

  if (nel > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "range", "real scalar");

      retval = range.base ();
    }
  else
    gripe_invalid_conversion ("range", "real scalar");

  return retval;
}

float
octave_range::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  octave_idx_type nel = range.nelem ();

  if (nel > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "range", "real scalar");

      retval = range.base ();
    }
  else
    gripe_invalid_conversion ("range", "real scalar");

  return retval;
}

charNDArray
octave_range::char_array_value (bool) const
{
  const Matrix matrix = range.matrix_value ();
  charNDArray retval (dims ());

  octave_idx_type nel = numel ();
  
  for (octave_idx_type i = 0; i < nel; i++)
    retval.elem (i) = static_cast<char>(matrix.elem (i));

  return retval;
}
  
octave_value
octave_range::all (int dim) const
{
  // FIXME -- this is a potential waste of memory.

  Matrix m = range.matrix_value ();

  return m.all (dim);
}

octave_value
octave_range::any (int dim) const
{
  // FIXME -- this is a potential waste of memory.

  Matrix m = range.matrix_value ();

  return m.any (dim);
}

octave_value 
octave_range::diag (octave_idx_type k) const
{ 
  return (k == 0
          ? octave_value (DiagMatrix (DiagArray2<double> (range.matrix_value ())))
          : octave_value (range.diag (k))); 
}


bool
octave_range::is_true (void) const
{
  bool retval = false;

  if (range.nelem () != 0)
    {
      // FIXME -- this is a potential waste of memory.

      Matrix m ((range.matrix_value () . all ()) . all ());

      retval = (m.rows () == 1 && m.columns () == 1 && m (0, 0) != 0.0);
    }

  return retval;
}

Complex
octave_range::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  octave_idx_type nel = range.nelem ();

  if (nel > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "range", "complex scalar");

      retval = range.base ();
    }
  else
    gripe_invalid_conversion ("range", "complex scalar");

  return retval;
}

FloatComplex
octave_range::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  octave_idx_type nel = range.nelem ();

  if (nel > 0)
    {
      gripe_implicit_conversion ("Octave:array-as-scalar",
				 "range", "complex scalar");

      retval = range.base ();
    }
  else
    gripe_invalid_conversion ("range", "complex scalar");

  return retval;
}

octave_value 
octave_range::resize (const dim_vector& dv, bool fill) const
{ 
  NDArray retval = array_value (); 
  if (fill)
    retval.resize (dv, NDArray::resize_fill_value());
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

void
octave_range::print (std::ostream& os, bool pr_as_read_syntax) const
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

  octave_idx_type n = range.nelem ();

  indent (os);

  if (n == 0 || n == 1)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      newline (os);
      retval = true;
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

  skip_until_newline (is, false);
}

bool 
octave_range::save_ascii (std::ostream& os)
{
  Range r = range_value ();
  double base = r.base ();
  double limit = r.limit ();
  double inc = r.inc ();

  os << "# base, limit, increment\n";
  octave_write_double (os, base);
  os << " ";
  octave_write_double (os, limit);
  os << " ";
  octave_write_double (os, inc);
  os << "\n";

  return true;
}

bool 
octave_range::load_ascii (std::istream& is)
{
  // # base, limit, range comment added by save ().
  skip_comments (is);

  is >> range;

  if (!is)
    {
      error ("load: failed to load range constant");
      return false;
    }

  return true;
}

bool 
octave_range::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = LS_DOUBLE;
  os.write (reinterpret_cast<char *> (&tmp), 1);
  Range r = range_value ();
  double bas = r.base ();
  double lim = r.limit ();
  double inc = r.inc ();
  os.write (reinterpret_cast<char *> (&bas), 8);
  os.write (reinterpret_cast<char *> (&lim), 8);
  os.write (reinterpret_cast<char *> (&inc), 8);

  return true;
}

bool 
octave_range::load_binary (std::istream& is, bool swap,
			   oct_mach_info::float_format /* fmt */)
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
  Range r (bas, lim, inc);
  range = r;
  return true;
}

#if defined (HAVE_HDF5)

// The following subroutines creates an HDF5 representation of the way
// we will store Octave range types (triplets of floating-point numbers). 
// NUM_TYPE is the HDF5 numeric type to use for storage (e.g. 
// H5T_NATIVE_DOUBLE to save as 'double'). Note that any necessary 
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

bool
octave_range::save_hdf5 (hid_t loc_id, const char *name,
			 bool /* save_as_floats */)
{
  hsize_t dimens[3];
  hid_t space_hid = -1, type_hid = -1, data_hid = -1;
  bool retval = true;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0) return false;

  type_hid = hdf5_make_range_type (H5T_NATIVE_DOUBLE);
  if (type_hid < 0) 
    {
      H5Sclose (space_hid);
      return false;
    }

  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }
  
  Range r = range_value ();
  double range_vals[3];
  range_vals[0] = r.base ();
  range_vals[1] = r.limit ();
  range_vals[2] = r.inc ();

  retval = H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		     range_vals) >= 0;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

  return retval;
}

bool 
octave_range::load_hdf5 (hid_t loc_id, const char *name)
{
  bool retval = false;

  hid_t data_hid = H5Dopen (loc_id, name);
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
  if (H5Dread (data_hid, range_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
	       rangevals) >= 0)
    {
      retval = true;
      Range r (rangevals[0], rangevals[1], rangevals[2]);
      range = r;
    }

  H5Tclose (range_type);
  H5Sclose (space_hid);
  H5Dclose (data_hid);

  return retval;
}

#endif

mxArray *
octave_range::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxDOUBLE_CLASS, dims (), mxREAL);

  double *pr = static_cast<double *> (retval->get_data ());

  mwSize nel = numel ();

  Matrix m = matrix_value ();

  const double *p = m.data ();

  for (mwSize i = 0; i < nel; i++)
    pr[i] = p[i];

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
