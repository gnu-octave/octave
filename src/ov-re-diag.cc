/*

Copyright (C) 2008 Jaroslav Hajek

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

#include "byte-swap.h"

#include "ov-re-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-base-diag.cc"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ls-utils.h"

template class octave_base_diag<DiagMatrix, Matrix>;

DEFINE_OCTAVE_ALLOCATOR (octave_diag_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_diag_matrix, "diagonal matrix", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_diag_matrix&);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_diag_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_diag_matrix&);

  return new octave_float_diag_matrix (v.float_diag_matrix_value ());
}

octave_base_value::type_conv_info
octave_diag_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_demotion_function,
                                            octave_float_diag_matrix::static_type_id ());
}

octave_base_value *
octave_diag_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  // FIXME: the proxy mechanism of DiagArray2 causes problems here.
  if (matrix.nelem () == 1)
    retval = new octave_scalar (double (matrix (0, 0)));

  return retval;
}

DiagMatrix
octave_diag_matrix::diag_matrix_value (bool) const
{
  return matrix;
}

FloatDiagMatrix
octave_diag_matrix::float_diag_matrix_value (bool) const
{
  return FloatDiagMatrix (matrix);
}

ComplexDiagMatrix
octave_diag_matrix::complex_diag_matrix_value (bool) const
{
  return ComplexDiagMatrix (matrix);
}

FloatComplexDiagMatrix
octave_diag_matrix::float_complex_diag_matrix_value (bool) const
{
  return FloatComplexDiagMatrix (matrix);
}

octave_value
octave_diag_matrix::abs (void) const
{
  return matrix.abs ();
}

octave_value
octave_diag_matrix::real (void) const
{
  return matrix;
}

octave_value
octave_diag_matrix::conj (void) const
{
  return matrix;
}

octave_value
octave_diag_matrix::imag (void) const
{
  return DiagMatrix (matrix.rows (), matrix.cols (), 0.0);
}

bool 
octave_diag_matrix::save_binary (std::ostream& os, bool& save_as_floats)
{

  int32_t r = matrix.rows (), c = matrix.cols ();
  os.write (reinterpret_cast<char *> (&r), 4);
  os.write (reinterpret_cast<char *> (&c), 4);

  Matrix m = Matrix (matrix.diag ());
  save_type st = LS_DOUBLE;
  if (save_as_floats)
    {
      if (m.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	st = LS_FLOAT;
    }
  else if (matrix.length () > 8192) // FIXME -- make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
	st = get_save_type (max_val, min_val);
    }

  const double *mtmp = m.data ();
  write_doubles (os, mtmp, st, m.numel ());

  return true;
}

bool 
octave_diag_matrix::load_binary (std::istream& is, bool swap,
				 oct_mach_info::float_format fmt)
{
  int32_t r, c;
  char tmp;
  if (! (is.read (reinterpret_cast<char *> (&r), 4)
         && is.read (reinterpret_cast<char *> (&c), 4)
         && is.read (reinterpret_cast<char *> (&tmp), 1)))
    return false;
  if (swap)
    {
      swap_bytes<4> (&r);
      swap_bytes<4> (&c);
    }

  DiagMatrix m (r, c);
  double *re = m.fortran_vec ();
  octave_idx_type len = m.length ();
  read_doubles (is, re, static_cast<save_type> (tmp), len, swap, fmt);
  if (error_state || ! is)
    return false;
  matrix = m;

  return true;
}

bool 
octave_diag_matrix::chk_valid_scalar (const octave_value& val, 
                                      double& x) const
{
  bool retval = val.is_real_scalar ();
  if (retval)
    x = val.double_value ();
  return retval;
}
