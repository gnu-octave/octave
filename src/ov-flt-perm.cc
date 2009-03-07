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

#include "ov-flt-perm.h"
#include "ov-flt-re-mat.h"
#include "ov-float.h"
#include "ops.h"

DEFINE_OCTAVE_ALLOCATOR (octave_float_perm_matrix);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_float_perm_matrix, 
                                     "float permutation matrix", "single");

static octave_base_value *
default_float_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_float_perm_matrix&);

  return new octave_float_matrix (v.float_matrix_value ());
}

octave_base_value::type_conv_info
octave_float_perm_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_float_numeric_conversion_function,
                                            octave_float_matrix::static_type_id ());
}

octave_base_value::type_conv_info
octave_float_perm_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info (0);
}

octave_base_value *
octave_float_perm_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    retval = new octave_float_scalar (matrix (0, 0));

  return retval;
}

octave_value
octave_float_perm_matrix::to_dense (void) const
{
  if (! dense_cache.is_defined ())
      dense_cache = FloatMatrix (matrix);

  return dense_cache;
}

