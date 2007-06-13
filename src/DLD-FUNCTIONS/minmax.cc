/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cmath>

#include "lo-ieee.h"
#include "lo-mappers.h"
#include "dNDArray.h"
#include "CNDArray.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

#include "ov-cx-mat.h"

#define MINMAX_BODY(FCN) \
 \
  octave_value_list retval;  \
 \
  int nargin = args.length (); \
 \
  if (nargin < 1 || nargin > 3 || nargout > 2) \
    { \
      print_usage (); \
      return retval; \
    } \
 \
  octave_value arg1; \
  octave_value arg2; \
  octave_value arg3; \
 \
  switch (nargin) \
    { \
    case 3: \
      arg3 = args(2); \
 \
    case 2: \
      arg2 = args(1); \
 \
    case 1: \
      arg1 = args(0); \
      break; \
 \
    default: \
      panic_impossible (); \
      break; \
    } \
 \
  int dim; \
  dim_vector dv = arg1.dims (); \
  if (error_state) \
    { \
      gripe_wrong_type_arg (#FCN, arg1);  \
      return retval; \
    } \
 \
  if (nargin == 3) \
    { \
      dim = arg3.nint_value () - 1;  \
      if (dim < 0 || dim >= dv.length ()) \
        { \
	  error ("%s: invalid dimension", #FCN); \
	  return retval; \
	} \
    } \
  else \
    { \
      dim = 0; \
      while ((dim < dv.length ()) && (dv (dim) <= 1)) \
	dim++; \
      if (dim == dv.length ()) \
	dim = 0; \
    } \
 \
  bool single_arg = (nargin == 1) || (arg2.is_empty() && nargin == 3);	\
 \
  if (single_arg && (nargout == 1 || nargout == 0)) \
    { \
      if (arg1.is_real_type ()) \
	{ \
	  NDArray m = arg1.array_value (); \
 \
	  if (! error_state) \
	    { \
	      NDArray n = m. FCN (dim); \
	      retval(0) = n; \
	    } \
	} \
      else if (arg1.is_complex_type ()) \
	{ \
	  ComplexNDArray m = arg1.complex_array_value (); \
 \
	  if (! error_state) \
	    { \
	      ComplexNDArray n = m. FCN (dim); \
	      retval(0) = n; \
	    } \
	} \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
    } \
  else if (single_arg && nargout == 2) \
    { \
      ArrayN<octave_idx_type> index; \
 \
      if (arg1.is_real_type ()) \
	{ \
	  NDArray m = arg1.array_value (); \
 \
	  if (! error_state) \
	    { \
	      NDArray n = m. FCN (index, dim);	\
	      retval(0) = n; \
	    } \
	} \
      else if (arg1.is_complex_type ()) \
	{ \
	  ComplexNDArray m = arg1.complex_array_value (); \
 \
	  if (! error_state) \
	    { \
	      ComplexNDArray n = m. FCN (index, dim);	\
	      retval(0) = n; \
	    } \
	} \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
 \
      octave_idx_type len = index.numel (); \
 \
      if (len > 0) \
	{ \
	  double nan_val = lo_ieee_nan_value (); \
 \
	  NDArray idx (index.dims ()); \
 \
	  for (octave_idx_type i = 0; i < len; i++) \
	    { \
	      OCTAVE_QUIT; \
	      int tmp = index.elem (i) + 1; \
	      idx.elem (i) = (tmp <= 0) \
		? nan_val : static_cast<double> (tmp); \
	    } \
 \
	  retval(1) = idx; \
	} \
      else \
	retval(1) = NDArray (); \
    } \
  else \
    { \
      int arg1_is_scalar = arg1.is_scalar_type (); \
      int arg2_is_scalar = arg2.is_scalar_type (); \
 \
      int arg1_is_complex = arg1.is_complex_type (); \
      int arg2_is_complex = arg2.is_complex_type (); \
 \
      if (arg1_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      Complex c1 = arg1.complex_value (); \
	      ComplexNDArray m2 = arg2.complex_array_value (); \
	      if (! error_state) \
		{ \
		  ComplexNDArray result = FCN (c1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      double d1 = arg1.double_value (); \
	      NDArray m2 = arg2.array_value (); \
 \
	      if (! error_state) \
		{ \
		  NDArray result = FCN (d1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else if (arg2_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      ComplexNDArray m1 = arg1.complex_array_value (); \
 \
	      if (! error_state) \
		{ \
		  Complex c2 = arg2.complex_value (); \
		  ComplexNDArray result = FCN (m1, c2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      NDArray m1 = arg1.array_value (); \
 \
	      if (! error_state) \
		{ \
		  double d2 = arg2.double_value (); \
		  NDArray result = FCN (m1, d2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      ComplexNDArray m1 = arg1.complex_array_value (); \
 \
	      if (! error_state) \
		{ \
		  ComplexNDArray m2 = arg2.complex_array_value (); \
 \
		  if (! error_state) \
		    { \
		      ComplexNDArray result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	  else \
	    { \
	      NDArray m1 = arg1.array_value (); \
 \
	      if (! error_state) \
		{ \
		  NDArray m2 = arg2.array_value (); \
 \
		  if (! error_state) \
		    { \
		      NDArray result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	} \
    } \
 \
  return retval

DEFUN_DLD (min, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} min (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Mapping Function} {[@var{w}, @var{iw}] =} min (@var{x})\n\
@cindex Utility Functions\n\
For a vector argument, return the minimum value.  For a matrix\n\
argument, return the minimum value from each column, as a row\n\
vector, or over the dimension @var{dim} if defined. For two matrices\n\
(or a matrix and scalar), return the pair-wise minimum.\n\
Thus,\n\
\n\
@example\n\
min (min (@var{x}))\n\
@end example\n\
\n\
@noindent\n\
returns the smallest element of @var{x}, and\n\
\n\
@example\n\
@group\n\
min (2:5, pi)\n\
    @result{}  2.0000  3.0000  3.1416  3.1416\n\
@end group\n\
@end example\n\
@noindent\n\
compares each element of the range @code{2:5} with @code{pi}, and\n\
returns a row vector of the minimum values.\n\
\n\
For complex arguments, the magnitude of the elements are used for\n\
comparison.\n\
\n\
If called with one input and two output arguments,\n\
@code{min} also returns the first index of the\n\
minimum value(s). Thus,\n\
\n\
@example\n\
@group\n\
[x, ix] = min ([1, 3, 0, 2, 5])\n\
    @result{}  x = 0\n\
        ix = 3\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  MINMAX_BODY (min);
}

DEFUN_DLD (max, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} max (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Mapping Function} {[@var{w}, @var{iw}] =} max (@var{x})\n\
@cindex Utility Functions\n\
For a vector argument, return the maximum value.  For a matrix\n\
argument, return the maximum value from each column, as a row\n\
vector, or over the dimension @var{dim} if defined. For two matrices\n\
(or a matrix and scalar), return the pair-wise maximum.\n\
Thus,\n\
\n\
@example\n\
max (max (@var{x}))\n\
@end example\n\
\n\
@noindent\n\
returns the largest element of @var{x}, and\n\
\n\
@example\n\
@group\n\
max (2:5, pi)\n\
    @result{}  3.1416  3.1416  4.0000  5.0000\n\
@end group\n\
@end example\n\
@noindent\n\
compares each element of the range @code{2:5} with @code{pi}, and\n\
returns a row vector of the maximum values.\n\
\n\
For complex arguments, the magnitude of the elements are used for\n\
comparison.\n\
\n\
If called with one input and two output arguments,\n\
@code{max} also returns the first index of the\n\
maximum value(s). Thus,\n\
\n\
@example\n\
@group\n\
[x, ix] = max ([1, 3, 5, 2, 5])\n\
    @result{}  x = 5\n\
        ix = 3\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  MINMAX_BODY (max);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
