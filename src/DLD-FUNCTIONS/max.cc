/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
              2005, 2006, 2007, 2008 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "dNDArray.h"
#include "CNDArray.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

#include "ov-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

#define MINMAX_DOUBLE_SBODY(FCN) \
{ \
  if (nargout == 1 || nargout == 0) \
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
  else if (nargout == 2) \
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
	retval(1) = NDArray (index, true, true);	\
      else \
	retval(1) = NDArray (); \
    } \
}

#define MINMAX_DOUBLE_BODY(FCN) \
{ \
  bool single_arg = (nargin == 1) || (arg2.is_empty() && nargin == 3);	\
  if (single_arg) \
    MINMAX_DOUBLE_SBODY (FCN) \
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
}

#define MINMAX_SINGLE_SBODY(FCN) \
{ \
  if (nargout == 1 || nargout == 0) \
    { \
      if (arg1.is_real_type ()) \
	{ \
	  FloatNDArray m = arg1.float_array_value (); \
 \
	  if (! error_state) \
	    { \
	      FloatNDArray n = m. FCN (dim); \
	      retval(0) = n; \
	    } \
	} \
      else if (arg1.is_complex_type ()) \
	{ \
	  FloatComplexNDArray m = arg1.float_complex_array_value (); \
 \
	  if (! error_state) \
	    { \
	      FloatComplexNDArray n = m. FCN (dim); \
	      retval(0) = n; \
	    } \
	} \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
    } \
  else if (nargout == 2) \
    { \
      ArrayN<octave_idx_type> index; \
 \
      if (arg1.is_real_type ()) \
	{ \
	  FloatNDArray m = arg1.float_array_value (); \
 \
	  if (! error_state) \
	    { \
	      FloatNDArray n = m. FCN (index, dim);	\
	      retval(0) = n; \
	    } \
	} \
      else if (arg1.is_complex_type ()) \
	{ \
	  FloatComplexNDArray m = arg1.float_complex_array_value (); \
 \
	  if (! error_state) \
	    { \
	      FloatComplexNDArray n = m. FCN (index, dim);	\
	      retval(0) = n; \
	    } \
	} \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
 \
      octave_idx_type len = index.numel (); \
 \
      if (len > 0) \
	retval(1) = NDArray (index, true, true);	\
      else \
	retval(1) = NDArray (); \
    } \
}

#define MINMAX_SINGLE_BODY(FCN) \
{ \
  bool single_arg = (nargin == 1) || (arg2.is_empty() && nargin == 3);	\
  if (single_arg) \
    MINMAX_SINGLE_SBODY(FCN) \
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
	      FloatComplex c1 = arg1.float_complex_value (); \
	      FloatComplexNDArray m2 = arg2.float_complex_array_value (); \
	      if (! error_state) \
		{ \
		  FloatComplexNDArray result = FCN (c1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      float d1 = arg1.float_value (); \
	      FloatNDArray m2 = arg2.float_array_value (); \
 \
	      if (! error_state) \
		{ \
		  FloatNDArray result = FCN (d1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else if (arg2_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      FloatComplexNDArray m1 = arg1.float_complex_array_value (); \
 \
	      if (! error_state) \
		{ \
		  FloatComplex c2 = arg2.float_complex_value (); \
		  FloatComplexNDArray result = FCN (m1, c2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      FloatNDArray m1 = arg1.float_array_value (); \
 \
	      if (! error_state) \
		{ \
		  float d2 = arg2.float_value (); \
		  FloatNDArray result = FCN (m1, d2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      FloatComplexNDArray m1 = arg1.float_complex_array_value (); \
 \
	      if (! error_state) \
		{ \
		  FloatComplexNDArray m2 = arg2.float_complex_array_value (); \
 \
		  if (! error_state) \
		    { \
		      FloatComplexNDArray result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	  else \
	    { \
	      FloatNDArray m1 = arg1.float_array_value (); \
 \
	      if (! error_state) \
		{ \
		  FloatNDArray m2 = arg2.float_array_value (); \
 \
		  if (! error_state) \
		    { \
		      FloatNDArray result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	} \
    } \
}

#define MINMAX_INT_SBODY(FCN, TYP) \
{ \
  if (nargout == 1 || nargout == 0) \
    { \
      TYP ## NDArray m = arg1. TYP ## _array_value (); \
 \
      if (! error_state) \
	{ \
	  TYP ## NDArray n = m. FCN (dim); \
	  retval(0) = n; \
	} \
    } \
  else if (nargout == 2) \
    { \
      ArrayN<octave_idx_type> index; \
 \
      TYP ## NDArray m = arg1. TYP ## _array_value (); \
 \
      if (! error_state) \
        { \
	  TYP ## NDArray n = m. FCN (index, dim);	\
	  retval(0) = n; \
	} \
 \
      octave_idx_type len = index.numel (); \
 \
      if (len > 0) \
	retval(1) = NDArray (index, true, true);	\
      else \
	retval(1) = NDArray (); \
    } \
}

#define MINMAX_INT_BODY(FCN, TYP) \
 { \
  bool single_arg = (nargin == 1) || (arg2.is_empty() && nargin == 3);	\
  if (single_arg) \
    MINMAX_INT_SBODY (FCN, TYP) \
  else \
    { \
      int arg1_is_scalar = arg1.is_scalar_type (); \
      int arg2_is_scalar = arg2.is_scalar_type (); \
 \
      if (arg1_is_scalar) \
	{ \
	  octave_ ## TYP d1 = arg1. TYP ## _scalar_value (); \
	  TYP ## NDArray m2 = arg2. TYP ## _array_value (); \
 \
	  if (! error_state) \
	    { \
	      TYP ## NDArray result = FCN (d1, m2); \
	      if (! error_state) \
		retval(0) = result; \
	    } \
	} \
      else if (arg2_is_scalar) \
	{ \
	  TYP ## NDArray m1 = arg1. TYP ## _array_value (); \
 \
	  if (! error_state) \
	    { \
	      octave_ ## TYP d2 = arg2. TYP ## _scalar_value (); \
	      TYP ## NDArray result = FCN (m1, d2); \
	      if (! error_state) \
		retval(0) = result; \
	    } \
	} \
      else \
	{ \
	  TYP ## NDArray m1 = arg1. TYP ## _array_value (); \
 \
	  if (! error_state) \
	    { \
	      TYP ## NDArray m2 = arg2. TYP ## _array_value (); \
 \
	      if (! error_state) \
		{ \
		  TYP ## NDArray result = FCN (m1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
    } \
}

#define MINMAX_SPARSE_BODY(FCN) \
{ \
  bool single_arg = (nargin == 1) || arg2.is_empty();	\
 \
  if (single_arg && (nargout == 1 || nargout == 0)) \
    { \
      if (arg1.type_id () == octave_sparse_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_matrix_value () .FCN (dim); \
      else if (arg1.type_id () == \
	       octave_sparse_complex_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_complex_matrix_value () .FCN (dim); \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
    } \
  else if (single_arg && nargout == 2) \
    { \
      Array2<octave_idx_type> index; \
 \
      if (arg1.type_id () == octave_sparse_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_matrix_value () .FCN (index, dim); \
      else if (arg1.type_id () == \
	       octave_sparse_complex_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_complex_matrix_value () .FCN (index, dim); \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
 \
      octave_idx_type len = index.numel (); \
 \
      if (len > 0) \
	retval(1) = NDArray (index, true, true);	\
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
	      \
	      SparseComplexMatrix m2 = arg2.sparse_complex_matrix_value (); \
	      \
	      if (! error_state) \
		{ \
		  SparseComplexMatrix result = FCN (c1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      double d1 = arg1.double_value (); \
	      SparseMatrix m2 = arg2.sparse_matrix_value (); \
	      \
	      if (! error_state) \
		{ \
		  SparseMatrix result = FCN (d1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else if (arg2_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      SparseComplexMatrix m1 = arg1.sparse_complex_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  Complex c2 = arg2.complex_value (); \
		  SparseComplexMatrix result = FCN (m1, c2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      SparseMatrix m1 = arg1.sparse_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  double d2 = arg2.double_value (); \
		  SparseMatrix result = FCN (m1, d2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      SparseComplexMatrix m1 = arg1.sparse_complex_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  SparseComplexMatrix m2 = arg2.sparse_complex_matrix_value (); \
 \
		  if (! error_state) \
		    { \
		      SparseComplexMatrix result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	  else \
	    { \
	      SparseMatrix m1 = arg1.sparse_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  SparseMatrix m2 = arg2.sparse_matrix_value (); \
 \
		  if (! error_state) \
		    { \
		      SparseMatrix result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	} \
    } \
}


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
  if (arg1.is_integer_type ()) \
    { \
      if (arg1.is_uint8_type ()) \
        MINMAX_INT_BODY (FCN, uint8) \
      else if (arg1.is_uint16_type ()) \
        MINMAX_INT_BODY (FCN, uint16) \
      else if (arg1.is_uint32_type ()) \
        MINMAX_INT_BODY (FCN, uint32) \
      else if (arg1.is_uint64_type ()) \
        MINMAX_INT_BODY (FCN, uint64) \
      else if (arg1.is_int8_type ()) \
        MINMAX_INT_BODY (FCN, int8) \
      else if (arg1.is_int16_type ()) \
        MINMAX_INT_BODY (FCN, int16) \
      else if (arg1.is_int32_type ()) \
        MINMAX_INT_BODY (FCN, int32) \
      else if (arg1.is_int64_type ()) \
        MINMAX_INT_BODY (FCN, int64) \
    } \
  else if (arg1.is_sparse_type ()) \
    MINMAX_SPARSE_BODY (FCN) \
  else if (arg1.is_single_type ()) \
    MINMAX_SINGLE_BODY (FCN) \
  else \
    MINMAX_DOUBLE_BODY (FCN) \
 \
 return retval;

DEFUN_DLD (min, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} min (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{w}, @var{iw}] =} min (@var{x})\n\
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

/*

%% test/octave.test/arith/min-1.m
%!assert (min ([1, 4, 2, 3]) == 1);
%!assert (min ([1; -10; 5; -2]) == -10);

%% test/octave.test/arith/min-2.m
%!assert(all (min ([4, i; -2, 2]) == [-2, i]));

%% test/octave.test/arith/min-3.m
%!error <Invalid call to min.*> min ();

%% test/octave.test/arith/min-4.m
%!error <Invalid call to min.*> min (1, 2, 3, 4);

%!test
%! x = reshape (1:8,[2,2,2]);
%! assert (max (x,[],1), reshape ([2, 4, 6, 8], [1,2,2]));
%! assert (max (x,[],2), reshape ([3, 4, 7, 8], [2,1,2]));
%! [y, i ] = max (x, [], 3);
%! assert (y, [5, 7; 6, 8]);
%! assert (ndims(y), 2);
%! assert (i, [2, 2; 2, 2]);
%! assert (ndims(i), 2);

*/

DEFUN_DLD (max, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} max (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{w}, @var{iw}] =} max (@var{x})\n\
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

%% test/octave.test/arith/max-1.m
%!assert (max ([1, 4, 2, 3]) == 4);
%!assert (max ([1; -10; 5; -2]) == 5);
 
%% test/octave.test/arith/max-2.m
%!assert(all (max ([4, i 4.999; -2, 2, 3+4i]) == [4, 2, 3+4i]));

%% test/octave.test/arith/max-3.m
%!error <Invalid call to max.*> max ();

%% test/octave.test/arith/max-4.m
%!error <Invalid call to max.*> max (1, 2, 3, 4);

%!test
%! x = reshape (1:8,[2,2,2]);
%! assert (min (x,[],1), reshape ([1, 3, 5, 7], [1,2,2]));
%! assert (min (x,[],2), reshape ([1, 2, 5, 6], [2,1,2]));
%! [y, i ] = min (x, [], 3);
%! assert (y, [1, 3; 2, 4]);
%! assert (ndims(y), 2);
%! assert (i, [1, 1; 1, 1]);
%! assert (ndims(i), 2);


*/

#define CUMMINMAX_BODY(FCN) \
 \
  octave_value_list retval;  \
 \
  int nargin = args.length (); \
 \
  if (nargin < 1 || nargin > 2 || nargout > 2) \
    { \
      print_usage (); \
      return retval; \
    } \
 \
  octave_value arg1; \
  octave_value arg2; \
 \
  switch (nargin) \
    { \
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
  if (nargin == 2) \
    { \
      dim = arg2.nint_value () - 1;  \
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
  if (arg1.is_integer_type ()) \
    { \
      if (arg1.is_uint8_type ()) \
        MINMAX_INT_SBODY (FCN, uint8) \
      else if (arg1.is_uint16_type ()) \
        MINMAX_INT_SBODY (FCN, uint16) \
      else if (arg1.is_uint32_type ()) \
        MINMAX_INT_SBODY (FCN, uint32) \
      else if (arg1.is_uint64_type ()) \
        MINMAX_INT_SBODY (FCN, uint64) \
      else if (arg1.is_int8_type ()) \
        MINMAX_INT_SBODY (FCN, int8) \
      else if (arg1.is_int16_type ()) \
        MINMAX_INT_SBODY (FCN, int16) \
      else if (arg1.is_int32_type ()) \
        MINMAX_INT_SBODY (FCN, int32) \
      else if (arg1.is_int64_type ()) \
        MINMAX_INT_SBODY (FCN, int64) \
    } \
  else if (arg1.is_single_type ()) \
    MINMAX_SINGLE_SBODY (FCN) \
  else \
    MINMAX_DOUBLE_SBODY (FCN) \
 \
 return retval;

DEFUN_DLD (cummin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} cummin (@var{x}, @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{w}, @var{iw}] =} cummin (@var{x})\n\
@cindex Utility Functions\n\
Return the cumulative minimum values. That means, the call\n\
@example\n\
  [w, iw] = cummin (x, dim)\n\
@end example\n\
\n\
@noindent\n\
is equivalent to the following code:\n\
@example\n\
  w = iw = zeros (size (x));\n\
  idxw = idxx = repmat (@{':'@}, 1, ndims (x));\n\
  for i = 1:size (x, dim)\n\
    idxw@{dim@} = i; idxx@{dim@} = 1:i;\n\
    [w(idxw@{:@}), iw(idxw@{:@})] =\
 min(x(idxx@{:@}), [], dim);\n\
  endfor\n\
@end example\n\
\n\
@noindent\n\
but computed in a much faster manner.\n\
The behaviour if @var{dim} or @var{iw} is unspecified is analogous\n\
to @code{min}.\n\
@end deftypefn")
{
  CUMMINMAX_BODY (cummin);
}

DEFUN_DLD (cummax, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} cummax (@var{x}, @var{dim})\n\
@deftypefnx {Loadable Function} {[@var{w}, @var{iw}] =} cummax (@var{x})\n\
@cindex Utility Functions\n\
Return the cumulative maximum values. That means, the call\n\
@example\n\
  [w, iw] = cummax (x, dim)\n\
@end example\n\
\n\
@noindent\n\
is equivalent to the following code:\n\
@example\n\
  w = iw = zeros (size (x));\n\
  idxw = idxx = repmat (@{':'@}, 1, ndims (x));\n\
  for i = 1:size (x, dim)\n\
    idxw@{dim@} = i; idxx@{dim@} = 1:i;\n\
    [w(idxw@{:@}), iw(idxw@{:@})] =\
 max(x(idxx@{:@}), [], dim);\n\
  endfor\n\
@end example\n\
\n\
@noindent\n\
but computed in a much faster manner.\n\
The behaviour if @var{dim} or @var{iw} is unspecified is analogous\n\
to @code{max}.\n\
@end deftypefn")
{
  CUMMINMAX_BODY (cummax);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
