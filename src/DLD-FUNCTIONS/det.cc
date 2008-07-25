/*

Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

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

#include "CmplxDET.h"
#include "dbleDET.h"
#include "fCmplxDET.h"
#include "floatDET.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (det, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{d}, @var{rcond}] =} det (@var{a})\n\
Compute the determinant of @var{a} using @sc{Lapack} for full and UMFPACK\n\
for sparse matrices.  Return an estimate of the reciprocal condition number\n\
if requested.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);
    
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr == 0 && nc == 0)
    {
      retval(0) = 1.0;
      return retval;
    }

  int arg_is_empty = empty_arg ("det", nr, nc);
  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix (1, 1, 1.0));

  if (nr != nc)
    {
      gripe_square_matrix_required ("det");
      return retval;
    }


  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
	{
	  octave_idx_type info;
	  float rcond = 0.0;
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.
	  FloatMatrix m = arg.float_matrix_value ();
	  if (! error_state)
	    {
	      FloatDET det = m.determinant (info, rcond);
	      retval(1) = rcond;
	      retval(0) = info == -1 ? static_cast<float>(0.0) : det.value ();
	    }
	}
      else if (arg.is_complex_type ())
	{
	  octave_idx_type info;
	  float rcond = 0.0;
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.
	  FloatComplexMatrix m = arg.float_complex_matrix_value ();
	  if (! error_state)
	    {
	      FloatComplexDET det = m.determinant (info, rcond);
	      retval(1) = rcond;
	      retval(0) = info == -1 ? FloatComplex (0.0) : det.value ();
	    }
	}
    }
  else
    {
      if (arg.is_real_type ())
	{
	  octave_idx_type info;
	  double rcond = 0.0;
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.
	  if (arg.is_sparse_type ())
	    {
	      SparseMatrix m = arg.sparse_matrix_value ();
	      if (! error_state)
		{
		  DET det = m.determinant (info, rcond);
		  retval(1) = rcond;
		  retval(0) = info == -1 ? 0.0 : det.value ();
		}
	    }
	  else
	    {
	      Matrix m = arg.matrix_value ();
	      if (! error_state)
		{
		  DET det = m.determinant (info, rcond);
		  retval(1) = rcond;
		  retval(0) = info == -1 ? 0.0 : det.value ();
		}
	    }
	}
      else if (arg.is_complex_type ())
	{
	  octave_idx_type info;
	  double rcond = 0.0;
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.
	  if (arg.is_sparse_type ())
	    {
	      SparseComplexMatrix m = arg.sparse_complex_matrix_value ();
	      if (! error_state)
		{
		  ComplexDET det = m.determinant (info, rcond);
		  retval(1) = rcond;
		  retval(0) = info == -1 ? Complex (0.0) : det.value ();
		}
	    }
	  else
	    {
	      ComplexMatrix m = arg.complex_matrix_value ();
	      if (! error_state)
		{
		  ComplexDET det = m.determinant (info, rcond);
		  retval(1) = rcond;
		  retval(0) = info == -1 ? Complex (0.0) : det.value ();
		}
	    }
	}
      else
	gripe_wrong_type_arg ("det", arg);
    }
  return retval;
}

/*

%!assert(det ([1, 2; 3, 4]), -2, 10 * eps);
%!assert(det (single([1, 2; 3, 4])), single(-2), 10 * eps ('single'));
%!error <Invalid call to det.*> det ();
%!error <Invalid call to det.*> det (1, 2);
%!error det ([1, 2; 3, 4; 5, 6]);

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
