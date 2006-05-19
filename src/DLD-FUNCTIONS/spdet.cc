/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "dbleDET.h"
#include "CmplxDET.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// PKG_ADD: dispatch ("det", "spdet", "sparse matrix");
// PKG_ADD: dispatch ("det", "spdet", "sparse complex matrix");
// PKG_ADD: dispatch ("det", "spdet", "sparse bool matrix");
DEFUN_DLD (spdet, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{d}, @var{rcond}] = } spdet (@var{a})\n\
Compute the determinant of sparse matrix @var{a} using UMFPACK.  Return\n\
an estimate of the reciprocal condition number if requested.\n\
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

  int arg_is_empty = empty_arg ("spdet", nr, nc);
  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix (1, 1, 1.0));

  if (nr != nc)
    {
      gripe_square_matrix_required ("spdet");
      return retval;
    }

  if (arg.is_real_type ())
    {
      SparseMatrix m = args(0).sparse_matrix_value ();

      if (! error_state)
	{
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.

	  octave_idx_type info;
	  double rcond = 0.0;
	  DET det = m.determinant (info, rcond);
	  retval(1) = rcond;
	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  retval(0) = ((info == -1 || xrcond == 1.0) ? 0.0 : det.value ());
	}
    }
  else if (arg.is_complex_type ())
    {
      SparseComplexMatrix m = args(0).sparse_complex_matrix_value ();

      if (! error_state)
	{
	  // Always compute rcond, so we can detect numerically
	  // singular matrices.

	  octave_idx_type info;
	  double rcond = 0.0;
	  ComplexDET det = m.determinant (info, rcond);
	  retval(1) = rcond;
	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  retval(0) = ((info == -1 || xrcond == 1.0)
		       ? Complex (0.0) : det.value ());
	}
    }
  else
    {
      gripe_wrong_type_arg ("spdet", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
