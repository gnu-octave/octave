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

#include "CmplxDET.h"
#include "dbleDET.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (det, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{d}, @var{rcond}] = } det (@var{a})\n\
Compute the determinant of @var{a} using @sc{Lapack}.  Return an estimate\n\
of the reciprocal condition number if requested.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("det");
      return retval;
    }

  octave_value arg = args(0);
    
  int nr = arg.rows ();
  int nc = arg.columns ();

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

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

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
      ComplexMatrix m = arg.complex_matrix_value ();

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
    gripe_wrong_type_arg ("det", arg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
