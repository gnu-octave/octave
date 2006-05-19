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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (inv, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{rcond}] = } inv (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{rcond}] = } inverse (@var{a})\n\
Compute the inverse of the square matrix @var{a}.  Return an estimate\n\
of the reciprocal condition number if requested, otherwise warn of an\n\
ill-conditioned matrix if the reciprocal condition number is small.\n\
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

  int arg_is_empty = empty_arg ("inverse", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("inverse");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  double rcond = 0.0;

	  Matrix result = m.inverse (info, rcond, 1);

	  if (nargout > 1)
	    retval(1) = rcond;

	  retval(0) = result;

	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  if (nargout < 2 && (info == -1 || xrcond == 1.0))
	    warning ("inverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  double rcond = 0.0;

	  ComplexMatrix result = m.inverse (info, rcond, 1);

	  if (nargout > 1)
	    retval(1) = rcond;

	  retval(0) = result;

	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  if (nargout < 2 && (info == -1 || xrcond == 1.0))
	    warning ("inverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	}
    }
  else
    {
      gripe_wrong_type_arg ("inv", arg);
    }

  return retval;
}

// FIXME -- this should really be done with an alias, but
// alias_builtin() won't do the right thing if we are actually using
// dynamic linking.

DEFUN_DLD (inverse, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} inverse (@var{a})\n\
See inv.\n\
@end deftypefn")
{
  return Finv (args, nargout);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
