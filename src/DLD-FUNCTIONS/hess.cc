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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CmplxHESS.h"
#include "dbleHESS.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (hess, args, nargout,
  "[P, H] = hess (A) or H = hess (A): Hessenberg decomposition")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 2)
    {
      print_usage ("hess");
      return retval;
    }

  octave_value arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("hess", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("hess");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix tmp = arg.matrix_value ();

      if (! error_state)
	{
	  HESS result (tmp);

	  if (nargout == 0 || nargout == 1)
	    {
	      retval.resize (1);
	      retval(0) = result.hess_matrix ();
	    }
	  else
	    {
	      retval.resize (2);
	      retval(0) = result.unitary_hess_matrix ();
	      retval(1) = result.hess_matrix ();
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix ctmp = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexHESS result (ctmp);

	  if (nargout == 0 || nargout == 1)
	    {
	      retval.resize (1);
	      retval(0) = result.hess_matrix ();
	    }
	  else
	    {
	      retval.resize (2);
	      retval(0) = result.unitary_hess_matrix ();
	      retval(1) = result.hess_matrix ();
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("hess", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
