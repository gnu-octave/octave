/*

Copyright (C) 1996 John W. Eaton

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

#include "CmplxLU.h"
#include "dbleLU.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (lu, args, nargout,
  "[L, U, P] = lu (A): LU factorization")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ("lu");
      return retval;
    }

  octave_value arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("lu", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (3, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("lu");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  LU fact (m);

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		Matrix P = fact.P ();
		Matrix L = P.transpose () * fact.L ();
		retval(1) = fact.U ();
		retval(0) = L;
	      }
	      break;

	    case 3:
	    default:
	      retval(2) = fact.P ();
	      retval(1) = fact.U ();
	      retval(0) = fact.L ();
	      break;
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  ComplexLU fact (m);

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		ComplexMatrix P = fact.P ();
		ComplexMatrix L = P.transpose () * fact.L ();
		retval(1) = fact.U ();
		retval(0) = L;
	      }
	      break;

	    case 3:
	    default:
	      retval(2) = fact.P ();
	      retval(1) = fact.U ();
	      retval(0) = fact.L ();
	      break;
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("lu", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
