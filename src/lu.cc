// f-lu.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dbleLU.h"
#include "CmplxLU.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("lu", Flu, Slu, 2, 3,
  "[L, U, P] = lu (A): LU factorization")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ("lu");
      return retval;
    }

  tree_constant arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("lu", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Octave_object (3, Matrix ());

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
