// f-givens.cc                                           -*- C++ -*-
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

// Originally written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "help.h"
#include "oct-obj.h"

DEFUN_DLD_BUILTIN (givens, args, nargout,
  "G = givens (X, Y)\n\
\n\
compute orthogonal matrix G = [c s; -conj (s) c]\n\
such that G [x; y] = [*; 0]  (x, y scalars)\n\
\n\
[c, s] = givens (x, y) returns the (c, s) values themselves.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 2)
    {
      print_usage ("givens");
      return retval;
    }
  else
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
	{
	  Complex cx = args(0).complex_value ();
	  Complex cy = args(1).complex_value ();

	  if (! error_state)
	    {
	      ComplexMatrix result = Givens (cx, cy);

	      if (! error_state)
		{
		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = result;
		      break;
   
		    case 2:
		      retval(1) = result (0, 1);
		      retval(0) = result (0, 0);
		      break;

		    default:
		      error ("givens: invalid number of output arguments");
		      break;
		    }
		}
	    }
	}
      else
	{
	  double x = args(0).double_value ();
	  double y = args(1).double_value ();

	  if (! error_state)
	    {
	      Matrix result = Givens (x, y);

	      if (! error_state)
		{
		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = result;
		      break;
   
		    case 2:
		      retval(1) = result (0, 1);
		      retval(0) = result (0, 0);
		      break;

		    default:
		      error ("givens: invalid number of output arguments");
		      break;
		    }
		}
	    }
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
