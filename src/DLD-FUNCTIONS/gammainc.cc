/*

Copyright (C) 1997 John W. Eaton

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

#include "lo-specfun.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (gammainc, args, ,
  "gammainc (x, a)\n\
\n\
Compute the incomplete gamma function\n\
\n\
  gammainc(x,a) = (\\int_0^x exp(-t) t^(a-1) dt) / gamma(a).\n\
\n\
If a is scalar, then gammainc(x,a) is returned for each element of x\n\
and vice versa.\n\
\n\
If neither a nor x is scalar, the sizes of a and x must agree, and\n\
gammainc is applied for corresponding elements of x and a.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);

      if (x_arg.is_scalar_type ())
	{
	  double x = x_arg.double_value ();

	  if (! error_state)
	    {
	      if (a_arg.is_scalar_type ())
		{
		  double a = a_arg.double_value ();

		  if (! error_state)
		    retval = gammainc (x, a);
		}
	      else
		{
		  Matrix a = a_arg.matrix_value ();

		  if (! error_state)
		    retval = gammainc (x, a);
		}
	    }
	}
      else
	{
	  Matrix x = x_arg.matrix_value ();

	  if (! error_state)
	    {
	      if (a_arg.is_scalar_type ())
		{
		  double a = a_arg.double_value ();

		  if (! error_state)
		    retval = gammainc (x, a);
		}
	      else
		{
		  Matrix a = a_arg.matrix_value ();

		  if (! error_state)
		    retval = gammainc (x, a);
		}
	    }
	}
    }
  else
    print_usage ("gammainc");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

