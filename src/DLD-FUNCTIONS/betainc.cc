/*

Copyright (C) 1997, 1999, 2000, 2004, 2005, 2006, 2007 John W. Eaton

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

#include "lo-specfun.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (betainc, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} betainc (@var{x}, @var{a}, @var{b})\n\
Return the incomplete Beta function,\n\
@iftex\n\
@tex\n\
$$\n\
 \\beta (x, a, b) = B (a, b)^{-1} \\int_0^x t^{(a-z)} (1-t)^{(b-1)} dt.\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@smallexample\n\
                                      x\n\
                                     /\n\
betainc (x, a, b) = beta (a, b)^(-1) | t^(a-1) (1-t)^(b-1) dt.\n\
                                     /\n\
                                  t=0\n\
@end smallexample\n\
@end ifinfo\n\
\n\
If x has more than one component, both @var{a} and @var{b} must be\n\
scalars.  If @var{x} is a scalar, @var{a} and @var{b} must be of\n\
compatible dimensions.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 3)
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);
      octave_value b_arg = args(2);

      // FIXME Can we make a template version of the duplicated code below
      if (x_arg.is_single_type () || a_arg.is_single_type () ||
	  b_arg.is_single_type ())
	{
	  if (x_arg.is_scalar_type ())
	    {
	      float x = x_arg.float_value ();

	      if (a_arg.is_scalar_type ())
		{
		  float a = a_arg.float_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  float b = b_arg.float_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  FloatNDArray b = b_arg.float_array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	      else
		{
		  FloatNDArray a = a_arg.float_array_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  float b = b_arg.float_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  FloatNDArray b = b_arg.float_array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	    }
	  else
	    {
	      FloatNDArray x = x_arg.float_array_value ();

	      if (a_arg.is_scalar_type ())
		{
		  float a = a_arg.float_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  float b = b_arg.float_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  FloatNDArray b = b_arg.float_array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	      else
		{
		  FloatNDArray a = a_arg.float_array_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  float b = b_arg.float_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  FloatNDArray b = b_arg.float_array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	    }
	}
      else
	{
	  if (x_arg.is_scalar_type ())
	    {
	      double x = x_arg.double_value ();

	      if (a_arg.is_scalar_type ())
		{
		  double a = a_arg.double_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  double b = b_arg.double_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  NDArray b = b_arg.array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	      else
		{
		  NDArray a = a_arg.array_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  double b = b_arg.double_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  NDArray b = b_arg.array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	    }
	  else
	    {
	      NDArray x = x_arg.array_value ();

	      if (a_arg.is_scalar_type ())
		{
		  double a = a_arg.double_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  double b = b_arg.double_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  NDArray b = b_arg.array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	      else
		{
		  NDArray a = a_arg.array_value ();

		  if (! error_state)
		    {
		      if (b_arg.is_scalar_type ())
			{
			  double b = b_arg.double_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		      else
			{
			  NDArray b = b_arg.array_value ();

			  if (! error_state)
			    retval = betainc (x, a, b);
			}
		    }
		}
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

