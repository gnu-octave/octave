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
#include "help.h"
#include "oct-obj.h"
#include "utils.h"

#define DO_BESSEL(type, alpha, x) \
  do \
    { \
      switch (type) \
	{ \
	  case 'j': \
	    retval = besselj (alpha, x); \
	    break; \
 \
	  case 'y': \
	    retval = bessely (alpha, x); \
	    break; \
 \
	  case 'i': \
	    retval = besseli (alpha, x); \
	    break; \
 \
	  case 'k': \
	    retval = besselk (alpha, x); \
	    break; \
 \
	  default: \
	    break; \
        } \
    } \
  while (0)

static void
gripe_bessel_arg_1 (const char *fn)
{
  error ("%s: alpha must be scalar or range with increment equal to 1", fn);
}

octave_value_list
do_bessel (char type, const char *fn, const octave_value_list& args)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_value alpha_arg = args(0);

      if (alpha_arg.is_scalar_type ())
	{
	  double alpha = alpha_arg.double_value ();

	  if (! error_state)
	    {
	      Matrix x = args(1).matrix_value ();

	      if (! error_state)
		DO_BESSEL (type, alpha, x);
	      else
		error ("%s: expecting matrix as second argument", fn);
	    }
	  else
	    gripe_bessel_arg_1 (fn);
	}
      else
	{
	  Range alpha;

	  if (! alpha_arg.is_range ())
	    {
	      ColumnVector tmp = alpha_arg.vector_value ();

	      if (! error_state)
		{
		  int len = tmp.length ();

		  double base = tmp(0);

		  for (int i = 1; i < len; i++)
		    {
		      if (tmp(i) != base + i)
			{
			  gripe_bessel_arg_1 (fn);
			  break;
			}
		    }

		  if (! error_state)
		    alpha = Range (tmp(0), tmp(len-1));
		}
	    }
	  else
	    alpha = alpha_arg.range_value ();

	  if (! error_state)
	    {
	      ColumnVector x = args(1).vector_value ();

	      if (! error_state)
		DO_BESSEL (type, alpha, x);
	      else
		error ("%s: expecting vector as second argument", fn);
	    }
	}
    }
  else
    print_usage (fn);

  return retval;
}

DEFUN_DLD (besselj, args, ,
  "besselj (alpha, x)\n\
\n\
Compute Bessel functions of the first kind.\n\
\n\
X must be a real matrix, vector or scalar.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If ALPHA is a\n\
range, X must be a vector or scalar, and the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.\n\
\n\
ALPHA must be greater than or equal to zero.  If ALPHA is a range, it\n\
must have an increment equal to one.")
{
  return do_bessel ('j', "besselj", args);
}

DEFUN_DLD (bessely, args, ,
  "bessely (alpha, x)\n\
\n\
Compute Bessel functions of the second kind.\n\
\n\
X must be a real matrix, vector or scalar.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If ALPHA is a\n\
range, X must be a vector or scalar, and the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.\n\
\n\
ALPHA must be greater than or equal to zero.  If ALPHA is a range, it\n\
must have an increment equal to one.")
{
  return do_bessel ('y', "bessely", args);
}

DEFUN_DLD (besseli, args, ,
  "besseli (alpha, x)\n\
\n\
Compute modified Bessel functions of the first kind.\n\
\n\
X must be a real matrix, vector or scalar.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If ALPHA is a\n\
range, X must be a vector or scalar, and the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.\n\
\n\
ALPHA must be greater than or equal to zero.  If ALPHA is a range, it\n\
must have an increment equal to one.")
{
  return do_bessel ('i', "besseli", args);
}

DEFUN_DLD (besselk, args, ,
  "besselk (alpha, x)\n\
\n\
Compute modified Bessel functions of the second kind.\n\
\n\
X must be a real matrix, vector or scalar.\n\
\n\
If ALPHA is a scalar, the result is the same size as X.  If ALPHA is a\n\
range, X must be a vector or scalar, and the result is a matrix with\n\
length(X) rows and length(ALPHA) columns.\n\
\n\
ALPHA must be greater than or equal to zero.  If ALPHA is a range, it\n\
must have an increment equal to one.")
{
  return do_bessel ('k', "besselk", args);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

