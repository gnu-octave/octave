// f-colloc.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

#include <string>

#include "CollocWt.h"

#include "defun-dld.h"
#include "error.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD_BUILTIN ("colloc", Fcolloc, Scolloc, FScolloc, 10,
  "[R, A, B, Q] = colloc (N [, \"left\"] [, \"right\"]): collocation weights")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ("colloc");
      return retval;
    }

  if (! args(0).is_scalar_type ())
    {
      error ("colloc: first argument must be a scalar");
      return retval;
    }

  double tmp = args(0).double_value ();

  if (error_state)
    return retval;

  if (xisnan (tmp))
    {
      error ("colloc: NaN is invalid as NCOL");
      return retval;
    }

  int ncol = NINT (tmp);
  if (ncol < 0)
    {
      error ("colloc: first argument must be non-negative");
      return retval;
    }

  int ntot = ncol;
  int left = 0;
  int right = 0;

  for (int i = 1; i < nargin; i++)
    {
      if (args(i).is_defined ())
	{
	  if (! args(i).is_string ())
	    {
	      error ("colloc: expecting string argument");
	      return retval;
	    }

	  string tstr = args(i).string_value ();
	  const char *s = tstr.c_str ();

	  if (s && (((*s == 'R' || *s == 'r') && strlen (s) == 1)
		    || strcmp (s, "right") == 0))
	    {
	      right = 1;
	    }
	  else if (s && (((*s == 'L' || *s == 'l') && strlen (s) == 1)
			 || strcmp (s, "left") == 0))
	    {
	      left = 1;
	    }
	  else
	    {
	      error ("colloc: unrecognized argument");
	      return retval;
	    }
	}
      else
	{
	  error ("colloc: unexpected NULL argument");
	  return retval;
	}
    }

  ntot += left + right;
  if (ntot < 1)
    {
      error ("colloc: the total number of roots must be positive");
      return retval;
    }
  
  CollocWt wts (ncol, left, right);

  ColumnVector r = wts.roots ();
  Matrix A = wts.first ();
  Matrix B = wts.second ();
  ColumnVector q = wts.quad_weights ();

  retval(3) = q;
  retval(2) = B;
  retval(1) = A;
  retval(0) = r;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

