// f-colloc.cc                                           -*- C++ -*-
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

#include "CollocWt.h"

#include "tree-const.h"
#include "error.h"
#include "utils.h"
#include "defun-dld.h"

DEFUN_DLD ("colloc", Fcolloc, Scolloc, 7, 4,
  "[R, A, B, Q] = colloc (N [, \"left\"] [, \"right\"]): collocation weights")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 2 || nargin > 4)
    {
      print_usage ("colloc");
      return retval;
    }

  if (args(1).const_type () != tree_constant_rep::complex_scalar_constant
      && args(1).const_type () != tree_constant_rep::scalar_constant)
    {
      error ("colloc: first argument must be a scalar");
      return retval;
    }

  int ncol = NINT (args(1).double_value ());
  if (ncol < 0)
    {
      error ("colloc: first argument must be non-negative");
      return retval;
    }

  int ntot = ncol;
  int left = 0;
  int right = 0;

  for (int i = 2; i < nargin; i++)
    {
      if (args(i).is_defined ())
	{
	  if (! args(i).is_string_type ())
	    {
	      error ("colloc: expecting string argument");
	      return retval;
	    }

	  char *s = args(i).string_value ();
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

  retval.resize (4);

  retval(0) = r;
  retval(1) = A;
  retval(2) = B;
  retval(3) = q;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

