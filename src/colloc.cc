// f-colloc.cc                                           -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include "CollocWt.h"

#include "tree-const.h"
#include "error.h"
#include "utils.h"
#include "f-colloc.h"

#ifdef WITH_DLD
tree_constant *
builtin_colloc_2 (const tree_constant *args, int nargin, int nargout)
{
  return collocation_weights (args, nargin);
}
#endif

tree_constant *
collocation_weights (const tree_constant *args, int nargin)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (args[1].const_type () != tree_constant_rep::complex_scalar_constant
      && args[1].const_type () != tree_constant_rep::scalar_constant)
    {
      message ("colloc", "first argument must be a scalar");
      return retval;
    }

  int ncol = NINT (args[1].double_value ());
  if (ncol < 0)
    {
      message ("colloc", "first argument must be non-negative");
      return retval;
    }

  int ntot = ncol;
  int left = 0;
  int right = 0;

  for (int i = 2; i < nargin; i++)
    {
      if (args[i].is_defined ())
	{
	  if (! args[i].is_string_type ())
	    {
	      message ("colloc", "expecting string argument");
	      return retval;
	    }

	  char *s = args[i].string_value ();
	  if (s != (char *) NULL
	      && (((*s == 'R' || *s == 'r') && strlen (s) == 1)
		  || strcmp (s, "right") == 0))
	    {
	      right = 1;
	    }
	  else if (s != (char *) NULL
		   && (((*s == 'L' || *s == 'l') && strlen (s) == 1)
		       || strcmp (s, "left") == 0))
	    {
	      left = 1;
	    }
	  else
	    {
	      message ("colloc", "unrecognized argument");
	      return retval;
	    }
	}
      else
	{
	  message ("colloc", "unexpected NULL argument");
	  return retval;
	}
    }

  ntot += left + right;
  if (ntot < 1)
    message ("colloc", "the total number of roots must be positive");
  
  CollocWt wts (ncol, left, right);

  ColumnVector r = wts.roots ();
  Matrix A = wts.first ();
  Matrix B = wts.second ();
  ColumnVector q = wts.quad_weights ();

  retval = new tree_constant [5];

  retval[0] = tree_constant (r);
  retval[1] = tree_constant (A);
  retval[2] = tree_constant (B);
  retval[3] = tree_constant (q);
  retval[4] = tree_constant ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

