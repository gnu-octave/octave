// f-givens.cc                                           -*- C++ -*-
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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CMatrix.h"
#include "dMatrix.h"
#include "f77-uscore.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "tree-const.h"
#include "user-prefs.h"

extern "C"
{
  int F77_FCN (dlartg, DLARTG) (const double&, const double&, double&,
				double&, double&);

  int F77_FCN (zlartg, ZLARTG) (const Complex&, const Complex&,
				double&, Complex&, Complex&);
}

DEFUN_DLD_BUILTIN ("givens", Fgivens, Sgivens, 3, 2,
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

  tree_constant arg_a = args(0);
  tree_constant arg_b = args(1);

  if (! arg_a.is_scalar_type () && arg_b.is_scalar_type ())
    {
      error("givens: requires two scalar arguments");
      return retval;
    }

  Complex cx, cy;
  double x, y;

  if (arg_a.is_complex_type ())
    {
      cx = arg_a.complex_value ();

      if (error_state)
	return retval;
    }
  else 
    {
      x = arg_a.double_value ();

      if (error_state)
	return retval;

      cx = x;			// copy to complex just in case
    }

  if (arg_b.is_complex_type ())
    {
      cy = arg_b.complex_value ();

      if (error_state)
	return retval;
    }
  else
    {
      y = arg_b.double_value ();

      if (error_state)
	return retval;

      cy = y;			// copy to complex just in case
    }

// Now compute the rotation.

  double cc;
  if (arg_a.is_complex_type () || arg_b.is_complex_type ())
    {
      Complex cs, temp_r;
 
      F77_FCN (zlartg, ZLARTG) (cx, cy, cc, cs, temp_r);

      switch (nargout)
	{
	case 0:		// output a matrix
	case 1:
	  {
	    ComplexMatrix g (2, 2);
	    g.elem (0, 0) = cc;
	    g.elem (1, 1) = cc;
	    g.elem (0, 1) = cs;
	    g.elem (1, 0) = -conj (cs);

	    retval(0) = g;
	  }
	  break;
   
	case 2:		// output scalar values
	  retval(0) = cc;
	  retval(1) = cs;
	  break;

	default:  
	  error ("givens: invalid number of output arguments");
	  break;
	}
    }
  else
    {
      double s, temp_r;

      F77_FCN (dlartg, DLARTG) (x, y, cc, s, temp_r);

      switch (nargout)
	{
	case 0:		// output a matrix
	case 1:
	  {
	    Matrix g (2, 2);
	    g.elem (0, 0) = cc;
	    g.elem (1, 1) = cc;
	    g.elem (0, 1) = s;
	    g.elem (1, 0) = -s;

	    retval(0) = g;
	  }
	  break;
   
	case 2:		// output scalar values
	  retval(0) = cc;
	  retval(1) = s;
	  break;
   
	default:
	  error ("givens: invalid number of output arguments");
	  break;
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
