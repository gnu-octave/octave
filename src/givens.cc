// tc-givens.cc                                           -*- C++ -*-
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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "dMatrix.h"
#include "CMatrix.h"
#include "f77-uscore.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "error.h"
#include "gripes.h"
#include "f-givens.h"

extern "C"
{
  int F77_FCN (dlartg) (const double*, const double*, double*, double*,
			double*);

  int F77_FCN (zlartg) (const Complex*, const Complex*, double*,
			Complex*, Complex*);
}

// These aren't used?
#if 0
int F77_FCN (dorgqr) (const int*, const int*, const int*, double*,
		      const int*, double*, double*, const int*, int*);
  
int F77_FCN (zunghr) (const int*, const int*, const int*, Complex*,
		      const int*, Complex*, Complex*, const int*,
		      int*, long, long);
#endif

#ifdef WITH_DLD
tree_constant *
builtin_givens_2 (const tree_constant *args, int nargin, int nargout)
{
  return givens (args, nargin, nargout);
}
#endif

tree_constant *
givens (const tree_constant *args, int nargin, int nargout)
{

  tree_constant *retval = NULL_TREE_CONST;

  tree_constant arga = args[1].make_numeric ();
  tree_constant argb = args[2].make_numeric ();

  if (! arga.is_scalar_type () && argb.is_scalar_type ())
    {
      error("givens: requires two scalar arguments"); 
    }
  else
    {

      retval = new tree_constant [nargout+1];

      Complex cx, cy;
      double x, y;

      if (arga.is_complex_type ())
	cx = arga.complex_value ();
      else 
	{
	  x = arga.double_value ();
	  cx = x;			// copy to complex just in case
	}

      if (argb.is_complex_type ())
	cy = argb.complex_value ();
      else
	{
	  y = argb.double_value ();
	  cy = y;			// copy to complex just in case
	}

// Now compute the rotation.

      double cc;
      if (arga.is_complex_type () || argb.is_complex_type ())
	{
	  Complex cs, temp_r;
 
	  F77_FCN (zlartg) (&cx, &cy, &cc, &cs, &temp_r);

	  switch (nargout)
	    {
	    case 1:		// output a matrix
	      {
		ComplexMatrix g (2, 2);
		g.elem (0, 0) = cc;
		g.elem (1, 1) = cc;
		g.elem (0, 1) = cs;
		g.elem (1, 0) = -conj (cs);

		retval[0] = tree_constant (g);
	      }
	      break;
   
	    case 2:		// output scalar values
	      retval[0] = tree_constant(cc);
	      retval[1] = tree_constant(cs);
	      break;

	    default:  
	      error ("givens: illegal number of output arguments");
	      break;
	    }
	}
      else
	{
	  double s, temp_r;
 
	  F77_FCN (dlartg) (&x, &y, &cc, &s, &temp_r);

	  switch (nargout)
	    {
	    case 1:		// output a matrix
	      {
		Matrix g (2, 2);
		g.elem (0, 0) = cc;
		g.elem (1, 1) = cc;
		g.elem (0, 1) = s;
		g.elem (1, 0) = -s;

		retval[0] = tree_constant (g);
	      }
	      break;
   
	    case 2:		// output scalar values
	      retval[0] = tree_constant (cc);
	      retval[1] = tree_constant (s);
	      break;
   
	    default:
	      error ("givens: illegal number of output arguments");
	      break;
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
