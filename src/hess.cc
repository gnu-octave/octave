// f-hess.cc                                           -*- C++ -*-
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

#include "dbleHESS.h"
#include "CmplxHESS.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "error.h"
#include "gripes.h"
#include "f-hess.h"

#ifdef WITH_DLD
Octave_object
builtin_hess_2 (const Octave_object& args, int nargin, int nargout)
{
  return hess (args, nargin, nargout);
}
#endif

Octave_object
hess (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  tree_constant arg = args(1).make_numeric ();

  int a_nr = arg.rows ();
  int a_nc = arg.columns ();

  if (a_nr == 0 || a_nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    warning ("hess: argument is empty matrix");
	  Matrix m;
	  retval.resize (2);
	  retval(0) = tree_constant (m);
	  retval(1) = tree_constant (m);
        }
      else
	error ("hess: empty matrix is invalid as argument");

      return retval;
    }

  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("hess");
      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;

  switch (arg.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	tmp = arg.matrix_value ();

	HESS result (tmp);

	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = tree_constant (result.hess_matrix ());
	  }
        else
	  {
	    retval.resize (2);
	    retval(0) = tree_constant (result.unitary_hess_matrix ());
	    retval(1) = tree_constant (result.hess_matrix ());
          }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ctmp = arg.complex_matrix_value ();

	ComplexHESS result (ctmp);

	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = tree_constant (result.hess_matrix ());
	  }
  	else
	  {
	    retval.resize (2);
	    retval(0) = tree_constant (result.unitary_hess_matrix ());
	    retval(1) = tree_constant (result.hess_matrix ());
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = arg.double_value ();
	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = tree_constant (d);
	  }
	else
	  {
	    retval.resize (2);
	    retval(0) = tree_constant (1);
	    retval(1) = tree_constant (d);
	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = arg.complex_value ();
	if (nargout == 0 || nargout == 1)
 	  {
	    retval.resize (1);
	    retval(0) = tree_constant (c);
	  }
	else
	  {
	    retval.resize (2);
	    retval(0) = tree_constant (1);
	    retval(1) = tree_constant (c);
	  }
      }
      break;
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
