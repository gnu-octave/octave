// f-chol.cc                                           -*- C++ -*-
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

#include "Matrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "f-chol.h"

#ifdef WITH_DLD
tree_constant *
builtin_chol_2 (const tree_constant *args, int nargin, int nargout)
{
  return chol (args, nargin, nargout);
}
#endif

tree_constant *
chol (const tree_constant *a, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];

  tree_constant tmp = a[1].make_numeric ();;
    
  int nr = tmp.rows ();
  int nc = tmp.columns ();

  if (nr == 0 || nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("chol", 0);
	  Matrix m;
	  retval = new tree_constant [3];
	  retval[0] = tree_constant (m);
	  retval[1] = tree_constant (m);
	}
      else
	gripe_empty_arg ("chol", 1);

      return retval;
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
        int info;
	CHOL fact (m, info);
        if (info != 0)
          error ("chol: matrix not positive definite");
        else
  	  retval[0] = tree_constant (fact.chol_matrix ());
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
        int info;
	ComplexCHOL fact (m, info);
        if (info != 0)
          error ("chol: matrix not positive definite");
        else
	  retval[0] = tree_constant (fact.chol_matrix ());
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	retval[0] = tree_constant (d);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval[0] = tree_constant (c);
      }
      break;
    default:
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

