// f-schur.cc                                           -*- C++ -*-
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
#include "error.h"
#include "gripes.h"
#include "f-schur.h"

#ifdef WITH_DLD
tree_constant *
builtin_schur_2 (tree_constant *args, int nargin, int nargout)
{
  return schur (args, nargin, nargout);
}
#endif

tree_constant *
schur (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  tree_constant arg = args[1].make_numeric ();

  char *ord;
  if (nargin != 3)
    ord = "U";
  else
    ord = args[2].string_value ();

  if (*ord != 'U' && *ord != 'A' && *ord != 'D'
      && *ord != 'u' && *ord != 'a' && *ord != 'd')
    {
      warning ("schur: incorrect ordered schur argument `%c'", *ord);
      Matrix m;
      retval = new tree_constant [3];
      retval[0] = tree_constant (m);
      retval[1] = tree_constant (m);
      return retval;
    }
  int a_nr = arg.rows ();
  int a_nc = arg.columns ();

  if (a_nr == 0 || a_nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
        {
          if (flag < 0)
            warning ("schur: argument is empty matrix");
          Matrix m;
          retval = new tree_constant [3];
          retval[0] = tree_constant (m);
          retval[1] = tree_constant (m);
        }
      else
        error ("schur: empty matrix is invalid as argument");

      return retval;
    }
  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("schur");
      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;
 
  switch (arg.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	tmp = arg.matrix_value ();

	SCHUR result (tmp,ord);

        if (nargout == 1)
          {
            retval = new tree_constant [2];
            retval[0] = tree_constant (result.schur_matrix ());
          }
        else
          {
            retval = new tree_constant [3];
            retval[0] = tree_constant (result.unitary_matrix ());
            retval[1] = tree_constant (result.schur_matrix ());
          }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ctmp = arg.complex_matrix_value ();

        ComplexSCHUR result (ctmp,ord);
 
        if (nargout == 1)
          {
            retval = new tree_constant [2];
            retval[0] = tree_constant (result.schur_matrix ());
          }
        else
          {
            retval = new tree_constant [3];
            retval[0] = tree_constant (result.unitary_matrix ());
            retval[1] = tree_constant (result.schur_matrix ());
          }
      }    
      break;
    case tree_constant_rep::scalar_constant:
      {
        double d = arg.double_value ();
        if (nargout == 1)
  	  {
	    retval = new tree_constant [2];
            retval[0] = tree_constant (d);
          }
        else
	  {
	    retval = new tree_constant [3];
	    retval[0] = tree_constant (1);
	    retval[1] = tree_constant (d);
  	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
        Complex c = arg.complex_value ();
	if (nargout == 1)
	  {
	    retval = new tree_constant [2];
	    retval[0] = tree_constant (c);
	  }
	else
	  {
	    retval = new tree_constant [3];
	    retval[0] = tree_constant (1);
	    retval[1] = tree_constant (c);
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
