// f-lu.cc                                           -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "Matrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "f-lu.h"

#ifdef WITH_DLD
tree_constant *
builtin_lu_2 (const tree_constant *args, int nargin, int nargout)
{
  return lu (args[1], nargout);
}
#endif

tree_constant *
lu (const tree_constant& a, int nargout)
{
  tree_constant *retval = new tree_constant [4];

  tree_constant tmp = a.make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("lu", 0);
	  Matrix m;
	  retval = new tree_constant [4];
	  retval[0] = tree_constant (m);
	  retval[1] = tree_constant (m);
	  retval[2] = tree_constant (m);
	  return retval;
	}
      else
	gripe_empty_arg ("lu", 1);
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	if (m.rows () == m.columns ())
	  {
	    LU fact (m);
	    switch (nargout)
	      {
	      case 1:
	      case 2:
		{
		  Matrix P = fact.P ();
		  Matrix L = P.transpose () * fact.L ();
		  retval[0] = tree_constant (L);
		  retval[1] = tree_constant (fact.U ());
		}
		break;
	      case 3:
	      default:
		retval[0] = tree_constant (fact.L ());
		retval[1] = tree_constant (fact.U ());
		retval[2] = tree_constant (fact.P ());
		break;
	      }
	  }
	else
	  gripe_square_matrix_required ("lu");
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	if (m.rows () == m.columns ())
	  {
	    ComplexLU fact (m);
	    switch (nargout)
	      {
	      case 1:
	      case 2:
		{
		  ComplexMatrix P = fact.P ();
		  ComplexMatrix L = P.transpose () * fact.L ();
		  retval[0] = tree_constant (L);
		  retval[1] = tree_constant (fact.U ());
		}
		break;
	      case 3:
	      default:
		retval[0] = tree_constant (fact.L ());
		retval[1] = tree_constant (fact.U ());
		retval[2] = tree_constant (fact.P ());
		break;
	      }
	  }
	else
	  gripe_square_matrix_required ("lu");
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	retval[0] = tree_constant (1.0);
	retval[1] = tree_constant (d);
	retval[2] = tree_constant (1.0);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval[0] = tree_constant (1.0);
	retval[1] = tree_constant (c);
	retval[2] = tree_constant (1.0);
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
