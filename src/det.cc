// f-det.cc                                           -*- C++ -*-
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
#include "f-det.h"

#ifdef WITH_DLD
tree_constant *
builtin_det_2 (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];
  retval[0] = determinant (args[1]);
  return retval;
}
#endif

tree_constant
determinant (const tree_constant& a)
{
  tree_constant retval;

  tree_constant tmp = a.make_numeric ();;
    
  int nr = tmp.rows ();
  int nc = tmp.columns ();
  if (nr == 0 || nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	gripe_empty_arg ("det", 0);
      else if (flag == 0)
	gripe_empty_arg ("det", 1);
    }

  if (nr == 0 && nc == 0)
    return tree_constant (1.0);

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	if (m.rows () == m.columns ())
	  {
	    int info;
	    double rcond = 0.0;
	    DET det = m.determinant (info, rcond);
	    if (info == -1)
	      message ("det",
		       "matrix singular to machine precision, rcond = %g",
		       rcond);
	    else
	      {
		double d = det.value ();
		retval = tree_constant (d);
	      }
	  }
	else
	  gripe_square_matrix_required ("det");
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	if (m.rows () == m.columns ())
	  {
	    int info;
	    double rcond = 0.0;
	    ComplexDET det = m.determinant (info, rcond);
	    if (info == -1)
	      message ("det",
		       "matrix singular to machine precision, rcond = %g",
		       rcond);
	    else
	      {
		Complex c = det.value ();
		retval = tree_constant (c);
	      }
	  }
	else
	  gripe_square_matrix_required ("det");
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	retval = tree_constant (d);
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval = tree_constant (c);
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
