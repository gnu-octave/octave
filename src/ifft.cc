// f-ifft.cc                                           -*- C++ -*-
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
#include "f-ifft.h"

#ifdef WITH_DLD
tree_constant *
builtin_ifft_2 (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = new tree_constant [2];
  retval[0] = ifft (args[1]);
  return retval;
}
#endif

tree_constant
ifft (const tree_constant& a)
{
  tree_constant retval;

  tree_constant tmp = a.make_numeric ();;
    
  if (tmp.rows () == 0 || tmp.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("ifft", 0);
	  Matrix m;
	  retval = tree_constant (m);
	}
      else
	gripe_empty_arg ("ifft", 1);

      return retval;
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	ComplexMatrix mifft = m.ifourier ();
	retval = tree_constant (mifft);
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	ComplexMatrix mifft = m.ifourier ();
	retval = tree_constant (mifft);
      }
      break;
    case tree_constant_rep::scalar_constant:
    case tree_constant_rep::complex_scalar_constant:
      error ("ifft: invalid scalar arguement");
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
