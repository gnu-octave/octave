// f-eig.cc                                           -*- C++ -*-
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

#include "EIG.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "f-eig.h"

#ifdef WITH_DLD
Octave_object
builtin_eig_2 (const Octave_object& args, int nargin, int nargout)
{
  return eig (args, nargin, nargout);
}
#endif

Octave_object
eig (const Octave_object& args, int nargin, int nargout)
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
	    gripe_empty_arg ("eig", 0);
	  Matrix m;
	  retval.resize (2);
	  retval(0) = tree_constant (m);
	  retval(1) = tree_constant (m);
	}
      else
	gripe_empty_arg ("eig", 1);

      return retval;
    }

  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("eig");
      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;
  EIG result;
  switch (arg.const_type ())
    {
    case tree_constant_rep::scalar_constant:
      tmp.resize (1, 1);
      tmp.elem (0, 0) = arg.double_value ();
      result = EIG (tmp);
      break;
    case tree_constant_rep::matrix_constant:
      tmp = arg.matrix_value ();
      result = EIG (tmp);
      break;
    case tree_constant_rep::complex_scalar_constant:
      ctmp.resize (1, 1);
      ctmp.elem (0, 0) = arg.complex_value ();
      result = EIG (ctmp);
      break;
    case tree_constant_rep::complex_matrix_constant:
      ctmp = arg.complex_matrix_value ();
      result = EIG (ctmp);
      break;
    default:
      panic_impossible ();
      break;
    }

  if (nargout == 0 || nargout == 1)
    {
      retval.resize (1);
      retval(0) = tree_constant (result.eigenvalues (), 1);
    }
  else
    {
// Blame it on Matlab.

      ComplexDiagMatrix d (result.eigenvalues ());

      retval.resize (2);
      retval(0) = tree_constant (result.eigenvectors ());
      retval(1) = tree_constant (d);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
