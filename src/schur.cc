// f-schur.cc                                           -*- C++ -*-
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

#include "dbleSCHUR.h"
#include "CmplxSCHUR.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "error.h"
#include "gripes.h"
#include "f-schur.h"

#ifdef WITH_DLD
Octave_object
builtin_schur_2 (const Octave_object& args, int nargout)
{
  return schur (args, nargout);
}
#endif

Octave_object
schur (const Octave_object& args, int nargout)
{
  Octave_object retval;

  int nargin = args.length ();

  tree_constant arg = args(1).make_numeric ();

  char *ord;
  if (nargin != 3)
    ord = "U";
  else
    ord = args(2).string_value ();

  if (*ord != 'U' && *ord != 'A' && *ord != 'D'
      && *ord != 'u' && *ord != 'a' && *ord != 'd')
    {
      warning ("schur: incorrect ordered schur argument `%c'", *ord);
      Matrix m;
      retval.resize (2);
      retval(0) = m;
      retval(1) = m;
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
          retval.resize (2);
          retval(0) = m;
          retval(1) = m;
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

        if (nargout == 0 || nargout == 1)
          {
            retval.resize (1);
            retval(0) = result.schur_matrix ();
          }
        else
          {
            retval.resize (2);
            retval(0) = result.unitary_matrix ();
            retval(1) = result.schur_matrix ();
          }
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ctmp = arg.complex_matrix_value ();

        ComplexSCHUR result (ctmp,ord);
 
        if (nargout == 0 || nargout == 1)
          {
            retval.resize (1);
            retval(0) = result.schur_matrix ();
          }
        else
          {
            retval.resize (2);
            retval(0) = result.unitary_matrix ();
            retval(1) = result.schur_matrix ();
          }
      }    
      break;
    case tree_constant_rep::scalar_constant:
      {
        double d = arg.double_value ();
        if (nargout == 0 || nargout == 1)
  	  {
	    retval.resize (1);
            retval(0) = d;
          }
        else
	  {
	    retval.resize (2);
	    retval(0) = 1.0;
	    retval(1) = d;
  	  }
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
        Complex c = arg.complex_value ();
	if (nargout == 0 || nargout == 1)
	  {
	    retval.resize (1);
	    retval(0) = c;
	  }
	else
	  {
	    retval.resize (2);
	    retval(0) = 1.0;
	    retval(1) = c;
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
