// f-qr.cc                                           -*- C++ -*-
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

#include "dbleQR.h"
#include "CmplxQR.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "f-qr.h"

#ifdef WITH_DLD
Octave_object
builtin_qr_2 (const Octave_object& args, int nargout)
{
  return qr (args(1), nargout);
}
#endif

Octave_object
qr (const tree_constant& a, int nargout)
{
  Octave_object retval (2);

  tree_constant tmp = a.make_numeric ();;
    
  int nr = tmp.rows ();
  int nc = tmp.columns ();

  if (nr == 0 || nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0)
	    gripe_empty_arg ("qr", 0);
	  Matrix m;
	  retval(0) = m;
	  retval(1) = m;
	}
      else
	gripe_empty_arg ("qr", 1);

      return retval;
    }

  switch (tmp.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = tmp.matrix_value ();
	QR fact (m);
	retval(0) = fact.Q ();
	retval(1) = fact.R ();
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	ComplexQR fact (m);
	retval(0) = fact.Q ();
	retval(1) = fact.R ();
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	retval(0) = 1.0;
	retval(1) = d;
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval(0) = 1.0;
	retval(1) = c;
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
