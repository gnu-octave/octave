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
#include "defun-dld.h"

DEFUN_DLD ("qr", Fqr, Sqr, 2, 2,
  "[Q, R] = qr (X): form QR factorization of X")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 2)
    {
      print_usage ("qr");
      return retval;
    }

  tree_constant tmp = args(1).make_numeric ();
    
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
	  retval(1) = m;
	  retval(0) = m;
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
	retval(1) = fact.R ();
	retval(0) = fact.Q ();
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      {
	ComplexMatrix m = tmp.complex_matrix_value ();
	ComplexQR fact (m);
	retval(1) = fact.R ();
	retval(0) = fact.Q ();
      }
      break;
    case tree_constant_rep::scalar_constant:
      {
	double d = tmp.double_value ();
	retval(1) = d;
	retval(0) = 1.0;
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      {
	Complex c = tmp.complex_value ();
	retval(1) = c;
	retval(0) = 1.0;
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
