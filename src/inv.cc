// f-inv.cc                                           -*- C++ -*-
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

#include "dMatrix.h"
#include "CMatrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD ("inv", Finv, Sinv, 2, 1,
  "inv (X): inverse of a square matrix")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2)
    {
      print_usage ("inv");
      return retval;
    }

  tree_constant tmp = args(1).make_numeric ();

  int nr = tmp.rows ();
  int nc = tmp.columns ();
  if (nr == 0 || nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	gripe_empty_arg ("inverse", 0);
      else if (flag == 0)
	gripe_empty_arg ("inverse", 1);
    }

  Matrix mtmp;
  if (nr == 0 && nc == 0)
    return mtmp;

  if (tmp.is_real_matrix ())
    {
      Matrix m = tmp.matrix_value ();
      if (m.rows () == m.columns ())
	{
	  int info;
	  double rcond = 0.0;
	  Matrix minv = m.inverse (info, rcond);
	  if (info == -1)
	    warning ("inverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	  else
	    retval = minv;
	}
      else
	gripe_square_matrix_required ("inverse");
    }
  else if (tmp.is_real_scalar ())
    {
      double d = 1.0 / tmp.double_value ();
      retval = d;
    }
  else if (tmp.is_complex_matrix ())
    {
      ComplexMatrix m = tmp.complex_matrix_value ();
      if (m.rows () == m.columns ())
	{
	  int info;
	  double rcond = 0.0;
	  ComplexMatrix minv = m.inverse (info, rcond);
	  if (info == -1)
	    warning ("inverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	  else
	    retval = minv;
	}
      else
	gripe_square_matrix_required ("inverse");
    }
  else if (tmp.is_complex_scalar ())
    {
      Complex c = 1.0 / tmp.complex_value ();
      retval = c;
    }
  else
    {
      gripe_wrong_type_arg ("inv", tmp);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
