// f-inv.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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
#include <config.h>
#endif

#include "dMatrix.h"
#include "CMatrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("inv", Finv, Sinv, 2, 1,
  "inv (X): inverse of a square matrix")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("inv");
      return retval;
    }

  tree_constant arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("inverse", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Matrix ();

  if (nr != nc)
    {
      gripe_square_matrix_required ("inverse");
      return retval;
    }

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
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
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
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
    }
  else
    {
      gripe_wrong_type_arg ("inv", arg);
    }

  return retval;
}

// XXX FIXME XXX -- this should really be done with an alias, but
// alias_builtin() won't do the right thing if we are actually using
// dynamic linking.

DEFUN_DLD_BUILTIN ("inverse", Finverse, Sinverse, 2, 1,
  "inverse (X): inverse of a square matrix")
{
  return Finv (args, nargout);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
