// f-eig.cc                                           -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "EIG.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("eig", Feig, Seig, 2, 1,
  "eig (X) or [V, D] = eig (X): compute eigenvalues and eigenvectors of X")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 2)
    {
      print_usage ("eig");
      return retval;
    }

  tree_constant arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("eig", nr, nc);
  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return Octave_object (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("eig");
      return retval;
    }

  Matrix tmp;
  ComplexMatrix ctmp;
  EIG result;

  if (arg.is_real_type ())
    {
      tmp = arg.matrix_value ();

      if (error_state)
	return retval;
      else
	result = EIG (tmp);
    }
  else if (arg.is_complex_type ())
    {
      ctmp = arg.complex_matrix_value ();

      if (error_state)
	return retval;
      else
	result = EIG (ctmp);
    }
  else
    {
      gripe_wrong_type_arg ("eig", tmp);
      return retval;
    }

  if (nargout == 0 || nargout == 1)
    {
      retval(0) = result.eigenvalues (), 1;
    }
  else
    {
// Blame it on Matlab.

      ComplexDiagMatrix d (result.eigenvalues ());

      retval(1) = d;
      retval(0) = result.eigenvectors ();
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
