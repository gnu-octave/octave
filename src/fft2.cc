// f-fft2.cc                                           -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "user-prefs.h"
#include "utils.h"

// This function should be merged with Fifft2.

DEFUN_DLD_BUILTIN (fft2, args, ,
  "fft2 (X [, N] [, M])\n\
\n\
two dimensional fast fourier transform of a vector")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ("fft2");
      return retval;
    }

  tree_constant arg = args(0);

  int n_rows = arg.rows ();
  if (nargin > 1)
    {
      double dval = args(1).double_value ();
      if (xisnan (dval))
	error ("fft2: NaN is invalid as N_ROWS");
      else
	n_rows = NINT (dval);
    }

  if (error_state)
    return retval;

  int n_cols = arg.columns ();
  if (nargin > 2)
    {
      double dval = args(2).double_value ();
      if (xisnan (dval))
	error ("fft2: NaN is invalid as N_COLS");
      else
	n_cols = NINT (dval);
    }

  if (error_state)
    return retval;

  if (n_rows < 0 || n_cols < 0)
    {
      error ("fft2: number of points must be greater than zero");
      return retval;
    }

  int arg_is_empty = empty_arg ("fft2", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty || n_rows == 0 || n_cols == 0)
    return Matrix ();

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  m.resize (n_rows, n_cols, 0.0);
	  retval = m.fourier2d ();
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  m.resize (n_rows, n_cols, 0.0);
	  retval = m.fourier2d ();
	}
    }
  else
    {
      gripe_wrong_type_arg ("fft2", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
