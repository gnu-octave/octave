// f-fft.cc                                           -*- C++ -*-
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

// This function should be merged with Fifft.

DEFUN_DLD_BUILTIN ("fft", Ffft, Sfft, 3, 1,
  "fft (X [, N]): fast fourier transform of a vector")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ("fft");
      return retval;
    }

  tree_constant arg = args(0);

  int n_points = arg.rows ();
  if (n_points == 1)
    n_points = arg.columns ();

  if (nargin == 2)
    {
      double dval = args(1).double_value ();
      if (xisnan (dval))
	error ("fft: NaN is invalid as the N_POINTS");
      else
	n_points = NINT (dval);
    }

  if (error_state)
    return retval;

  if (n_points < 0)
    {
      error ("fft: number of points must be greater than zero");
      return retval;
    }

  int arg_is_empty = empty_arg ("fft", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty || n_points == 0)
    return Matrix ();

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  if (m.rows () == 1)
	    m.resize (1, n_points, 0.0);
	  else
	    m.resize (n_points, m.columns (), 0.0);
	  retval = m.fourier ();
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  if (m.rows () == 1)
	    m.resize (1, n_points, 0.0);
	  else
	    m.resize (n_points, m.columns (), 0.0);
	  retval = m.fourier ();
	}
    }
  else
    {
      gripe_wrong_type_arg ("fft", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
