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

#include "dbleQRP.h"
#include "CmplxQRP.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

DEFUN_DLD_BUILTIN ("qr", Fqr, Sqr, 2, 2,
  "[Q, R] = qr (X):      form Q unitary and R upper triangular such\n\
                       that Q * R = X\n\
\n\
[Q, R] = qr (X, 0):    form the economy decomposition such that if X is\n\
                       if X is m by n then only the first n columns of Q\n\
                       are computed.\n\
\n\
[Q, R, P] = qr (X):    form QRP factorization of X where\n\
                       P is a permutation matrix such that\n\
                       A * P = Q * R\n\
\n\
[Q, R, P] = qr (X, 0): form the economy decomposition with \n\
                       permutation vector P such that Q * R = X (:, P)\n\
\n\
qr (X) alone returns the output of the LAPACK routine dgeqrf, such\n\
that R = triu (qr (X))")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 && nargin != 3 || nargout > 3)
    {
      print_usage ("qr");
      return retval;
    }

  tree_constant arg = args(1);
    
  if (empty_arg ("qr", arg.rows (), arg.columns ()) < 0)
    return retval;

  QR::type type = nargout == 1 ? QR::raw
    : (nargin == 3 ? QR::economy : QR::std);

  if (arg.is_real_type ())
    {
      Matrix m = arg.matrix_value ();

      if (! error_state)
	{
	  if (nargout < 3)
	    {
	      QR fact (m, type);
	      retval(1) = fact.R ();
	      retval(0) = fact.Q ();
	    }
	  else
	    {
	      QRP fact (m, type);
	      retval(2) = fact.P ();
	      retval(1) = fact.R ();
	      retval(0) = fact.Q ();
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (! error_state)
	{
	  if (nargout < 3)
	    {
	      ComplexQR fact (m, type);
	      retval(1) = fact.R ();
	      retval(0) = fact.Q ();
	    }
	  else
	    {
	      ComplexQRP fact (m, type);
	      retval(2) = fact.P ();
	      retval(1) = fact.R ();
	      retval(0) = fact.Q ();
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("qr", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
