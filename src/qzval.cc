// f-qzval.cc                                           -*- C++ -*-
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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include "CColVector.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "f77-uscore.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "tree-const.h"
#include "user-prefs.h"
#include "utils.h"

extern "C"
{
  int F77_FCN (qzhes, QZHES) (const int&, const int&, double*,
			      double*, const long&, double*);
 
  int F77_FCN (qzit, QZIT) (const int&, const int&, double*, double*,
			    const double&, const long&, double*,
			    int&);
 
  int F77_FCN (qzval, QZVAL) (const int&, const int&, double*,
			      double*, double*, double*, double*,
			      const long&, double*);
}

DEFUN_DLD_BUILTIN ("qzval", Fqzval, Sqzval, 3, 1,
  "X = qzval (A, B)\n\
\n\
compute generalized eigenvalues of the matrix pencil (A - lambda B).\n\
A and B must be real matrices.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 1)
    {
      print_usage ("qzval");
      return retval;
    }

  tree_constant arg_a = args(0);
  tree_constant arg_b = args(1);

  int a_nr = arg_a.rows();
  int a_nc = arg_a.columns();

  int b_nr = arg_b.rows();
  int b_nc = arg_b.columns();

  int arg_a_is_empty = empty_arg ("qzval", a_nr, a_nc);
  int arg_b_is_empty = empty_arg ("qzval", b_nr, b_nc);

  if (arg_a_is_empty > 0 && arg_b_is_empty > 0)
    return Matrix ();
  else if (arg_a_is_empty || arg_b_is_empty)
    return retval;

  // Arguments are not empty, so check for correct dimensions.

  if (a_nr != a_nc || b_nr != b_nc)
    {
      gripe_square_matrix_required ("qzval: first two parameters:");
      return retval;
    }

  if (a_nr != b_nr)
    {
      gripe_nonconformant ();
      return retval;
    }
  
  // Dimensions look o.k., let's solve the problem.

  if (arg_a.is_complex_type () || arg_b.is_complex_type ())
    {
      error ("qzval: cannot yet do complex matrix arguments\n");
      return retval;
    }

  // Do everything in real arithmetic.

  Matrix jnk (a_nr, a_nr, 0.0);

  ColumnVector alfr (a_nr);
  ColumnVector alfi (a_nr);
  ColumnVector beta (a_nr);

  long matz = 0;
  int info;

  // XXX FIXME ??? XXX
  double eps = DBL_EPSILON;

  Matrix ca = arg_a.matrix_value ();

  if (error_state)
    return retval;

  Matrix cb = arg_b.matrix_value ();

  if (error_state)
    return retval;

  // Use EISPACK qz functions.

  F77_FCN (qzhes, QZHES) (a_nr, a_nr, ca.fortran_vec (),
			  cb.fortran_vec (), matz,
			  jnk.fortran_vec ()); 

  F77_FCN (qzit, QZIT) (a_nr, a_nr, ca.fortran_vec (),
			cb.fortran_vec (), eps, matz,
			jnk.fortran_vec (), info);

  if (info)
    error ("qzval: trouble in qzit, info = %d", info);

  F77_FCN (qzval, QZVAL) (a_nr, a_nr, ca.fortran_vec (),
			  cb.fortran_vec (), alfr.fortran_vec (),
			  alfi.fortran_vec (), beta.fortran_vec (),
			  matz, jnk.fortran_vec ());

  // Count and extract finite generalized eigenvalues.

  int i;
  int cnt = 0;

  Complex Im (0, 1);

  for (i = 0; i < a_nr; i++)
    if (beta (i) != 0)
      cnt++;

  ComplexColumnVector cx (cnt, 0.0);

  for (i = 0; i < a_nr; i++)
    {
      if (beta (i) != 0)
	{
	  // Finite generalized eigenvalue.

	  cnt--;
	  cx (cnt) = (alfr (i) + Im * alfi (i)) / beta (i);
	}
    }

  retval = cx;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
