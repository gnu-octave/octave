// f-qzval.cc                                           -*- C++ -*-
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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <float.h>

#include "dMatrix.h"
#include "dColVector.h"
#include "CColVector.h"
#include "f77-uscore.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "help.h"
#include "defun-dld.h"

extern "C"
{
  int F77_FCN (qzhes) (const int*, const int*, double*, double*, const
		       long*, double*);
 
  int F77_FCN (qzit) (const int*, const int*, double*, double*, const
		      double*, const long*, double*, int*);
 
  int F77_FCN (qzval) (const int*, const int*, double*, double*,
		       double*, double*, double*, const long*, double*);
}

DEFUN_DLD ("qzvalue", Fqzvalue, Sqzvalue, 3, 1,
  "X = qzval (A, B)\n\
\n\
compute generalized eigenvalues of the matrix pencil (A - lambda B).\n\
A and B must be real matrices.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 3 || nargout > 1)
    {
      print_usage ("qzvalue");
      return retval;
    }

  tree_constant arga = args(1).make_numeric ();
  tree_constant argb = args(2).make_numeric();

  if (arga.is_empty () || argb.is_empty ())
    retval = vector_of_empties (nargout, "qzvalue");
  else
    {

// Arguments are not empty, so check for correct dimensions.

      int a_rows = arga.rows();
      int a_cols = arga.columns();
      int b_rows = argb.rows();
      int b_cols = argb.columns();
  
      if ((a_rows != a_cols) || (b_rows != b_cols))
	{
	  gripe_square_matrix_required ("qzvalue: first two parameters:");
	  return retval;
	}
      else if (a_rows != b_rows)
	{
	  gripe_nonconformant ();
	  return retval;
	}
  
// Dimensions look o.k., let's solve the problem.

      retval.resize (nargout ? nargout : 1);

      if (arga.is_complex_type () || argb.is_complex_type ())
	error ("qzvalue: cannot yet do complex matrix arguments\n");
      else
	{

// Do everything in real arithmetic.

	  Matrix jnk (a_rows, a_rows, 0.0);

	  ColumnVector alfr (a_rows);
	  ColumnVector alfi (a_rows);
	  ColumnVector beta (a_rows);

	  long matz = 0;
	  int info;

// XXX FIXME ??? XXX
	  double eps = DBL_EPSILON;

	  Matrix ca = arga.matrix_value ();
	  Matrix cb = argb.matrix_value ();

// Use EISPACK qz functions.

	  F77_FCN (qzhes) (&a_rows, &a_rows, ca.fortran_vec (),
			   cb.fortran_vec (), &matz, jnk.fortran_vec ());

	  F77_FCN (qzit) (&a_rows, &a_rows, ca.fortran_vec (),
			  cb.fortran_vec (), &eps, &matz,
			  jnk.fortran_vec (), &info);  

	  if (info)
	    error ("qzvalue: trouble in qzit, info = %d", info);

	  F77_FCN (qzval) (&a_rows, &a_rows, ca.fortran_vec (),
			   cb.fortran_vec (), alfr.fortran_vec (),
			   alfi.fortran_vec (), beta.fortran_vec (),
			   &matz, jnk.fortran_vec ());

// Count and extract finite generalized eigenvalues.

	  int i, cnt;
	  Complex Im (0, 1);
	  for (i = 0, cnt = 0; i < a_rows; i++)
	    if (beta (i) != 0)
	      cnt++;

	  ComplexColumnVector cx (cnt, 0.0);

	  for (i = 0; i < a_rows; i++)
	    {
	      if (beta (i) != 0)
		{

// Finite generalized eigenvalue.

		  cnt--;
		  cx (cnt) = (alfr (i) + Im * alfi (i)) / beta (i);
		}
	    }
	  retval(0) = cx;
	}
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
