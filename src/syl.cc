// f-syl.cc                                           -*- C++ -*-
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

#include "dMatrix.h"
#include "CMatrix.h"
#include "dbleSCHUR.h"
#include "CmplxSCHUR.h"
#include "f77-uscore.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "help.h"
#include "defun-dld.h"

extern "C"
{
  int F77_FCN (dtrsyl) (const char*, const char*, const int*,
			const int*, const int*, const double*,
			const int*, const double*, const int*,
			const double*, const int*, double*, int*,
			long, long);
 
  int F77_FCN (ztrsyl) (const char*, const char*, const int*,
			const int*, const int*, const Complex*,
			const int*, const Complex*, const int*,
			const Complex*, const int*, double*, int*,
			long, long);
}

DEFUN_DLD ("syl", Fsyl, Ssyl, 4, 1,
  "X = syl (A, B, C): solve the Sylvester equation A X + X B + C = 0")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 4 || nargout > 1)
    {
      print_usage ("syl");
      return retval;
    }

  tree_constant arga = args(1).make_numeric ();
  tree_constant argb = args(2).make_numeric ();
  tree_constant argc = args(3).make_numeric ();

  if (arga.is_empty () || argb.is_empty () || argc.is_empty ())
    retval = vector_of_empties (nargout, "syl");
  else
    {

// Arguments are not empty, so check for correct dimensions.

      int a_rows = arga.rows ();
      int a_cols = arga.columns ();
      int b_rows = argb.rows ();
      int b_cols = argb.columns ();
      int c_rows = argc.rows ();
      int c_cols = argc.columns ();
  
      if ((a_rows != a_cols) || (b_rows != b_cols))
	{
	  gripe_square_matrix_required ("syl: first two parameters:");
	  return retval;
	}
      else if ((a_rows != c_rows) || (b_rows != c_cols))
	{
	  gripe_nonconformant ();
	  return retval;
	}
  
// Dimensions look o.k., let's solve the problem.

    retval.resize (nargout ? nargout : 1);

    if (arga.is_complex_type () || argb.is_complex_type ()
	|| argc.is_complex_type ())
      {

// Do everything in complex arithmetic;

	ComplexMatrix ca = arga.complex_matrix_value ();
	ComplexMatrix cb = argb.complex_matrix_value ();
	ComplexMatrix cc = argc.complex_matrix_value ();
  
// Compute Schur decompositions

	ComplexSCHUR as (ca, "U");
	ComplexSCHUR bs (cb, "U");
  
// Transform cc to new coordinates.

	ComplexMatrix ua = as.unitary_matrix ();
	ComplexMatrix sch_a = as.schur_matrix ();
	ComplexMatrix ub = bs.unitary_matrix ();
	ComplexMatrix sch_b = bs.schur_matrix ();
  
	ComplexMatrix cx = ua.hermitian () * cc * ub;
  
// Solve the sylvester equation, back-transform, and return the solution.
  
	double scale;
	int info;
	int one = 1;
  
	F77_FCN (ztrsyl) ("N", "N", &one, &a_rows, &b_rows,
			  sch_a.fortran_vec (), &a_rows,
			  sch_b.fortran_vec (), &b_rows,
			  cx.fortran_vec (), &a_rows, &scale, &info,
			  1L, 1L);

	cx = -ua * cx * ub.hermitian ();
  
	retval(0) = cx;
      }
    else
      {

// Do everything in real arithmetic;

	Matrix ca = arga.to_matrix ();
	Matrix cb = argb.to_matrix ();
	Matrix cc = argc.to_matrix ();
  
// Compute Schur decompositions.

	SCHUR as (ca, "U");
	SCHUR bs (cb, "U");
  
// Transform cc to new coordinates.

	Matrix ua = as.unitary_matrix ();
	Matrix sch_a = as.schur_matrix ();
	Matrix ub = bs.unitary_matrix ();
	Matrix sch_b = bs.schur_matrix ();
  
	Matrix cx = ua.transpose () * cc * ub;
  
// Solve the sylvester equation, back-transform, and return the solution.
  
	double scale;
	int info;
	int one = 1;

	F77_FCN (dtrsyl) ("N", "N", &one, &a_rows, &b_rows,
			  sch_a.fortran_vec (), &a_rows, 
			  sch_b.fortran_vec (), &b_rows,
			  cx.fortran_vec (), &a_rows, &scale, &info,
			  1L, 1L);

	if (info)
	  error ("syl: trouble in dtrsyl info = %d", info);
  
	cx = -ua*cx*ub.transpose ();
  
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
