// tc-syl.cc                                           -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include "Matrix.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"

int F77_FCN (dtrsyl) (const char*, const char*, const int*, const
		      int*, const int*, const double*, const int*,
		      const double*, const int*, const double*, const
		      int*, double*, int*, long, long);
 
int F77_FCN (ztrsyl) (const char*, const char*, const int*, const
		      int*, const int*, const Complex*, const int*,
		      const Complex*, const int*, const Complex*,
		      const int*, double*, int*, long, long);

// Local function: check for empty matrix arguments.  Probably should make 
// this available elsewhere, since tc-xxx functions do this a lot.

static inline int
empty_arg (tree_constant& arg)
{
  return (arg.rows () == 0 || arg.columns () == 0);
}


// Local function: construct return vector of empty matrices.  Return
// empty matrices and/or gripe when appropriate.  Probably should make
// this available elsewhere, since tc-xxx functions do this a lot.

tree_constant *
empty_tree (int nargout, char* fcn_name)
{
  tree_constant *retval = NULL_TREE_CONST;

// Got an empty argument, check if should gripe/return empty values.

  int flag = user_pref.propagate_empty_matrices;
  if (flag != 0)
    {
      if (flag < 0)
	gripe_empty_arg (fcn_name, 0);

      Matrix m;
      retval = new tree_constant [nargout+1];
      for (int i = 0; i < nargout; i++)
	retval[i] = tree_constant (m);
    }
  else
    gripe_empty_arg (fcn_name, 1);

  return retval;
}

// Return value of tree_constant argument as ComplexMatrix.

ComplexMatrix
ComplexMatrixLoad (tree_constant& arg)
{
  ComplexMatrix retval;

// Set argument size for scalar (for later).

  switch (arg.const_type ())
    {
    case tree_constant_rep::scalar_constant:
      retval.resize (1, 1);
      {
        double real_val = arg.double_value ();
        retval.elem (0, 0) = real_val;
      }
      break;
    case tree_constant_rep::complex_scalar_constant:
      retval.resize (1, 1);
      retval.elem (0, 0) = arg.complex_value ();
      break;
    case tree_constant_rep::matrix_constant:
      {
        Matrix tmp = arg.matrix_value ();
        retval = tmp;
      }
      break;
    case tree_constant_rep::complex_matrix_constant:
      retval = arg.complex_matrix_value ();
      break;
    default:
      panic_impossible ();
      break;
    }
  return retval;
}

#ifdef WITH_DLD
tree_constant *
builtin_syl_2 (tree_constant *args, int nargin, int nargout)
{
  return syl (args, nargin, nargout);
}
#endif

tree_constant *
syl (tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  tree_constant arga = args[1].make_numeric ();
  tree_constant argb = args[2].make_numeric ();
  tree_constant argc = args[3].make_numeric ();

  if (empty_arg (arga) || empty_arg (argb) || empty_arg (argc))
    retval = empty_tree (nargout, "syl");
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

    retval = new tree_constant[nargout+1];

    if (arga.is_complex_type () || argb.is_complex_type ()
	|| argc.is_complex_type ())
      {

// Do everything in complex arithmetic;

	ComplexMatrix ca = ComplexMatrixLoad (arga);
	ComplexMatrix cb = ComplexMatrixLoad (argb);
	ComplexMatrix cc = ComplexMatrixLoad (argc);
  
// Compute Schur decompositions

	ComplexSCHUR as (ca, "U");
	ComplexSCHUR bs (cb, "U");
  
// Transform cc to new coordinates.

	ComplexMatrix ua = as.unitary_matrix ();
	ComplexMatrix sch_a = as.schur_matrix ();
	ComplexMatrix ub = bs.unitary_matrix ();
	ComplexMatrix sch_b = bs.schur_matrix ();
  
	cx = ua.hermitian () * cc * ub;
  
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
  
	retval[0] = tree_constant (cx);
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
  
	cx = ua.transpose () * cc * ub;
  
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
  
	retval[0] = tree_constant (cx);
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
