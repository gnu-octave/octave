// tc-balance.cc                                           -*- C++ -*-
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
#include "dbleAEPBAL.h"
#include "CmplxAEPBAL.h"
#include "dbleAEPBAL.h"
#include "CmplxAEPBAL.h"
#include "dbleGEPBAL.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "f-balance.h"

#ifdef WITH_DLD
tree_constant *
builtin_balance_2 (const tree_constant *args, int nargin, int nargout)
{
  return balance (args, nargin, nargout);
}
#endif

tree_constant *
balance (const tree_constant *args, int nargin, int nargout)
{
  char *bal_job;
  int my_nargin;		// # args w/o optional string arg
  tree_constant *retval = NULL_TREE_CONST;

  // determine if balancing option is listed
  // set my_nargin to the number of matrix inputs
  if (args[nargin-1].const_type () == tree_constant_rep::string_constant ){
    bal_job = args[nargin-1].string_value ();
    my_nargin = nargin-2;
  }
  else
  {
    bal_job = "B";
    my_nargin = nargin-1;
  }

  tree_constant arg = args[1].make_numeric ();
  int a_nr = arg.rows ();
  int a_nc = arg.columns ();

// Check argument 1 dimensions.

  if (a_nr == 0 || a_nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag != 0)
	{
	  if (flag < 0) warning ("balance: argument is empty matrix");
	  Matrix m;
	  retval = new tree_constant [3];
	  retval[0] = tree_constant (m);
	  retval[1] = tree_constant (m);
	}
      else
	error ("balance: empty matrix is invalid as argument");

      return retval;
    }

  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("balance");
      return retval;
    }

// Extract argument 1 parameter for both AEP and GEP.

  Matrix aa;
  ComplexMatrix caa;
  if (arg.is_complex_type ())
    {
      if (arg.is_matrix_type ())
	caa=arg.complex_matrix_value ();
      else
	{
	  caa.resize (1, 1);
	  caa.elem (0, 0) = arg.complex_value ();
	}
    }
  else
    {
      if (arg.is_matrix_type ())
	aa = arg.matrix_value ();
      else
	{
	  double d = arg.double_value ();
	  aa.resize (1, 1);
	  aa.elem (0, 0) = d;
	}
    }

// Treat AEP/ GEP cases.

  switch (my_nargin)
    {
    case 1:

// Algebraic eigenvalue problem.

      retval = new tree_constant[nargout+1];
      if (arg.is_complex_type ())
	{
	  ComplexAEPBALANCE result (caa, bal_job);

	  if (nargout == 1)
	    retval[0] = tree_constant(result.balanced_matrix ());
	  else
	    {
	      retval[0] = tree_constant (result.balancing_matrix ());
	      retval[1] = tree_constant (result.balanced_matrix ());
	    }
	}
      else
	{
	  AEPBALANCE result (aa, bal_job);

	  if (nargout == 1)
	    retval[0] = tree_constant (result.balanced_matrix ());
	  else
	    {
	      retval[0] = tree_constant (result.balancing_matrix ());
	      retval[1] = tree_constant (result.balanced_matrix ());
	    }
	}
      break;
    case 2:

// Generalized eigenvalue problem.

      {
	retval = new tree_constant[nargout+1];
      
// 1st we have to check argument 2 dimensions and type...

	tree_constant brg = args[2].make_numeric ();
	int b_nr = brg.rows ();
	int b_nc = brg.columns ();
      
// Check argument 2 dimensions -- must match arg 1.

	if ((b_nr != b_nc) || (b_nr != a_nr))
	  {
	    gripe_nonconformant ();
	    return retval;
	  }
      
// Now, extract the second matrix...
// Extract argument 1 parameter for both AEP and GEP.

	Matrix bb;
	ComplexMatrix cbb;
	if (brg.is_complex_type ())
	  {
	    if (brg.is_matrix_type ())
	      cbb = brg.complex_matrix_value ();
	    else
	      {
		cbb.resize (1, 1);
		cbb.elem (0, 0) = brg.complex_value ();
	      }
	  }
	else
	  {
	    if (brg.is_matrix_type ()) bb = brg.matrix_value ();
	    else
	      {
		double d = brg.double_value ();
		bb.resize (1, 1);
		bb.elem (0, 0) = d;
	      }
	  }
  
// Both matrices loaded, now let's check what kind of arithmetic:

	if (arg.is_complex_type () || brg.is_complex_type ())
	  {
	    if (arg.is_real_type ())
	      caa = aa;
	    else if (brg.is_real_type ())
	      cbb = bb;

// Compute magnitudes of elements for balancing purposes.
// Surely there's a function I can call someplace!

	    for (int i = 0; i < a_nr; i++)
	      for (int j = 0; j < a_nr; j++)
		{
		  aa.elem (i, j) = abs (caa.elem (i, j));
		  bb.elem (i, j) = abs (cbb.elem (i, j));
		}
	  }

	GEPBALANCE result (aa, bb, bal_job);

	if (arg.is_complex_type () || brg.is_complex_type ())
	  {
	    caa = result.left_balancing_matrix () * caa
	      * result.right_balancing_matrix ();

	    cbb = result.left_balancing_matrix () * cbb
	      * result.right_balancing_matrix ();

	    switch (nargout)
	      {
	      case 1:
		warning ("balance: should use two output arguments");
		retval[0] = tree_constant (caa);
		break;
	      case 2:
		retval[0] = tree_constant (caa);
		retval[1] = tree_constant (cbb);
		break;
	      case 4:
		retval[0] = tree_constant (result.left_balancing_matrix ());
		retval[1] = tree_constant (result.right_balancing_matrix ());
		retval[2] = tree_constant (caa);
		retval[3] = tree_constant (cbb);
		break;
	      default:
		error ("balance: illegal number of output arguments");
		break;
	      }
	  }
	else
	  {
	    switch (nargout)
	      {
	      case 2:
		retval[0] = tree_constant (result.balanced_a_matrix ());
		retval[1] = tree_constant (result.balanced_b_matrix ());
		break;
	      case 4:
		retval[0] = tree_constant (result.left_balancing_matrix ());
		retval[1] = tree_constant (result.right_balancing_matrix ());
		retval[2] = tree_constant (result.balanced_a_matrix ());
		retval[3] = tree_constant (result.balanced_b_matrix ());
		break;
	      default:
		error ("balance: illegal number of output arguments");
		break;
	      }
	  }
      }
      break;
    default:
      error ("balance requires one (AEP) or two (GEP) numeric arguments");
      break;
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
