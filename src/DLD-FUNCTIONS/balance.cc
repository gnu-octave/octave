/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include <string>

#include "CmplxAEPBAL.h"
#include "CmplxAEPBAL.h"
#include "dbleAEPBAL.h"
#include "dbleAEPBAL.h"
#include "dbleGEPBAL.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (balance, args, nargout,
  "AA = balance (A [, OPT]) or [[DD,] AA] =  balance (A [, OPT])\n\
\n\
generalized eigenvalue problem:\n\
\n\
  [cc, dd, aa, bb] = balance (a, b [, opt])\n\
\n\
where OPT is an optional single character argument as follows: \n\
\n\
  N: no balancing; arguments copied, transformation(s) set to identity\n\
  P: permute argument(s) to isolate eigenvalues where possible\n\
  S: scale to improve accuracy of computed eigenvalues\n\
  B: (default) permute and scale, in that order.  Rows/columns\n\
     of a (and b) that are isolated by permutation are not scaled\n\
\n\
[DD, AA] = balance (A, OPT) returns aa = inv(dd)*a*dd,\n\
\n\
[CC, DD, AA, BB] = balance (A, B, OPT) returns AA (BB) = CC*A*DD (CC*B*DD)")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout < 0 || nargout > 4)
    {
      print_usage ("balance");
      return retval;
    }

  string bal_job;
  int my_nargin;		// # args w/o optional string arg

  // Determine if balancing option is listed.  Set my_nargin to the
  // number of matrix inputs.

  if (args(nargin-1).is_string ())
    {
      bal_job = args(nargin-1).string_value ();
      my_nargin = nargin-1;
    }
  else
    {
      bal_job = "B";
      my_nargin = nargin;
    }

  octave_value arg_a = args(0);

  int a_nr = arg_a.rows ();
  int a_nc = arg_a.columns ();

  // Check argument 1 dimensions.

  int arg_is_empty = empty_arg ("balance", a_nr, a_nc);

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("balance");
      return retval;
    }

  // Extract argument 1 parameter for both AEP and GEP.

  Matrix aa;
  ComplexMatrix caa;
  if (arg_a.is_complex_type ())
    caa = arg_a.complex_matrix_value ();
  else
    aa = arg_a.matrix_value ();

  if (error_state)
    return retval;

  // Treat AEP/GEP cases.

  switch (my_nargin)
    {
    case 1:
      
      // Algebraic eigenvalue problem.

      if (arg_a.is_complex_type ())
	{
	  ComplexAEPBALANCE result (caa, bal_job);

	  if (nargout == 0 || nargout == 1)
	    retval(0) = result.balanced_matrix ();
	  else
	    {
	      retval(1) = result.balanced_matrix ();
	      retval(0) = result.balancing_matrix ();
	    }
	}
      else
	{
	  AEPBALANCE result (aa, bal_job);

	  if (nargout == 0 || nargout == 1)
	    retval(0) = result.balanced_matrix ();
	  else
	    {
	      retval(1) = result.balanced_matrix ();
	      retval(0) = result.balancing_matrix ();
	    }
	}
      break;

    case 2:
      {
	// Generalized eigenvalue problem.

	// 1st we have to check argument 2 dimensions and type...

	octave_value arg_b = args(1);

	int b_nr = arg_b.rows ();
	int b_nc = arg_b.columns ();
      
	// Check argument 2 dimensions -- must match arg 1.

	if (b_nr != b_nc || b_nr != a_nr)
	  {
	    gripe_nonconformant ();
	    return retval;
	  }
      
	// Now, extract the second matrix...
	// Extract argument 1 parameter for both AEP and GEP.

	Matrix bb;
	ComplexMatrix cbb;
	if (arg_b.is_complex_type ())
	  cbb = arg_b.complex_matrix_value ();
	else
	  bb = arg_b.matrix_value ();

	if (error_state)
	  return retval;

	// Both matrices loaded, now let's check what kind of arithmetic:

	if (arg_a.is_complex_type () || arg_b.is_complex_type ())
	  {
	    if (arg_a.is_real_type ())
	      caa = aa;

	    if (arg_b.is_real_type ())
	      cbb = bb;

	    // Compute magnitudes of elements for balancing purposes.
	    // Surely there's a function I can call someplace!

	    for (int i = 0; i < a_nr; i++)
	      for (int j = 0; j < a_nc; j++)
		{
		  aa (i, j) = abs (caa (i, j));
		  bb (i, j) = abs (cbb (i, j));
		}
	  }

	GEPBALANCE result (aa, bb, bal_job);

	if (arg_a.is_complex_type () || arg_b.is_complex_type ())
	  {
	    caa = result.left_balancing_matrix () * caa
	      * result.right_balancing_matrix ();

	    cbb = result.left_balancing_matrix () * cbb
	      * result.right_balancing_matrix ();

	    switch (nargout)
	      {
	      case 0:
	      case 1:
		warning ("balance: should use two output arguments");
		retval(0) = caa;
		break;

	      case 2:
		retval(1) = cbb;
		retval(0) = caa;
		break;

	      case 4:
		retval(3) = cbb;
		retval(2) = caa;
		retval(1) = result.right_balancing_matrix ();
		retval(0) = result.left_balancing_matrix ();
		break;

	      default:
		error ("balance: invalid number of output arguments");
		break;
	      }
	  }
	else
	  {
	    switch (nargout)
	      {
	      case 0:
	      case 1:
		warning ("balance: should use two output arguments");
		retval(0) = result.balanced_a_matrix ();
		break;

	      case 2:
		retval(1) = result.balanced_b_matrix ();
		retval(0) = result.balanced_a_matrix ();
		break;

	      case 4:
		retval(3) = result.balanced_b_matrix ();
		retval(2) = result.balanced_a_matrix ();
		retval(1) = result.right_balancing_matrix ();
		retval(0) = result.left_balancing_matrix ();
		break;

	      default:
		error ("balance: invalid number of output arguments");
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
;;; End: ***
*/
