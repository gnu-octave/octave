// f-expm.cc                                           -*- C++ -*-
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

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "dMatrix.h"
#include "CMatrix.h"
#include "CColVector.h"
#include "dbleAEPBAL.h"
#include "CmplxAEPBAL.h"
#include "f77-uscore.h"

#include "tree-const.h"
#include "user-prefs.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun-dld.h"

extern "C"
{
  double F77_FCN (dlange) (const char*, const int*, const int*,
			   const double*, const int*, double*);

  double F77_FCN (zlange) (const char*, const int*, const int*,
			   const Complex*, const int*, double*);
}

DEFUN_DLD_BUILTIN ("expm", Fexpm, Sexpm, 2, 1,
  "expm (X): matrix exponential, e^A")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("expm");
      return retval;
    }

  tree_constant arg = args(0);

// Constants for matrix exponential calculation.

  static double padec [] =
    {
      5.0000000000000000e-1,
      1.1666666666666667e-1,
      1.6666666666666667e-2,
      1.6025641025641026e-3,
      1.0683760683760684e-4,
      4.8562548562548563e-6,
      1.3875013875013875e-7,
      1.9270852604185938e-9,
    };

  int nr = arg.rows ();
  int nc = arg.columns ();

  int arg_is_empty = empty_arg ("expm", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return Matrix ();

  if (nr != nc)
    {
      gripe_square_matrix_required ("expm");
      return retval;
    }

  int i, j;

  char* balance_job = "B";	// variables for balancing

  int sqpow;		// power for scaling and squaring
  double inf_norm;	// norm of preconditioned matrix
  int minus_one_j;	// used in computing pade approx

  if (arg.is_real_type ())
    {

// Compute the exponential.

      Matrix m = arg.matrix_value ();

      if (error_state)
	return retval;

      double trshift = 0;		// trace shift value

// Preconditioning step 1: trace normalization.

      for (i = 0; i < nc; i++)
	trshift += m.elem (i, i);
      trshift /= nc;
      for (i = 0; i < nc; i++)
	m.elem (i, i) -= trshift;

// Preconditioning step 2: balancing.

      AEPBALANCE mbal (m, balance_job);
      m = mbal.balanced_matrix ();
      Matrix d = mbal.balancing_matrix ();

// Preconditioning step 3: scaling.

      ColumnVector work(nc);
      inf_norm = F77_FCN (dlange) ("I", &nc, &nc,
				   m.fortran_vec (), &nc,
				   work.fortran_vec ());

      sqpow = (int) (1.0 + log (inf_norm) / log (2.0));

// Check whether we need to square at all.

      if (sqpow < 0)
	sqpow = 0;
      else
	{
	  for (inf_norm = 1.0, i = 0; i < sqpow; i++)
	    inf_norm *= 2.0;

	  m = m / inf_norm;
	}

// npp, dpp: pade' approx polynomial matrices.

      Matrix npp (nc, nc, 0.0);
      Matrix dpp = npp;

// now powers a^8 ... a^1.

      minus_one_j = -1;
      for (j = 7; j >= 0; j--)
	{
	  npp = m * npp + m * padec[j];
	  dpp = m * dpp + m * (minus_one_j * padec[j]);
	  minus_one_j *= -1;
	}
// Zero power.

      dpp = -dpp;
      for(j = 0; j < nc; j++)
	{
	  npp.elem (j, j) += 1.0;
	  dpp.elem (j, j) += 1.0;
	}

// Compute pade approximation = inverse (dpp) * npp.

      Matrix result = dpp.solve (npp);

// Reverse preconditioning step 3: repeated squaring.

      while (sqpow)
	{
	  result = result * result;
	  sqpow--;
	}

// Reverse preconditioning step 2: inverse balancing.

      result = result.transpose();
      d = d.transpose ();
      result = result * d;
      result = d.solve (result);
      result = result.transpose ();

// Reverse preconditioning step 1: fix trace normalization.

      result = result * exp (trshift);

      retval = result;
    }
  else if (arg.is_complex_type ())
    {
      ComplexMatrix m = arg.complex_matrix_value ();

      if (error_state)
	return retval;

      Complex trshift = 0.0;		// trace shift value

// Preconditioning step 1: trace normalization.

      for (i = 0; i < nc; i++)
	trshift += m.elem (i, i);
      trshift /= nc;
      for (i = 0; i < nc; i++)
	m.elem (i, i) -= trshift;

// Preconditioning step 2: eigenvalue balancing.

      ComplexAEPBALANCE mbal (m, balance_job);
      m = mbal.balanced_matrix ();
      ComplexMatrix d = mbal.balancing_matrix ();

// Preconditioning step 3: scaling.

      ColumnVector work (nc);
      inf_norm = F77_FCN (zlange) ("I", &nc, &nc, m.
				   fortran_vec (), &nc,
				   work.fortran_vec ());

      sqpow = (int) (1.0 + log (inf_norm) / log (2.0));

// Check whether we need to square at all.

      if (sqpow < 0)
	sqpow = 0;
      else
	{
	  for (inf_norm = 1.0, i = 0; i < sqpow; i++)
	    inf_norm *= 2.0;

	  m = m / inf_norm;
	}

// npp, dpp: pade' approx polynomial matrices.

      ComplexMatrix npp (nc, nc, 0.0);
      ComplexMatrix dpp = npp;

// Now powers a^8 ... a^1.

      minus_one_j = -1;
      for (j = 7; j >= 0; j--)
	{
	  npp = m * npp + m * padec[j];
	  dpp = m * dpp + m * (minus_one_j * padec[j]);
	  minus_one_j *= -1;
	}

// Zero power.

      dpp = -dpp;
      for (j = 0; j < nc; j++)
	{
	  npp.elem (j, j) += 1.0;
	  dpp.elem (j, j) += 1.0;
	}

// Compute pade approximation = inverse (dpp) * npp.

      ComplexMatrix result = dpp.solve (npp);
	
// Reverse preconditioning step 3: repeated squaring.

      while (sqpow)
	{
	  result = result * result;
	  sqpow--;
	}

// reverse preconditioning step 2: inverse balancing XXX FIXME XXX:
// should probably do this with lapack calls instead of a complete
// matrix inversion.

      result = result.transpose ();
      d = d.transpose ();
      result = result * d;
      result = d.solve (result);
      result = result.transpose ();

// Reverse preconditioning step 1: fix trace normalization.

      result = result * exp (trshift);

      retval = result;
    }
  else
    {
      gripe_wrong_type_arg ("expm", arg);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
