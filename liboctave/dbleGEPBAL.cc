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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cmath>

#include <string>

#include "dbleGEPBAL.h"
#include "f77-fcn.h"

extern "C"
{
  int F77_FCN (dgebak, DGEBAK) (const char*, const char*, const int&,
				const int&, const int&, double*,
				const int&, double*, const int&, int&,
				long, long);

  int F77_FCN (reduce, REDUCE) (const int&, const int&, double*,
				const int&, double*, int&, int&,
				double*, double*);

  int F77_FCN (scaleg, SCALEG) (const int&, const int&, double*,
				const int&, double*, const int&,
				const int&, double*, double*,
				double*);

  int F77_FCN (gradeq, GRADEQ) (const int&, const int&, double*,
				const int&, double*, int&, int&,
				double*, double*);
}

int
GEPBALANCE::init (const Matrix& a, const Matrix& b, const string& balance_job)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  int b_nr = b.rows ();

  if (a_nr != a_nc || a_nr != b_nr || b_nr != b.cols ())
    {
      (*current_liboctave_error_handler)
	("GEPBALANCE requires square matrices of the same size");
      return -1;
    }

  int n = a_nc;

  int info;
  int ilo;
  int ihi;

  Array<double> cscale (n);
  double *pcscale = cscale.fortran_vec ();

  Matrix wk (n, 6, 0.0);
  double *pwk = wk.fortran_vec ();

  // Back out the permutations:
  //
  // cscale contains the exponents of the column scaling factors in its 
  // ilo through ihi locations and the reducing column permutations in 
  // its first ilo-1 and its ihi+1 through n locations.
  //
  // cperm contains the column permutations applied in grading the a and b 
  // submatrices in its ilo through ihi locations.
  //
  // wk contains the exponents of the row scaling factors in its ilo 
  // through ihi locations, the reducing row permutations in its first 
  // ilo-1 and its ihi+1 through n locations, and the row permutations
  // applied in grading the a and b submatrices in its n+ilo through 
  // n+ihi locations.
  
  // Copy matrices into local structure.

  balanced_a_mat = a;
  balanced_b_mat = b;

  double *p_balanced_a_mat = balanced_a_mat.fortran_vec ();
  double *p_balanced_b_mat = balanced_b_mat.fortran_vec ();

  // Check for permutation option.

  char job = balance_job[0];

  if (job == 'P' || job == 'B')
    {
      F77_XFCN (reduce, REDUCE, (n, n, p_balanced_a_mat, n,
				 p_balanced_b_mat, ilo, ihi,
				 pcscale, pwk));
    }
  else
    {
      // Set up for scaling later.

      ilo = 1;
      ihi = n;
    }

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in reduce");
  else
    {
      Array<double> cperm (n);
      double *pcperm = cperm.fortran_vec ();

      // Check for scaling option.

      if ((job == 'S' || job == 'B') && ilo != ihi)
	{
	  F77_XFCN (scaleg, SCALEG, (n, n, p_balanced_a_mat, n,
				     p_balanced_b_mat, ilo, ihi,
				     pcscale, pcperm, pwk));
	}
      else
	{
	  // Set scaling data to 0's.

	  for (int i = ilo-1; i < ihi; i++)
	    {
	      cscale.elem (i) = 0.0;
	      wk.elem (i, 0) = 0.0;
	    }
	}

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in scaleg");
      else
	{
	  // Scaleg returns exponents, not values, so...

	  for (int i = ilo-1; i < ihi; i++)
	    {
	      cscale.elem (i) = pow (2.0, cscale.elem (i));
	      wk.elem (i, 0) = pow (2.0, -wk.elem (i, 0));
	    }

	  // Initialize balancing matrices to identity.

	  left_balancing_mat = Matrix (n, n, 0.0);
	  for (int i = 0; i < n; i++)
	    left_balancing_mat (i, i) = 1.0;

	  right_balancing_mat = left_balancing_mat;

	  double *p_right_balancing_mat = right_balancing_mat.fortran_vec ();
	  double *p_left_balancing_mat = left_balancing_mat.fortran_vec ();

	  // Column permutations/scaling.

	  char side = 'R';

	  F77_XFCN (dgebak, DGEBAK, (&job, &side, n, ilo, ihi, pcscale,
				     n, p_right_balancing_mat, n, info,
				     1L, 1L));
    
	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgebak");
	  else
	    {
	      // Row permutations/scaling.

	      side = 'L';

	      F77_XFCN (dgebak, DGEBAK, (&job, &side, n, ilo, ihi, pwk,
					 n, p_left_balancing_mat, n,
					 info, 1L, 1L));

#if 0
	      // XXX FIXME XXX --- these four lines need to be added and
	      // debugged.  GEPBALANCE::init will work without them, though, so
	      // here they are.

	      if ((job == 'P' || job == 'B') && ilo != ihi)
		{
		  F77_XFCN (gradeq, GRADEQ, (n, n, p_balanced_a_mat, n,
					     p_balanced_b_mat, ilo, ihi,
					     pcperm, pwk));
		}
#endif

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in dgebak");
	      else
		{
		  // Transpose for aa = cc*a*dd convention...

		  left_balancing_mat = left_balancing_mat.transpose ();
		}
	    }
	}
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
