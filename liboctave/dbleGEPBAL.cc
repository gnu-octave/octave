//                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#include <math.h>

#include "dbleGEPBAL.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (dgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, double*, const int*,
			int*, long, long);

  int F77_FCN (reduce) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);

  int F77_FCN (scaleg) (const int*, const int*, double*,
	   	        const int*, double*,
			const int*, const int*, double*, double*, double*);

  int F77_FCN (gradeq) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);
}

int
GEPBALANCE::init (const Matrix& a, const Matrix& b, const char *balance_job)
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

// Parameters for balance call.

  int info;
  int ilo;
  int ihi;
  double *cscale = new double [n];
  double *cperm = new double [n];
  Matrix wk (n, 6, 0.0);

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

// Initialize balancing matrices to identity.

  left_balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    left_balancing_mat (i, i) = 1.0;

  right_balancing_mat = left_balancing_mat;

// Check for permutation option.

  if (*balance_job == 'P' || *balance_job == 'B')
    {
      F77_FCN (reduce) (&n, &n, balanced_a_mat.fortran_vec (),
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cscale, wk.fortran_vec ());
    }
  else
    {

// Set up for scaling later.

      ilo = 1;
      ihi = n;
    }

// Check for scaling option.

  if ((*balance_job == 'S' || *balance_job == 'B') && ilo != ihi)
    {
      F77_FCN (scaleg) (&n, &n, balanced_a_mat.fortran_vec (), 
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cscale, cperm, wk.fortran_vec ());
    }
  else
    {

// Set scaling data to 0's.

      for (int tmp = ilo-1; tmp < ihi; tmp++)
	{
	  cscale[tmp] = 0.0;
	  wk.elem (tmp, 0) = 0.0;
	}
    }

// Scaleg returns exponents, not values, so...

  for (int tmp = ilo-1; tmp < ihi; tmp++)
    {
      cscale[tmp] = pow (2.0, cscale[tmp]);
      wk.elem (tmp, 0) = pow (2.0, -wk.elem (tmp, 0));
    }

// Column permutations/scaling.

  F77_FCN (dgebak) (balance_job, "R", &n, &ilo, &ihi, cscale, &n, 
		    right_balancing_mat.fortran_vec (), &n, &info, 1L,
		    1L);
    
// Row permutations/scaling.

  F77_FCN (dgebak) (balance_job, "L", &n, &ilo, &ihi, &wk.elem (0, 0), &n, 
		    left_balancing_mat.fortran_vec (), &n, &info, 1L, 1L);

// XXX FIXME XXX --- these four lines need to be added and debugged.
// GEPBALANCE::init will work without them, though, so here they are.

#if 0
  if ((*balance_job == 'P' || *balance_job == 'B') && ilo != ihi)
    {
      F77_FCN (gradeq) (&n, &n, balanced_a_mat.fortran_vec (),
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cperm, &wk.elem (0, 1));
    }
#endif

// Transpose for aa = cc*a*dd convention...
  left_balancing_mat = left_balancing_mat.transpose ();

  delete [] cscale;
  delete [] cperm;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
