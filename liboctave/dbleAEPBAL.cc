//                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
#include <config.h>
#endif

#include "dbleAEPBAL.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (dgebal) (const char*, const int&, double*,
                        const int&, int&, int&, double*,
                        int&, long, long);

  int F77_FCN (dgebak) (const char*, const char*, const int&, const int&,
			const int&, double*, const int&, double*, const int&,
			int&, long, long);
}

int
AEPBALANCE::init (const Matrix& a, const char *balance_job)
{
  int a_nc = a.cols ();
  if (a.rows () != a_nc)
    {
      (*current_liboctave_error_handler) ("AEPBALANCE requires square matrix");
      return -1;
    }

  int n = a_nc;

// Parameters for balance call.

  int info;
  int ilo;
  int ihi;
  double *scale = new double [n];

// Copy matrix into local structure.

  balanced_mat = a;

  F77_FCN (dgebal) (balance_job, n, balanced_mat.fortran_vec (), 
		    n, ilo, ihi, scale, info, 1L, 1L);

// Initialize balancing matrix to identity.

  balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    balancing_mat.elem (i ,i) = 1.0;

  F77_FCN (dgebak) (balance_job, "R", n, ilo, ihi, scale, n, 
		    balancing_mat.fortran_vec (), n, info, 1L, 1L);

  delete [] scale;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
