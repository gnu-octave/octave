//                                        -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#include <string>

#include "CmplxAEPBAL.h"
#include "dMatrix.h"
#include "f77-fcn.h"

extern "C"
{
  int F77_FCN (zgebal, ZGEBAL) (const char*, const int&, Complex*,
				const int&, int&, int&, double*, int&,
				long, long);
 
  int F77_FCN (zgebak, ZGEBAK) (const char*, const char*, const int&,
				const int&, const int&, double*, const
				int&, Complex*, const int&, int&,
				long, long);
}

int
ComplexAEPBALANCE::init (const ComplexMatrix& a, const string& balance_job)
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

  char bal_job = balance_job[0];

  F77_FCN (zgebal, ZGEBAL) (&bal_job, n,
			    balanced_mat.fortran_vec (), n, ilo, ihi,
			    scale, info, 1L, 1L);

  // Initialize balancing matrix to identity.

  balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    balancing_mat (i, i) = 1.0;

  F77_FCN (zgebak, ZGEBAK) (&bal_job, "R", n, ilo, ihi, scale, n, 
			    balancing_mat.fortran_vec (), n, info, 1L,
			    1L);

  delete [] scale;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
