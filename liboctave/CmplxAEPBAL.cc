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
  int n = a.cols ();

  if (a.rows () != n)
    {
      (*current_liboctave_error_handler) ("AEPBALANCE requires square matrix");
      return -1;
    }

  int info;
  int ilo;
  int ihi;

  Array<double> scale (n);
  double *pscale = scale.fortran_vec ();

  balanced_mat = a;
  Complex *p_balanced_mat = balanced_mat.fortran_vec ();

  char job = balance_job[0];

  F77_XFCN (zgebal, ZGEBAL, (&job, n, p_balanced_mat, n, ilo, ihi,
			     pscale, info, 1L, 1L));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgebal");
  else
    {
      balancing_mat = Matrix (n, n, 0.0);
      for (int i = 0; i < n; i++)
	balancing_mat.elem (i, i) = 1.0;

      Complex *p_balancing_mat = balancing_mat.fortran_vec ();

      char side = 'R';

      F77_XFCN (zgebak, ZGEBAK, (&job, &side, n, ilo, ihi, pscale, n,
				 p_balancing_mat, n, info, 1L, 1L));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgebak");
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
