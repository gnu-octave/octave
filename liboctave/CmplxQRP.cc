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

#include <cassert>

#include "CmplxQRP.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (zgeqpf, ZGEQPF) (const int&, const int&, Complex*,
				const int&, int*, Complex*, Complex*,
				double*, int&);

  int F77_FCN (zungqr, ZUNGQR) (const int&, const int&, const int&,
				Complex*, const int&, Complex*,
				Complex*, const int&, int&);
}

// It would be best to share some of this code with ComplexQR class...

ComplexQRP::ComplexQRP (const ComplexMatrix& a, QR::type qr_type)
{
  assert (qr_type != QR::raw);

  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler)
	("ComplexQR must have non-empty matrix");
      return;
    }

  int min_mn = m < n ? m : n;
  Array<Complex> tau (min_mn);
  Complex *ptau = tau.fortran_vec ();

  int lwork = n;
  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  int info = 0;

  ComplexMatrix A_fact;
  if (m > n)
    {
      A_fact.resize (m, m);
      A_fact.insert (a, 0, 0);
    }
  else
    A_fact = a;

  Complex *tmp_data = A_fact.fortran_vec ();

  Array<double> rwork (2*n);
  double *prwork = rwork.fortran_vec ();

  Array<int> jpvt (n, 0);
  int *pjpvt = jpvt.fortran_vec ();

  // Code to enforce a certain permutation could go here...

  F77_XFCN (zgeqpf, ZGEQPF, (m, n, tmp_data, m, pjpvt, ptau, pwork,
			     prwork, info));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeqpf");
  else
    {
      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      if (qr_type == QR::economy && m > n)
	{
	  p.resize (1, n, 0.0);
	  for (int j = 0; j < n; j++)
	    p.elem (0, j) = jpvt.elem (j);
	}
      else
	{
	  p.resize (n, n, 0.0);
	  for (int j = 0; j < n; j++)
	    p.elem (jpvt.elem (j) - 1, j) = 1.0;
	}

      volatile int n2;

      if (qr_type == QR::economy && m > n)
	{
	  n2 = n;
	  r.resize (n, n, 0.0);
	}
      else
	{
	  n2 = m;
	  r.resize (m, n, 0.0);
	}

      for (int j = 0; j < n; j++)
	{
	  int limit = j < min_mn-1 ? j : min_mn-1;
	  for (int i = 0; i <= limit; i++)
	    r.elem (i, j) = A_fact.elem (i, j);
	}

      lwork = 32*m;
      work.resize (lwork);
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zungqr, ZUNGQR, (m, m, min_mn, tmp_data, m, ptau,
				 pwork, lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zungqr");
      else
	{
	  q = ComplexMatrix (tmp_data, m, m);
	  q.resize (m, n2);
	}
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
