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

#include <assert.h>

#include "CmplxQRP.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (zgeqpf) (const int&, const int&, Complex*, const int&,
			int*, Complex*, Complex*, double*, int&);

  int F77_FCN (zungqr) (const int&, const int&, const int&, Complex*,
			const int&, Complex*, Complex*, const int&, int&);
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

  Complex *tmp_data;
  int min_mn = m < n ? m : n;
  Complex *tau = new Complex[min_mn];
  int lwork = n;
  Complex *work = new Complex[lwork];
  int info = 0;

  if (m > n)
    {
      tmp_data = new Complex [m*m];
      copy (tmp_data, a.data (), a.length ());
    }
  else
    tmp_data = dup (a.data (), a.length ());


  work = new Complex[n];
  double *rwork = new double[2*n];
  int *jpvt = new int[n];

// Clear Pivot vector (code to enforce a certain permutation would go
// here...)

  for (int i = 0; i < n; i++)
    jpvt[i] = 0;      

  F77_FCN (zgeqpf) (m, n, tmp_data, m, jpvt, tau, work, rwork, info);

// Form Permutation matrix (if economy is requested, return the
// indices only!)

  if (qr_type == QR::economy && m > n)
    {
      p.resize (1, n, 0.0);
      for (int j = 0; j < n; j++)
	p.elem (0, j) = jpvt[j];
    }
  else
    {
      p.resize (n, n, 0.0);
      for (int j = 0; j < n; j++)
	p.elem (jpvt[j]-1, j) = 1.0;
    }

  delete [] jpvt;
  delete [] work;

  int n2;
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
	r.elem (i, j) = tmp_data[m*j+i];
    }

  lwork = 32*m;
  work = new Complex[lwork];

  F77_FCN (zungqr) (m, m, min_mn, tmp_data, m, tau, work, lwork, info);

  q = ComplexMatrix (tmp_data, m, m);
  q.resize (m, n2);

  delete [] tau;
  delete [] work;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
