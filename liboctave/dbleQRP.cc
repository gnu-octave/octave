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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include "dbleQRP.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeqpf, DGEQPF) (const int&, const int&, double*,
			     const int&, int*, double*, double*, int&);

  F77_RET_T
  F77_FUNC (dorgqr, DORGQR) (const int&, const int&, const int&,
			     double*, const int&, double*, double*,
			     const int&, int&);
}

// It would be best to share some of this code with QR class...

QRP::QRP (const Matrix& a, QR::type qr_type)
  : QR (), p ()
{
  init (a, qr_type);
}

void
QRP::init (const Matrix& a, QR::type qr_type)
{
  assert (qr_type != QR::raw);

  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  int min_mn = m < n ? m : n;
  Array<double> tau (min_mn);
  double *ptau = tau.fortran_vec ();

  int lwork = 3*n > 32*m ? 3*n : 32*m;
  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  int info = 0;

  Matrix A_fact = a;
  if (m > n && qr_type != QR::economy)
    A_fact.resize (m, m, 0.0);

  double *tmp_data = A_fact.fortran_vec ();

  Array<int> jpvt (n, 0);
  int *pjpvt = jpvt.fortran_vec ();

  // Code to enforce a certain permutation could go here...

  F77_XFCN (dgeqpf, DGEQPF, (m, n, tmp_data, m, pjpvt, ptau, pwork, info));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dgeqpf");
  else
    {
      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      if (qr_type == QR::economy)
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

      int n2 = (qr_type == QR::economy) ? min_mn : m;

      if (qr_type == QR::economy && m > n)
	r.resize (n, n, 0.0);
      else
	r.resize (m, n, 0.0);

      for (int j = 0; j < n; j++)
	{
	  int limit = j < min_mn-1 ? j : min_mn-1;
	  for (int i = 0; i <= limit; i++)
	    r.elem (i, j) = A_fact.elem (i, j);
	}

      F77_XFCN (dorgqr, DORGQR, (m, n2, min_mn, tmp_data, m, ptau,
				 pwork, lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dorgqr");
      else
	{
	  q = A_fact;
	  q.resize (m, n2);
	}
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
