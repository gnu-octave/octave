/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dbleQR.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeqrf, DGEQRF) (const octave_idx_type&, const octave_idx_type&, double*, const octave_idx_type&,
			     double*, double*, const octave_idx_type&, octave_idx_type&); 

  F77_RET_T
  F77_FUNC (dorgqr, DORGQR) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, double*,
			     const octave_idx_type&, double*, double*, const octave_idx_type&, octave_idx_type&);
}

QR::QR (const Matrix& a, QR::type qr_type)
  : q (), r ()
{
  init (a, qr_type);
}

void
QR::init (const Matrix& a, QR::type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  octave_idx_type min_mn = m < n ? m : n;
  Array<double> tau (min_mn);
  double *ptau = tau.fortran_vec ();

  octave_idx_type lwork = 32*n;
  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  octave_idx_type info = 0;

  Matrix A_fact = a;
  if (m > n && qr_type != QR::economy)
      A_fact.resize (m, m, 0.0);

  double *tmp_data = A_fact.fortran_vec ();

  F77_XFCN (dgeqrf, DGEQRF, (m, n, tmp_data, m, ptau, pwork, lwork, info));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dgeqrf");
  else
    {
      if (qr_type == QR::raw)
	{
	  for (octave_idx_type j = 0; j < min_mn; j++)
	    {
	      octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
	      for (octave_idx_type i = limit + 1; i < m; i++)
		A_fact.elem (i, j) *= tau.elem (j);
	    }

	  r = A_fact;

	  if (m > n)
	    r.resize (m, n);
	}
      else
	{
	  octave_idx_type n2 = (qr_type == QR::economy) ? min_mn : m;

	  if (qr_type == QR::economy && m > n)
	    r.resize (n, n, 0.0);
	  else
	    r.resize (m, n, 0.0);

	  for (octave_idx_type j = 0; j < n; j++)
	    {
	      octave_idx_type limit = j < min_mn-1 ? j : min_mn-1;
	      for (octave_idx_type i = 0; i <= limit; i++)
		r.elem (i, j) = tmp_data[m*j+i];
	    }

	  lwork = 32 * n2;
	  work.resize (lwork);
	  double *pwork2 = work.fortran_vec ();

	  F77_XFCN (dorgqr, DORGQR, (m, n2, min_mn, tmp_data, m, ptau,
				     pwork2, lwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dorgqr");
	  else
	    {
	      q = A_fact;
	      q.resize (m, n2);
	    }
	}
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
