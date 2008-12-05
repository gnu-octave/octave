/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2007
              John W. Eaton

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

#include <cassert>

#include "CmplxQRP.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zgeqp3, ZGEQP3) (const octave_idx_type&, const octave_idx_type&, Complex*,
			     const octave_idx_type&, octave_idx_type*, Complex*, Complex*,
			     const octave_idx_type&, double*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (zungqr, ZUNGQR) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
			     Complex*, const octave_idx_type&, Complex*,
			     Complex*, const octave_idx_type&, octave_idx_type&);
}

// It would be best to share some of this code with ComplexQR class...

ComplexQRP::ComplexQRP (const ComplexMatrix& a, QR::type qr_type)
  : ComplexQR (), p ()
{
  init (a, qr_type);
}

void
ComplexQRP::init (const ComplexMatrix& a, QR::type qr_type)
{
  assert (qr_type != QR::raw);

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler)
	("ComplexQR must have non-empty matrix");
      return;
    }

  octave_idx_type min_mn = m < n ? m : n;
  Array<Complex> tau (min_mn);
  Complex *ptau = tau.fortran_vec ();

  octave_idx_type info = 0;

  ComplexMatrix A_fact = a;
  if (m > n && qr_type != QR::economy)
    A_fact.resize (m, m, 0.0);

  Complex *tmp_data = A_fact.fortran_vec ();

  Array<double> rwork (2*n);
  double *prwork = rwork.fortran_vec ();

  MArray<octave_idx_type> jpvt (n, 0);
  octave_idx_type *pjpvt = jpvt.fortran_vec ();

  Complex rlwork = 0;
  // Workspace query...
  F77_XFCN (zgeqp3, ZGEQP3, (m, n, tmp_data, m, pjpvt, ptau, &rlwork,
			     -1, prwork, info));

  octave_idx_type lwork = rlwork.real ();
  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  // Code to enforce a certain permutation could go here...

  F77_XFCN (zgeqp3, ZGEQP3, (m, n, tmp_data, m, pjpvt, ptau, pwork,
			     lwork, prwork, info));

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= 1;
  p = PermMatrix (jpvt, true);

  octave_idx_type n2 = (qr_type == QR::economy) ? min_mn : m;

  if (qr_type == QR::economy && m > n)
    r.resize (n, n, 0.0);
  else
    r.resize (m, n, 0.0);

  for (octave_idx_type j = 0; j < n; j++)
    {
      octave_idx_type limit = j < min_mn-1 ? j : min_mn-1;
      for (octave_idx_type i = 0; i <= limit; i++)
	r.elem (i, j) = A_fact.elem (i, j);
    }

  F77_XFCN (zungqr, ZUNGQR, (m, n2, min_mn, tmp_data, m, ptau,
			     pwork, lwork, info));

  q = A_fact;
  q.resize (m, n2);
}

ColumnVector
ComplexQRP::Pvec (void) const
{
  Array<double> pa (p.pvec ());
  ColumnVector pv (MArray<double> (pa) + 1.0);
  return pv;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
