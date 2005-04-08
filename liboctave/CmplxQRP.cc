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

#include "CmplxQRP.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zgeqpf, ZGEQPF) (const octave_idx_type&, const octave_idx_type&, Complex*,
			     const octave_idx_type&, octave_idx_type*, Complex*, Complex*,
			     double*, octave_idx_type&);

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

  octave_idx_type lwork = 3*n > 32*m ? 3*n : 32*m;
  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  octave_idx_type info = 0;

  ComplexMatrix A_fact = a;
  if (m > n && qr_type != QR::economy)
    A_fact.resize (m, m, 0.0);

  Complex *tmp_data = A_fact.fortran_vec ();

  Array<double> rwork (2*n);
  double *prwork = rwork.fortran_vec ();

  Array<octave_idx_type> jpvt (n, 0);
  octave_idx_type *pjpvt = jpvt.fortran_vec ();

  // Code to enforce a certain permutation could go here...

  F77_XFCN (zgeqpf, ZGEQPF, (m, n, tmp_data, m, pjpvt, ptau, pwork,
			     prwork, info));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeqpf");
  else
    {
      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      if (qr_type == QR::economy)
	{
	  p.resize (1, n, 0.0);
	  for (octave_idx_type j = 0; j < n; j++)
	    p.elem (0, j) = jpvt.elem (j);
	}
      else
	{
	  p.resize (n, n, 0.0);
	  for (octave_idx_type j = 0; j < n; j++)
	    p.elem (jpvt.elem (j) - 1, j) = 1.0;
	}

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

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zungqr");
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
