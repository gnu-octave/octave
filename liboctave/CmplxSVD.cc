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

#include "CmplxSVD.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (zgesvd, ZGESVD) (const char*, const char*, const int&,
				const int&, Complex*, const int&,
				double*, Complex*, const int&,
				Complex*, const int&, Complex*,
				const int&, double*, int&, long,
				long);
}

ComplexMatrix
ComplexSVD::left_singular_matrix (void) const
{
  if (type_computed == SVD::sigma_only)
    {
      (*current_liboctave_error_handler)
	("ComplexSVD: U not computed because type == SVD::sigma_only");
      return ComplexMatrix ();
    }
  else
    return left_sm;
}

ComplexMatrix
ComplexSVD::right_singular_matrix (void) const
{
  if (type_computed == SVD::sigma_only)
    {
      (*current_liboctave_error_handler)
	("ComplexSVD: V not computed because type == SVD::sigma_only");
      return ComplexMatrix ();
    }
  else
    return right_sm;
}

int
ComplexSVD::init (const ComplexMatrix& a, SVD::type svd_type)
{
  int info;

  int m = a.rows ();
  int n = a.cols ();

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  int min_mn = m < n ? m : n;
  int max_mn = m > n ? m : n;

  char jobu = 'A';
  char jobv = 'A';

  int ncol_u = m;
  int nrow_vt = n;
  int nrow_s = m;
  int ncol_s = n;

  switch (svd_type)
    {
    case SVD::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case SVD::sigma_only:
      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  type_computed = svd_type;

  if (jobu != 'N')
    left_sm.resize (m, ncol_u);

  Complex *u = left_sm.fortran_vec ();

  sigma.resize (nrow_s, ncol_s);
  double *s_vec = sigma.fortran_vec ();

  if (jobv != 'N')
    right_sm.resize (nrow_vt, n);

  Complex *vt = right_sm.fortran_vec ();

  int lwork = 2*min_mn + max_mn;

  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  int lrwork = 5*max_mn;

  Array<double> rwork (lrwork);
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zgesvd, ZGESVD, (&jobu, &jobv, m, n, tmp_data, m, s_vec, u,
			     m, vt, nrow_vt, pwork, lwork, prwork, info,
			     1L, 1L));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgesvd");
  else
    {
      if (jobv != 'N')
	right_sm = right_sm.hermitian ();
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
