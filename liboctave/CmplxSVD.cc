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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CmplxSVD.h"
#include "f77-uscore.h"
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

int
ComplexSVD::init (const ComplexMatrix& a, SVD::type svd_type)
{
  int info;

  int m = a.rows ();
  int n = a.cols ();

  Complex *tmp_data = dup (a.data (), a.length ());

  int min_mn = m < n ? m : n;
  int max_mn = m > n ? m : n;

  char *jobu = "A";
  char *jobv = "A";

  int ncol_u = m;
  int nrow_vt = n;
  int nrow_s = m;
  int ncol_s = n;

  if (svd_type == SVD::economy)
    {
      jobu = jobv = "S";
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
    }

  Complex *u = new Complex[m * ncol_u];
  double *s_vec  = new double[min_mn];
  Complex *vt = new Complex[nrow_vt * n];

  int lwork = 2*min_mn + max_mn;
  Complex *work = new Complex[lwork];

  int lrwork = 5*max_mn;
  double *rwork = new double[lrwork];

  F77_FCN (zgesvd, ZGESVD) (jobu, jobv, m, n, tmp_data, m, s_vec, u,
			    m, vt, nrow_vt, work, lwork, rwork, info,
			    1L, 1L);

  left_sm = ComplexMatrix (u, m, ncol_u);
  sigma = DiagMatrix (s_vec, nrow_s, ncol_s);
  ComplexMatrix vt_m (vt, nrow_vt, n);
  right_sm = vt_m.hermitian ();

  delete [] tmp_data;
  delete [] work;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
