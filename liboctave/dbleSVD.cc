//                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include "dbleSVD.h"
#include "mx-inlines.cc"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (dgesvd) (const char*, const char*, const int*,
			const int*, double*, const int*, double*,
			double*, const int*, double*, const int*,
			double*, const int*, int*, long, long);
}

int
SVD::init (const Matrix& a)
{
  int info;

  int m = a.rows ();
  int n = a.cols ();

  char jobu = 'A';
  char jobv = 'A';

  double *tmp_data = dup (a.data (), a.length ());

  int min_mn = m < n ? m : n;
  int max_mn = m > n ? m : n;

  double *u = new double[m*m];
  double *s_vec  = new double[min_mn];
  double *vt = new double[n*n];

  int tmp1 = 3*min_mn + max_mn;
  int tmp2 = 5*min_mn - 4;
  int lwork = tmp1 > tmp2 ? tmp1 : tmp2;
  double *work = new double[lwork];

  F77_FCN (dgesvd) (&jobu, &jobv, &m, &n, tmp_data, &m, s_vec, u, &m,
		    vt, &n, work, &lwork, &info, 1L, 1L);

  left_sm = Matrix (u, m, m);
  sigma = DiagMatrix (s_vec, m, n);
  Matrix vt_m (vt, n, n);
  right_sm = Matrix (vt_m.transpose ());

  delete [] tmp_data;
  delete [] work;

  return info;
}

ostream&
operator << (ostream& os, const SVD& a)
{
  os << a.left_singular_matrix () << "\n";
  os << a.singular_values () << "\n";
  os << a.right_singular_matrix () << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
