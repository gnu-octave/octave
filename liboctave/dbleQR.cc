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

#include "dbleQR.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (dgeqrf) (const int*, const int*, double*, const int*,
			double*, double*, const int*, int*);

  int F77_FCN (dorgqr) (const int*, const int*, const int*, double*,
			const int*, double*, double*, const int*, int*);
}

QR::QR (const Matrix& a)
{
  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  double *tmp_data;
  int min_mn = m < n ? m : n;
  double *tau = new double[min_mn];
  int lwork = 32*n;
  double *work = new double[lwork];
  int info = 0;

  if (m > n)
    {
      tmp_data = new double [m*m];
      copy (tmp_data, a.data (), a.length ());
    }
  else
    tmp_data = dup (a.data (), a.length ());

  F77_FCN (dgeqrf) (&m, &n, tmp_data, &m, tau, work, &lwork, &info);

  delete [] work;

  r.resize (m, n, 0.0);
  for (int j = 0; j < n; j++)
    {
      int limit = j < min_mn-1 ? j : min_mn-1;
      for (int i = 0; i <= limit; i++)
	r.elem (i, j) = tmp_data[m*j+i];
    }

  lwork = 32*m;
  work = new double[lwork];

  F77_FCN (dorgqr) (&m, &m, &min_mn, tmp_data, &m, tau, work, &lwork, &info);

  q = Matrix (tmp_data, m, m);

  delete [] tau;
  delete [] work;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
