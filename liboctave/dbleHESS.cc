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

#include "dbleHESS.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (dgebal, DGEBAL) (const char*, const int&, double*,
				const int&, int&, int&, double*,
				int&, long, long);

  int F77_FCN (dgehrd, DGEHRD) (const int&, const int&, const int&,
				double*, const int&, double*, double*,
				const int&, int&, long, long);

  int F77_FCN (dorghr, DORGHR) (const int&, const int&, const int&,
				double*, const int&, double*, double*,
				const int&, int&, long, long);

  int F77_FCN (dgebak, DGEBAK) (const char*, const char*, const int&,
				const int&, const int&, double*,
				const int&, double*, const int&, int&,
				long, long);
}

int
HESS::init (const Matrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("HESS requires square matrix");
      return -1;
    }

  char *jobbal = "N";
  char *side = "R";

  int n = a_nc;
  int lwork = 32 * n;
  int info;
  int ilo;
  int ihi;

  double *h = dup (a.data (), a.length ());

  double *tau = new double [n+1];
  double *scale = new double [n];
  double *z = new double [n*n];
  double *work = new double [lwork];

  F77_FCN (dgebal, DGEBAL) (jobbal, n, h, n, ilo, ihi, scale, info,
			    1L, 1L);

  F77_FCN (dgehrd, DGEHRD) (n, ilo, ihi, h, n, tau, work, lwork, info,
			    1L, 1L);

  copy (z, h, n*n);

  F77_FCN (dorghr, DORGHR) (n, ilo, ihi, z, n, tau, work, lwork, info,
			    1L, 1L);

  F77_FCN (dgebak, DGEBAK) (jobbal, side, n, ilo, ihi, scale, n, z, n,
			    info, 1L, 1L);

  // We need to clear out all of the area below the sub-diagonal which
  // was used to store the unitary matrix.

  hess_mat = Matrix (h, n, n);
  unitary_hess_mat = Matrix (z, n, n);

  // If someone thinks of a more graceful way of doing this (or faster
  // for that matter :-)), please let me know! 

  if (n > 2)
    for (int j = 0; j < a_nc; j++)
      for (int i = j+2; i < a_nr; i++)
        hess_mat.elem (i, j) = 0;

  delete [] tau;
  delete [] work;
  delete [] scale;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
