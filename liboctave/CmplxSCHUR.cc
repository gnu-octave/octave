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

#include "CmplxSCHUR.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (zgeesx, ZGEESX) (const char*, const char*,
				int (*)(const Complex&), 
				const char*, const int&, Complex*,
				const int&, int&, Complex*, Complex*,
				const int&, double&, double&,
				Complex*, const int&, double*, int*,
				int&, long, long);
}

static int
complex_select_ana (const Complex& a)
{
  return a.real () < 0.0;
}

static int
complex_select_dig (const Complex& a)
{
  return (abs (a) < 1.0);
}

int
ComplexSCHUR::init (const ComplexMatrix& a, const char *ord)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexSCHUR requires square matrix");
      return -1;
    }

  char *jobvs = "V";
  char *sort;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
     sort = "S";
   else
     sort = "N";

  char *sense = "N";

  int n = a_nc;
  int lwork = 8 * n;
  int info;
  int sdim;
  double rconde;
  double rcondv;

  double *rwork = new double [n];

// bwork is not referenced for non-ordered Schur.

  int *bwork = 0;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
    bwork = new int [n];

  Complex *s = dup (a.data (), a.length ());

  Complex *work = new Complex [lwork];
  Complex *q = new Complex [n*n];
  Complex *w = new Complex [n];

  if (*ord == 'A' || *ord == 'a')
    {
      F77_FCN (zgeesx, ZGEESX) (jobvs, sort, complex_select_ana,
				sense, n, s, n, sdim, w, q, n, rconde,
				rcondv, work, lwork, rwork, bwork,
				info, 1L, 1L);
    }
  else if (*ord == 'D' || *ord == 'd')
    {
      F77_FCN (zgeesx, ZGEESX) (jobvs, sort, complex_select_dig,
				sense, n, s, n, sdim, w, q, n, rconde,
				rcondv, work, lwork, rwork, bwork,
				info, 1L, 1L);
    }
  else
    {
      F77_FCN (zgeesx, ZGEESX) (jobvs, sort, (void *) 0, sense, n, s,
				n, sdim, w, q, n, rconde, rcondv,
				work, lwork, rwork, bwork, info, 1L,
				1L);
    }

  schur_mat = ComplexMatrix (s, n, n);
  unitary_mat = ComplexMatrix (q, n, n);

  delete [] w;
  delete [] work;
  delete [] rwork;
  delete [] bwork;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
