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

#include "dbleSCHUR.h"
#include "f77-uscore.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (dgeesx, DGEESX) (const char*, const char*,
				int (*)(const double&, const double&),
				const char*, const int&, double*,
				const int&, int&, double*, double*,
				double*, const int&, double&, double&,
				double*, const int&, int*, const int&,
				int*, int&, long, long);
}

static int
select_ana (const double& a, const double& b)
{
   return (a < 0.0);
}

static int
select_dig (const double& a, const double& b)
{
  return (hypot (a, b) < 1.0);
}

int
SCHUR::init (const Matrix& a, const char *ord)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("SCHUR requires square matrix");
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
  int liwork = 1;
  int info;
  int sdim;
  double rconde;
  double rcondv;

  double *s = dup (a.data (), a.length ());

  double *wr = new double [n];
  double *wi = new double [n];
  double *q = new double [n*n];
  double *work = new double [lwork];

  // These are not referenced for the non-ordered Schur routine.

  int *iwork = 0;
  int *bwork = 0;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
    {
      iwork = new int [liwork];
      bwork = new int [n];
    }

  if (*ord == 'A' || *ord == 'a')
    {
      F77_FCN (dgeesx, DGEESX) (jobvs, sort, select_ana, sense, n, s,
				n, sdim, wr, wi, q, n, rconde, rcondv,
				work, lwork, iwork, liwork, bwork,
				info, 1L, 1L);
    }
  else if (*ord == 'D' || *ord == 'd')
    {
      F77_FCN (dgeesx, DGEESX) (jobvs, sort, select_dig, sense, n, s,
				n, sdim, wr, wi, q, n, rconde, rcondv,
				work, lwork, iwork, liwork, bwork,
				info, 1L, 1L);
      
    }
  else
    {
      F77_FCN (dgeesx, DGEESX) (jobvs, sort, (void *) 0, sense, n, s,
				n, sdim, wr, wi, q, n, rconde, rcondv,
				work, lwork, iwork, liwork, bwork,
				info, 1L, 1L);
    }

  schur_mat = Matrix (s, n, n);
  unitary_mat = Matrix (q, n, n);

  delete [] wr;
  delete [] wi;
  delete [] work;
  delete [] iwork;
  delete [] bwork;

  return info;
}

ostream&
operator << (ostream& os, const SCHUR& a)
{
  os << a.schur_matrix () << "\n";
  os << a.unitary_matrix () << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
