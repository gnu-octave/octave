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

#include <iostream>

#include "dbleSCHUR.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeesx, DGEESX) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     SCHUR::select_function,
			     F77_CONST_CHAR_ARG_DECL,
			     const int&, double*, const int&, int&,
			     double*, double*, double*, const int&,
			     double&, double&, double*, const int&,
			     int*, const int&, int*, int&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);
}

static int
select_ana (const double& a, const double&)
{
   return (a < 0.0);
}

static int
select_dig (const double& a, const double& b)
{
  return (hypot (a, b) < 1.0);
}

int
SCHUR::init (const Matrix& a, const std::string& ord, bool calc_unitary)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("SCHUR requires square matrix");
      return -1;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;
  else
    selector = 0;

  int n = a_nc;
  int lwork = 8 * n;
  int liwork = 1;
  int info;
  int sdim;
  double rconde;
  double rcondv;

  schur_mat = a;

  if (calc_unitary)
    unitary_mat.resize (n, n);

  double *s = schur_mat.fortran_vec ();
  double *q = unitary_mat.fortran_vec ();

  Array<double> wr (n);
  double *pwr = wr.fortran_vec ();

  Array<double> wi (n);
  double *pwi = wi.fortran_vec ();

  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  // BWORK is not referenced for the non-ordered Schur routine.
  Array<int> bwork ((ord_char == 'N' || ord_char == 'n') ? 0 : n);
  int *pbwork = bwork.fortran_vec ();

  Array<int> iwork (liwork);
  int *piwork = iwork.fortran_vec ();

  F77_XFCN (dgeesx, DGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
			     F77_CONST_CHAR_ARG2 (&sort, 1),
			     selector,
			     F77_CONST_CHAR_ARG2 (&sense, 1),
			     n, s, n, sdim, pwr, pwi, q, n, rconde, rcondv,
			     pwork, lwork, piwork, liwork, pbwork, info
			     F77_CHAR_ARG_LEN (1)
			     F77_CHAR_ARG_LEN (1)
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dgeesx");

  return info;
}

std::ostream&
operator << (std::ostream& os, const SCHUR& a)
{
  os << a.schur_matrix () << "\n";
  os << a.unitary_matrix () << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
