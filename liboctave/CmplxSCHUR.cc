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

#include "CmplxSCHUR.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zgeesx, ZGEESX) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     ComplexSCHUR::select_function,
			     F77_CONST_CHAR_ARG_DECL,
			     const int&, Complex*, const int&, int&,
			     Complex*, Complex*, const int&, double&,
			     double&, Complex*, const int&, double*, int*,
			     int&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);
}

static int
select_ana (const Complex& a)
{
  return a.real () < 0.0;
}

static int
select_dig (const Complex& a)
{
  return (abs (a) < 1.0);
}

int
ComplexSCHUR::init (const ComplexMatrix& a, const std::string& ord, 
		    bool calc_unitary)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexSCHUR requires square matrix");
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
  int info;
  int sdim;
  double rconde;
  double rcondv;

  schur_mat = a;
  if (calc_unitary)
    unitary_mat.resize (n, n);

  Complex *s = schur_mat.fortran_vec ();
  Complex *q = unitary_mat.fortran_vec ();

  Array<double> rwork (n);
  double *prwork = rwork.fortran_vec ();

  Array<Complex> w (n);
  Complex *pw = w.fortran_vec ();

  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  Array<int> bwork ((ord_char == 'N' || ord_char == 'n') ? 0 : n);
  int *pbwork = bwork.fortran_vec ();

  F77_XFCN (zgeesx, ZGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
			     F77_CONST_CHAR_ARG2 (&sort, 1),
			     selector,
			     F77_CONST_CHAR_ARG2 (&sense, 1),
			     n, s, n, sdim, pw, q, n, rconde, rcondv,
			     pwork, lwork, prwork, pbwork, info
			     F77_CHAR_ARG_LEN (1)
			     F77_CHAR_ARG_LEN (1)
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeesx");

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
