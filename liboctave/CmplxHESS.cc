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

#include "CmplxHESS.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zgebal, ZGEBAL) (F77_CONST_CHAR_ARG_DECL,
			     const int&, Complex*, const int&,
			     int&, int&, double*, int&
			     F77_CHAR_ARG_LEN_DECL);
 
  F77_RET_T
  F77_FUNC (zgehrd, ZGEHRD) (const int&, const int&, const int&,
			     Complex*, const int&, Complex*,
			     Complex*, const int&, int&);
 
  F77_RET_T
  F77_FUNC (zunghr, ZUNGHR) (const int&, const int&, const int&,
			     Complex*, const int&, Complex*,
			     Complex*, const int&, int&);

  F77_RET_T
  F77_FUNC (zgebak, ZGEBAK) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     const int&, const int&, const int&, double*,
			     const int&, Complex*, const int&, int&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);
}

int
ComplexHESS::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexHESS requires square matrix");
      return -1;
    }

  char job = 'N';
  char side = 'R';

  int n = a_nc;
  int lwork = 32 * n;
  int info;
  int ilo;
  int ihi;

  hess_mat = a;
  Complex *h = hess_mat.fortran_vec ();

  Array<double> scale (n);
  double *pscale = scale.fortran_vec ();

  F77_XFCN (zgebal, ZGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
			     n, h, n, ilo, ihi, pscale, info
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgebal");
  else
    {
      Array<Complex> tau (n-1);
      Complex *ptau = tau.fortran_vec ();

      Array<Complex> work (lwork);
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zgehrd, ZGEHRD, (n, ilo, ihi, h, n, ptau, pwork, lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgehrd");
      else
	{
	  unitary_hess_mat = hess_mat;
	  Complex *z = unitary_hess_mat.fortran_vec ();

	  F77_XFCN (zunghr, ZUNGHR, (n, ilo, ihi, z, n, ptau, pwork,
				     lwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in zunghr");
	  else
	    {
	      F77_XFCN (zgebak, ZGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
					 F77_CONST_CHAR_ARG2 (&side, 1),
					 n, ilo, ihi, pscale, n, z, n, info
					 F77_CHAR_ARG_LEN (1)
					 F77_CHAR_ARG_LEN (1)));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in zgebak");
	      else
		{
		  // If someone thinks of a more graceful way of
		  // doing this (or faster for that matter :-)),
		  // please let me know!

		  if (n > 2)
		    for (int j = 0; j < a_nc; j++)
		      for (int i = j+2; i < a_nr; i++)
			hess_mat.elem (i, j) = 0;
		}
	    }
	}
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
