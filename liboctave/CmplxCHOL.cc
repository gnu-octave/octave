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

#include "CmplxCHOL.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zpotrf, ZPOTRF) (F77_CONST_CHAR_ARG_DECL,
			     const int&, Complex*, const int&, int&
			     F77_CHAR_ARG_LEN_DECL);
}

int
ComplexCHOL::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexCHOL requires square matrix");
      return -1;
    }

  int n = a_nc;
  int info;

  chol_mat = a;
  Complex *h = chol_mat.fortran_vec ();

  F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zpotrf");
  else
    {
      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
	for (int j = 0; j < a_nc; j++)
	  for (int i = j+1; i < a_nr; i++)
	    chol_mat.elem (i, j) = 0.0;
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
