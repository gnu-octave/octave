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

#include "CmplxCHOL.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (zpotrf, ZPOTRF) (const char*, const int&, Complex*,
				const int&, int&, long);
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

   Complex *h = dup (a.data (), a.length ());

   F77_FCN (zpotrf, ZPOTRF) ("U", n, h, n, info, 1L);

   chol_mat = ComplexMatrix (h, n, n);

// If someone thinks of a more graceful way of doing this (or faster for
// that matter :-)), please let me know!

  if (n > 1)
    for (int j = 0; j < a_nc; j++)
      for (int i = j+1; i < a_nr; i++)
        chol_mat.elem (i, j) = 0.0;

   return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
