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

#include "CmplxHESS.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (zgebal) (const char*, const int*, Complex*, const int*,
                        int*, int*, double*, int*, long, long);
 
  int F77_FCN (zgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, Complex*, 
			const int*, int*, long, long);

  int F77_FCN (zgehrd) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);
 
  int F77_FCN (zunghr) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);
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

   Complex *h = dup (a.data (), a.length ());

   double *scale = new double [n];
   Complex *tau = new Complex [n-1];
   Complex *work = new Complex [lwork];
   Complex *z = new Complex [n*n];

   F77_FCN (zgebal) (&job, &n, h, &n, &ilo, &ihi, scale, &info, 1L, 1L);

   F77_FCN (zgehrd) (&n, &ilo, &ihi, h, &n, tau, work, &lwork, &info, 1L,
		     1L);

   copy (z, h, n*n);

   F77_FCN (zunghr) (&n, &ilo, &ihi, z, &n, tau, work, &lwork, &info, 1L,
		     1L);

   F77_FCN (zgebak) (&job, &side, &n, &ilo, &ihi, scale, &n, z, &n, &info,
		     1L, 1L); 

   hess_mat = ComplexMatrix (h,n,n);
   unitary_hess_mat = ComplexMatrix (z,n,n);

// If someone thinks of a more graceful way of doing this (or faster for
// that matter :-)), please let me know!

   if (n > 2)
     for (int j = 0; j < a_nc; j++)
       for (int i = j+2; i < a_nr; i++)
         hess_mat.elem (i, j) = 0;

   delete [] work;
   delete [] tau;
   delete [] scale;

   return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
