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

#include "CmplxQR.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (zgeqrf, ZGEQRF) (const int&, const int&, Complex*,
				const int&, Complex*, Complex*,
				const int&, int&); 

  int F77_FCN (zungqr, ZUNGQR) (const int&, const int&, const int&,
				Complex*, const int&, Complex*,
				Complex*, const int&, int&);
}

ComplexQR::ComplexQR (const ComplexMatrix& a, QR::type qr_type)
{
  tau = 0;
  work = 0;
  tmp_data = 0;

  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler)
	("ComplexQR must have non-empty matrix");
      return;
    }

  int min_mn = m < n ? m : n;
  tau = new Complex[min_mn];

  int lwork = 32*n;
  work = new Complex[lwork];

  int info = 0;

  if (m > n)
    {
      tmp_data = new Complex [m*m];
      copy (tmp_data, a.data (), a.length ());
    }
  else
    tmp_data = dup (a.data (), a.length ());

  F77_XFCN (zgeqrf, ZGEQRF, (m, n, tmp_data, m, tau, work, lwork, info));

  delete [] work;
  work = 0;

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeqrf");
  else
    {
      if (qr_type == QR::raw)
	{
	  for (int j = 0; j < min_mn; j++)
	    {
	      int limit = j < min_mn - 1 ? j : min_mn - 1;
	      for (int i = limit + 1; i < m; i++)
		tmp_data[m*j+i] *= tau[j];
	    }
	}
      else
	{
	  volatile int n2;

	  if (qr_type == QR::economy && m > n)
	    {
	      n2 = n;
	      r.resize (n, n, 0.0);
	    }
	  else
	    {
	      n2 = m;
	      r.resize (m, n, 0.0);
	    }

	  for (int j = 0; j < n; j++)
	    {
	      int limit = j < min_mn-1 ? j : min_mn-1;
	      for (int i = 0; i <= limit; i++)
		r.elem (i, j) = tmp_data[m*j+i];
	    }

	  lwork = 32*m;
	  work = new Complex[lwork];

	  F77_XFCN (zungqr, ZUNGQR, (m, m, min_mn, tmp_data, m, tau, work,
				     lwork, info));

	  delete [] work;
	  work = 0;

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in zungqr");
	  else
	    {
	      q = ComplexMatrix (tmp_data, m, m);
	      tmp_data = 0;
	      q.resize (m, n2);
	    }
	}
    }

  delete [] tau;
  tau = 0;

  delete [] tmp_data;
  tmp_data = 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
