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

#include "CmplxLU.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (zgesv, ZGESV) (const int&, const int&, Complex*,
			      const int&, int*, Complex*, const int&,
			      int&);
}

ComplexLU::ComplexLU (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr == 0 || a_nc == 0 || a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("ComplexLU requires square matrix");
      return;
    }

  int n = a_nr;

  int *ipvt = new int [n];
  int *pvt = new int [n];
  Complex *tmp_data = dup (a.data (), a.length ());
  int info = 0;
  Complex *dummy = 0;

  F77_FCN (zgesv, ZGESV) (n, 0, tmp_data, n, ipvt, dummy, n, info);

  ComplexMatrix A_fact (tmp_data, n, n);

  for (int i = 0; i < n; i++)
    {
      ipvt[i] -= 1;
      pvt[i] = i;
    }

  for (int i = 0; i < n - 1; i++)
    {
      int k = ipvt[i];
      if (k != i)
	{
	  int tmp = pvt[k];
	  pvt[k] = pvt[i];
	  pvt[i] = tmp;
	}
    }

  l.resize (n, n, 0.0);
  u.resize (n, n, 0.0);
  p.resize (n, n, 0.0);

  for (int i = 0; i < n; i++)
    {
      p.elem (i, pvt[i]) = 1.0;

      l.elem (i, i) = 1.0;
      for (int j = 0; j < i; j++)
	l.elem (i, j) = A_fact.elem (i, j);

      for (int j = i; j < n; j++)
	u.elem (i, j) = A_fact.elem (i, j);
    }

  delete [] ipvt;
  delete [] pvt;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
