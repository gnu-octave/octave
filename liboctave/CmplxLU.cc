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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CmplxLU.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (zgesv) (const int&, const int&, Complex*, const int&,
		       int*, Complex*, const int&, int&);
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
  Complex *b;

  F77_FCN (zgesv) (n, 0, tmp_data, n, ipvt, b, n, info);

  ComplexMatrix A_fact (tmp_data, n, n);

  int i;

  for (i = 0; i < n; i++)
    {
      ipvt[i] -= 1;
      pvt[i] = i;
    }

  for (i = 0; i < n - 1; i++)
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

  for (i = 0; i < n; i++)
    {
      p.elem (i, pvt[i]) = 1.0;

      int j;

      l.elem (i, i) = 1.0;
      for (j = 0; j < i; j++)
	l.elem (i, j) = A_fact.elem (i, j);

      for (j = i; j < n; j++)
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
