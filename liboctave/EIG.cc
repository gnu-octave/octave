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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "EIG.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (dgeev, DGEEV) (const char*, const char*, const int&,
			      double*, const int&, double*, double*,
			      double*, const int&, double*,
			      const int&, double*, const int&, int&,
			      long, long);

  int F77_FCN (zgeev, ZGEEV) (const char*, const char*, const int&,
			      Complex*, const int&, Complex*,
			      Complex*, const int&, Complex*,
			      const int&, Complex*, const int&,
			      double*, int&, long, long);
}

int
EIG::init (const Matrix& a)
{
  int a_nr = a.rows ();
  if (a_nr != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int n = a_nr;

  int info;

  double *tmp_data = dup (a.data (), a.length ());
  double *wr = new double[n];
  double *wi = new double[n];
  Matrix vr (n, n);
  double *pvr = vr.fortran_vec ();
  int lwork = 8*n;
  double *work = new double[lwork];

  double *dummy;
  int idummy = 1;

  F77_FCN (dgeev, DGEEV) ("N", "V", n, tmp_data, n, wr, wi, dummy,
			  idummy, pvr, n, work, lwork, info, 1L, 1L);

  lambda.resize (n);
  v.resize (n, n);

  for (int j = 0; j < n; j++)
    {
      if (wi[j] == 0.0)
	{
	  lambda.elem (j) = Complex (wr[j]);
	  for (int i = 0; i < n; i++)
	    v.elem (i, j) = vr.elem (i, j);
	}
      else
	{
	  if (j+1 >= n)
	    {
	      (*current_liboctave_error_handler) ("EIG: internal error");
	      return -1;
	    }

	  for (int i = 0; i < n; i++)
	    {
	      lambda.elem (j) = Complex (wr[j], wi[j]);
	      lambda.elem (j+1) = Complex (wr[j+1], wi[j+1]);
	      double real_part = vr.elem (i, j);
	      double imag_part = vr.elem (i, j+1);
	      v.elem (i, j) = Complex (real_part, imag_part);
	      v.elem (i, j+1) = Complex (real_part, -imag_part);
	    }
	  j++;
	}
    }

  delete [] tmp_data;
  delete [] wr;
  delete [] wi;
  delete [] work;

  return info;
}

int
EIG::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  if (a_nr != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int n = a_nr;

  int info;

  lambda.resize (n);
  v.resize (n, n);

  Complex *pw = lambda.fortran_vec ();
  Complex *pvr = v.fortran_vec ();

  Complex *tmp_data = dup (a.data (), a.length ());

  int lwork = 8*n;
  Complex *work = new Complex[lwork];
  double *rwork = new double[4*n];

  Complex *dummy;
  int idummy = 1;

  F77_FCN (zgeev, ZGEEV) ("N", "V", n, tmp_data, n, pw, dummy, idummy,
			  pvr, n, work, lwork, rwork, info, 1L, 1L);

  delete [] tmp_data;
  delete [] work;
  delete [] rwork;

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
