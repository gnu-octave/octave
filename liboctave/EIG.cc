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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "EIG.h"
#include "dColVector.h"
#include "f77-fcn.h"
#include "lo-error.h"

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

  int F77_FCN (dsyev, DSYEV) (const char*, const char*, const int&,
			      double*, const int&, double*, double*,
			      const int&, int&, long, long);

  int F77_FCN (zheev, ZHEEV) (const char*, const char*, const int&,
			      Complex*, const int&, double*, Complex*,
			      const int&, double*, int&, long, long);
}

int
EIG::init (const Matrix& a)
{
  if (a.is_symmetric ())
    return symmetric_init (a);

  int n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  Array<double> wr (n);
  double *pwr = wr.fortran_vec ();

  Array<double> wi (n);
  double *pwi = wi.fortran_vec ();

  Matrix vr (n, n);
  double *pvr = vr.fortran_vec ();

  // XXX FIXME XXX -- it might be possible to choose a better value of
  // lwork that would result in more efficient computations.

  int lwork = 8*n;
  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  double *dummy = 0;
  int idummy = 1;

  F77_XFCN (dgeev, DGEEV, ("N", "V", n, tmp_data, n, pwr, pwi, dummy,
			   idummy, pvr, n, pwork, lwork, info, 1L, 1L));

  if (f77_exception_encountered || info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dgeev");
  else
    {
      if (info > 0)
	(*current_liboctave_error_handler) ("dgeev failed to converge");
      else
	{
	  lambda.resize (n);
	  v.resize (n, n);

	  for (int j = 0; j < n; j++)
	    {
	      if (wi.elem (j) == 0.0)
		{
		  lambda.elem (j) = Complex (wr.elem (j));
		  for (int i = 0; i < n; i++)
		    v.elem (i, j) = vr.elem (i, j);
		}
	      else
		{
		  if (j+1 >= n)
		    {
		      (*current_liboctave_error_handler)
			("EIG: internal error");
		      return -1;
		    }

		  lambda.elem(j) = Complex (wr.elem(j), wi.elem(j));
		  lambda.elem(j+1) = Complex (wr.elem(j+1), wi.elem(j+1));

		  for (int i = 0; i < n; i++)
		    {
		      double real_part = vr.elem (i, j);
		      double imag_part = vr.elem (i, j+1);
		      v.elem (i, j) = Complex (real_part, imag_part);
		      v.elem (i, j+1) = Complex (real_part, -imag_part);
		    }
		  j++;
		}
	    }
	}
    }

  return info;
}

int
EIG::symmetric_init (const Matrix& a)
{
  int n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  // XXX FIXME XXX -- it might be possible to choose a better value of
  // lwork that would result in more efficient computations.

  int lwork = 8*n;
  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  F77_XFCN (dsyev, DSYEV, ("V", "U", n, tmp_data, n, pwr, pwork,
			   lwork, info, 1L, 1L));

  if (f77_exception_encountered || info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dsyev");
  else if (info > 0)
    (*current_liboctave_error_handler) ("dsyev failed to converge");
  else
    {
      lambda = ComplexColumnVector (wr);
      v = ComplexMatrix (atmp);
    }

  return info;
}

int
EIG::init (const ComplexMatrix& a)
{
  if (a.is_hermitian ())
    return hermitian_init (a);

  int n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ComplexColumnVector w (n);
  Complex *pw = w.fortran_vec ();

  ComplexMatrix vtmp (n, n);
  Complex *pv = vtmp.fortran_vec ();

  // XXX FIXME XXX -- it might be possible to choose a better value of
  // lwork that would result in more efficient computations.

  int lwork = 8*n;
  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  int lrwork = 2*n;
  Array<double> rwork (lrwork);
  double *prwork = rwork.fortran_vec ();

  Complex *dummy = 0;
  int idummy = 1;

  F77_XFCN (zgeev, ZGEEV, ("N", "V", n, tmp_data, n, pw, dummy, idummy,
			   pv, n, pwork, lwork, prwork, info, 1L, 1L));

  if (f77_exception_encountered || info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeev");
  else if (info > 0)
    (*current_liboctave_error_handler) ("zgeev failed to converge");
  else
    {
      lambda = w;
      v = vtmp;
    }

  return info;
}

int
EIG::hermitian_init (const ComplexMatrix& a)
{
  int n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  // XXX FIXME XXX -- it might be possible to choose a better value of
  // lwork that would result in more efficient computations.

  int lwork = 8*n;
  Array<Complex> work (lwork);
  Complex *pwork = work.fortran_vec ();

  int lrwork = 3*n;
  Array<double> rwork (lrwork);
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zheev, ZHEEV, ("V", "U", n, tmp_data, n, pwr, pwork,
			   lwork, prwork, info, 1L, 1L));

  if (f77_exception_encountered || info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zheev");
  else if (info > 0)
    (*current_liboctave_error_handler) ("zheev failed to converge");
  else
    {
      lambda = ComplexColumnVector (wr);
      v = ComplexMatrix (atmp);
    }

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
