/*

Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2003, 2004,
              2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "EIG.h"
#include "dColVector.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeev, DGEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, double*, const octave_idx_type&, double*,
			   double*, double*, const octave_idx_type&, double*,
			   const octave_idx_type&, double*, const octave_idx_type&, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgeev, ZGEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, Complex*, const octave_idx_type&, Complex*,
			   Complex*, const octave_idx_type&, Complex*, const octave_idx_type&,
			   Complex*, const octave_idx_type&, double*, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dsyev, DSYEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, double*, const octave_idx_type&, double*,
			   double*, const octave_idx_type&, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zheev, ZHEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, Complex*, const octave_idx_type&, double*,
			   Complex*, const octave_idx_type&, double*, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
EIG::init (const Matrix& a, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
	("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  if (a.is_symmetric ())
    return symmetric_init (a, calc_ev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  Array<double> wr (n);
  double *pwr = wr.fortran_vec ();

  Array<double> wi (n);
  double *pwi = wi.fortran_vec ();

  volatile octave_idx_type nvr = calc_ev ? n : 0;
  Matrix vr (nvr, nvr);
  double *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  double *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (dgeev, DGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			   F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   n, tmp_data, n, pwr, pwi, dummy,
			   idummy, pvr, n, &dummy_work, lwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (lwork);
      double *pwork = work.fortran_vec ();

      F77_XFCN (dgeev, DGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			       F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       n, tmp_data, n, pwr, pwi, dummy,
			       idummy, pvr, n, pwork, lwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in dgeev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("dgeev failed to converge");
	  return info;
	}

      lambda.resize (n);
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
	{
	  if (wi.elem (j) == 0.0)
	    {
	      lambda.elem (j) = Complex (wr.elem (j));
	      for (octave_idx_type i = 0; i < nvr; i++)
		v.elem (i, j) = vr.elem (i, j);
	    }
	  else
	    {
	      if (j+1 >= n)
		{
		  (*current_liboctave_error_handler) ("EIG: internal error");
		  return -1;
		}

	      lambda.elem(j) = Complex (wr.elem(j), wi.elem(j));
	      lambda.elem(j+1) = Complex (wr.elem(j+1), wi.elem(j+1));

	      for (octave_idx_type i = 0; i < nvr; i++)
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
  else
    (*current_liboctave_error_handler) ("dgeev workspace query failed");

  return info;
}

octave_idx_type 
EIG::symmetric_init (const Matrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   F77_CONST_CHAR_ARG2 ("U", 1),
			   n, tmp_data, n, pwr, &dummy_work, lwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<double> work (lwork);
      double *pwork = work.fortran_vec ();

      F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       F77_CONST_CHAR_ARG2 ("U", 1),
			       n, tmp_data, n, pwr, pwork, lwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in dsyev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("dsyev failed to converge");
	  return info;
	}

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("dsyev workspace query failed");

  return info;
}

octave_idx_type
EIG::init (const ComplexMatrix& a, bool calc_ev)
{
  if (a.any_element_is_inf_or_nan ())
    {
      (*current_liboctave_error_handler)
	("EIG: matrix contains Inf or NaN values");
      return -1;
    }

  if (a.is_hermitian ())
    return hermitian_init (a, calc_ev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ComplexColumnVector w (n);
  Complex *pw = w.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  ComplexMatrix vtmp (nvr, nvr);
  Complex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 2*n;
  Array<double> rwork (lrwork);
  double *prwork = rwork.fortran_vec ();

  Complex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (zgeev, ZGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			   F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   n, tmp_data, n, pw, dummy, idummy,
			   pv, n, &dummy_work, lwork, prwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (lwork);
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zgeev, ZGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			       F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       n, tmp_data, n, pw, dummy, idummy,
			       pv, n, pwork, lwork, prwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in zgeev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("zgeev failed to converge");
	  return info;
	}

      lambda = w;
      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("zgeev workspace query failed");

  return info;
}

octave_idx_type
EIG::hermitian_init (const ComplexMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<double> rwork (lrwork);
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   F77_CONST_CHAR_ARG2 ("U", 1),
			   n, tmp_data, n, pwr, &dummy_work, lwork,
			   prwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<Complex> work (lwork);
      Complex *pwork = work.fortran_vec ();

      F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       F77_CONST_CHAR_ARG2 ("U", 1),
			       n, tmp_data, n, pwr, pwork, lwork, prwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in zheev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("zheev failed to converge");
	  return info;
	}

      lambda = ComplexColumnVector (wr);
      v = calc_ev ? ComplexMatrix (atmp) : ComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("zheev workspace query failed");

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
