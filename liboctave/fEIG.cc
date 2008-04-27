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

#include "fEIG.h"
#include "fColVector.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (sgeev, SGEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, float*, const octave_idx_type&, float*,
			   float*, float*, const octave_idx_type&, float*,
			   const octave_idx_type&, float*, const octave_idx_type&, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgeev, CGEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, FloatComplex*, const octave_idx_type&, FloatComplex*,
			   FloatComplex*, const octave_idx_type&, FloatComplex*, const octave_idx_type&,
			   FloatComplex*, const octave_idx_type&, float*, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ssyev, SSYEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, float*, const octave_idx_type&, float*,
			   float*, const octave_idx_type&, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cheev, CHEEV) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, FloatComplex*, const octave_idx_type&, float*,
			   FloatComplex*, const octave_idx_type&, float*, octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
FloatEIG::init (const FloatMatrix& a, bool calc_ev)
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

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  Array<float> wr (n);
  float *pwr = wr.fortran_vec ();

  Array<float> wi (n);
  float *pwi = wi.fortran_vec ();

  volatile octave_idx_type nvr = calc_ev ? n : 0;
  FloatMatrix vr (nvr, nvr);
  float *pvr = vr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  float *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (sgeev, SGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			   F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   n, tmp_data, n, pwr, pwi, dummy,
			   idummy, pvr, n, &dummy_work, lwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<float> work (lwork);
      float *pwork = work.fortran_vec ();

      F77_XFCN (sgeev, SGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			       F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       n, tmp_data, n, pwr, pwi, dummy,
			       idummy, pvr, n, pwork, lwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in sgeev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("sgeev failed to converge");
	  return info;
	}

      lambda.resize (n);
      v.resize (nvr, nvr);

      for (octave_idx_type j = 0; j < n; j++)
	{
	  if (wi.elem (j) == 0.0)
	    {
	      lambda.elem (j) = FloatComplex (wr.elem (j));
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

	      lambda.elem(j) = FloatComplex (wr.elem(j), wi.elem(j));
	      lambda.elem(j+1) = FloatComplex (wr.elem(j+1), wi.elem(j+1));

	      for (octave_idx_type i = 0; i < nvr; i++)
		{
		  float real_part = vr.elem (i, j);
		  float imag_part = vr.elem (i, j+1);
		  v.elem (i, j) = FloatComplex (real_part, imag_part);
		  v.elem (i, j+1) = FloatComplex (real_part, -imag_part);
		}
	      j++;
	    }
	}
    }
  else
    (*current_liboctave_error_handler) ("sgeev workspace query failed");

  return info;
}

octave_idx_type 
FloatEIG::symmetric_init (const FloatMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  float dummy_work;

  F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   F77_CONST_CHAR_ARG2 ("U", 1),
			   n, tmp_data, n, pwr, &dummy_work, lwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work);
      Array<float> work (lwork);
      float *pwork = work.fortran_vec ();

      F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       F77_CONST_CHAR_ARG2 ("U", 1),
			       n, tmp_data, n, pwr, pwork, lwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in ssyev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("ssyev failed to converge");
	  return info;
	}

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("ssyev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::init (const FloatComplexMatrix& a, bool calc_ev)
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

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatComplexColumnVector w (n);
  FloatComplex *pw = w.fortran_vec ();

  octave_idx_type nvr = calc_ev ? n : 0;
  FloatComplexMatrix vtmp (nvr, nvr);
  FloatComplex *pv = vtmp.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 2*n;
  Array<float> rwork (lrwork);
  float *prwork = rwork.fortran_vec ();

  FloatComplex *dummy = 0;
  octave_idx_type idummy = 1;

  F77_XFCN (cgeev, CGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			   F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   n, tmp_data, n, pw, dummy, idummy,
			   pv, n, &dummy_work, lwork, prwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<FloatComplex> work (lwork);
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (cgeev, CGEEV, (F77_CONST_CHAR_ARG2 ("N", 1),
			       F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       n, tmp_data, n, pw, dummy, idummy,
			       pv, n, pwork, lwork, prwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in cgeev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("cgeev failed to converge");
	  return info;
	}

      lambda = w;
      v = vtmp;
    }
  else
    (*current_liboctave_error_handler) ("cgeev workspace query failed");

  return info;
}

octave_idx_type
FloatEIG::hermitian_init (const FloatComplexMatrix& a, bool calc_ev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  octave_idx_type info = 0;

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  FloatComplex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<float> rwork (lrwork);
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			   F77_CONST_CHAR_ARG2 ("U", 1),
			   n, tmp_data, n, pwr, &dummy_work, lwork,
			   prwork, info
			   F77_CHAR_ARG_LEN (1)
			   F77_CHAR_ARG_LEN (1)));

  if (info == 0)
    {
      lwork = static_cast<octave_idx_type> (dummy_work.real ());
      Array<FloatComplex> work (lwork);
      FloatComplex *pwork = work.fortran_vec ();

      F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_ev ? "V" : "N", 1),
			       F77_CONST_CHAR_ARG2 ("U", 1),
			       n, tmp_data, n, pwr, pwork, lwork, prwork, info
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (info < 0)
	{
	  (*current_liboctave_error_handler) ("unrecoverable error in cheev");
	  return info;
	}

      if (info > 0)
	{
	  (*current_liboctave_error_handler) ("cheev failed to converge");
	  return info;
	}

      lambda = FloatComplexColumnVector (wr);
      v = calc_ev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ();
    }
  else
    (*current_liboctave_error_handler) ("cheev workspace query failed");

  return info;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
