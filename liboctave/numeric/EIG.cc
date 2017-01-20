/*

Copyright (C) 1994-2016 John W. Eaton
Copyright (C) 2016 Barbara Lócsi

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "EIG.h"
#include "dColVector.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"

octave_idx_type
EIG::init (const Matrix& a, bool calc_rev, bool calc_lev, bool balance)
{
  if (a.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  if (a.is_symmetric ())
    return symmetric_init (a, calc_rev, calc_lev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  Array<double> wr (dim_vector (n, 1));
  double *pwr = wr.fortran_vec ();

  Array<double> wi (dim_vector (n, 1));
  double *pwi = wi.fortran_vec ();

  octave_idx_type tnvr = calc_rev ? n : 0;
  Matrix vr (tnvr, tnvr);
  double *pvr = vr.fortran_vec ();

  octave_idx_type tnvl = calc_lev ? n : 0;
  Matrix vl (tnvl, tnvl);
  double *pvl = vl.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  octave_idx_type ilo;
  octave_idx_type ihi;

  Array<double> scale (dim_vector (n, 1));
  double *pscale = scale.fortran_vec ();

  double abnrm;

  Array<double> rconde (dim_vector (n, 1));
  double *prconde = rconde.fortran_vec ();

  Array<double> rcondv (dim_vector (n, 1));
  double *prcondv = rcondv.fortran_vec ();

  octave_idx_type dummy_iwork;

  F77_XFCN (dgeevx, DGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, tmp_data, n, pwr, pwi, pvl,
                             n, pvr, n, ilo, ihi, pscale,
                             abnrm, prconde, prcondv, &dummy_work,
                             lwork, &dummy_iwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("dgeevx workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work);
  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dgeevx, DGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, tmp_data, n, pwr, pwi, pvl,
                             n, pvr, n, ilo, ihi, pscale,
                             abnrm, prconde, prcondv, pwork,
                             lwork, &dummy_iwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dgeevx");

  if (info > 0)
    (*current_liboctave_error_handler) ("dgeevx failed to converge");

  lambda.resize (n);
  octave_idx_type nvr = calc_rev ? n : 0;
  v.resize (nvr, nvr);
  octave_idx_type nvl = calc_lev ? n : 0;
  w.resize (nvl, nvl);

  for (octave_idx_type j = 0; j < n; j++)
    {
      if (wi.elem (j) == 0.0)
        {
          lambda.elem (j) = Complex (wr.elem (j));
          for (octave_idx_type i = 0; i < nvr; i++)
            v.elem (i, j) = vr.elem (i, j);

          for (octave_idx_type i = 0; i < nvl; i++)
            w.elem (i, j) = vl.elem (i, j);
        }
      else
        {
          if (j+1 >= n)
            (*current_liboctave_error_handler) ("EIG: internal error");

          lambda.elem (j) = Complex (wr.elem (j), wi.elem (j));
          lambda.elem (j+1) = Complex (wr.elem (j+1), wi.elem (j+1));

          for (octave_idx_type i = 0; i < nvr; i++)
            {
              double real_part = vr.elem (i, j);
              double imag_part = vr.elem (i, j+1);
              v.elem (i, j) = Complex (real_part, imag_part);
              v.elem (i, j+1) = Complex (real_part, -imag_part);
            }

          for (octave_idx_type i = 0; i < nvl; i++)
            {
              double real_part = vl.elem (i, j);
              double imag_part = vl.elem (i, j+1);
              w.elem (i, j) = Complex (real_part, imag_part);
              w.elem (i, j+1) = Complex (real_part, -imag_part);
            }
          j++;
        }
    }

  return info;
}

octave_idx_type
EIG::symmetric_init (const Matrix& a, bool calc_rev, bool calc_lev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("dsyev workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work);
  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dsyev, DSYEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dsyev");

  if (info > 0)
    (*current_liboctave_error_handler) ("dsyev failed to converge");

  lambda = ComplexColumnVector (wr);
  v = calc_rev ? ComplexMatrix (atmp) : ComplexMatrix ();
  w = calc_lev ? ComplexMatrix (atmp) : ComplexMatrix ();

  return info;
}

octave_idx_type
EIG::init (const ComplexMatrix& a, bool calc_rev, bool calc_lev, bool balance)
{
  if (a.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  if (a.is_hermitian ())
    return hermitian_init (a, calc_rev, calc_lev);

  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ComplexColumnVector wr (n);
  Complex *pw = wr.fortran_vec ();

  octave_idx_type nvr = calc_rev ? n : 0;
  ComplexMatrix vrtmp (nvr, nvr);
  Complex *pvr = vrtmp.fortran_vec ();

  octave_idx_type nvl = calc_lev ? n : 0;
  ComplexMatrix vltmp (nvl, nvl);
  Complex *pvl = vltmp.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 2*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  octave_idx_type ilo;
  octave_idx_type ihi;

  Array<double> scale (dim_vector (n, 1));
  double *pscale = scale.fortran_vec ();

  double abnrm;

  Array<double> rconde (dim_vector (n, 1));
  double *prconde = rconde.fortran_vec ();

  Array<double> rcondv (dim_vector (n, 1));
  double *prcondv = rcondv.fortran_vec ();

  F77_XFCN (zgeevx, ZGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, F77_DBLE_CMPLX_ARG (tmp_data), n,
                             F77_DBLE_CMPLX_ARG (pw), F77_DBLE_CMPLX_ARG (pvl), n,
                             F77_DBLE_CMPLX_ARG (pvr), n, ilo, ihi, pscale, abnrm,
                             prconde, prcondv,
                             F77_DBLE_CMPLX_ARG (&dummy_work), lwork, prwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("zgeevx workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work.real ());
  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  F77_XFCN (zgeevx, ZGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, F77_DBLE_CMPLX_ARG (tmp_data), n,
                             F77_DBLE_CMPLX_ARG (pw), F77_DBLE_CMPLX_ARG (pvl), n,
                             F77_DBLE_CMPLX_ARG (pvr), n, ilo, ihi, pscale, abnrm,
                             prconde, prcondv,
                             F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zgeevx");

  if (info > 0)
    (*current_liboctave_error_handler) ("zgeevx failed to converge");

  lambda = wr;
  v = vrtmp;
  w = vltmp;

  return info;
}

octave_idx_type
EIG::hermitian_init (const ComplexMatrix& a, bool calc_rev, bool calc_lev)
{
  octave_idx_type n = a.rows ();

  if (n != a.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_DBLE_CMPLX_ARG (tmp_data), n, pwr,
                           F77_DBLE_CMPLX_ARG (&dummy_work), lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("zheev workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work.real ());
  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  F77_XFCN (zheev, ZHEEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_DBLE_CMPLX_ARG (tmp_data), n, pwr,
                           F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zheev");

  if (info > 0)
    (*current_liboctave_error_handler) ("zheev failed to converge");

  lambda = ComplexColumnVector (wr);
  v = calc_rev ? ComplexMatrix (atmp) : ComplexMatrix ();
  w = calc_lev ? ComplexMatrix (atmp) : ComplexMatrix ();

  return info;
}

octave_idx_type
EIG::init (const Matrix& a, const Matrix& b, bool calc_rev, bool calc_lev,
           bool force_qz)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  octave_idx_type info = 0;

  Matrix tmp = b;
  double *tmp_data = tmp.fortran_vec ();

  if (! force_qz)
    {
      F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                                 n, tmp_data, n,
                                 info
                                 F77_CHAR_ARG_LEN (1)));

      if (a.is_symmetric () && b.is_symmetric () && info == 0)
        return symmetric_init (a, b, calc_rev, calc_lev);
    }

  Matrix atmp = a;
  double *atmp_data = atmp.fortran_vec ();

  Matrix btmp = b;
  double *btmp_data = btmp.fortran_vec ();

  Array<double> ar (dim_vector (n, 1));
  double *par = ar.fortran_vec ();

  Array<double> ai (dim_vector (n, 1));
  double *pai = ai.fortran_vec ();

  Array<double> beta (dim_vector (n, 1));
  double *pbeta = beta.fortran_vec ();

  octave_idx_type tnvr = calc_rev ? n : 0;
  Matrix vr (tnvr, tnvr);
  double *pvr = vr.fortran_vec ();

  octave_idx_type tnvl = calc_lev ? n : 0;
  Matrix vl (tnvl, tnvl);
  double *pvl = vl.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dggev, DGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           par, pai, pbeta,
                           pvl, n, pvr, n,
                           &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("dggev workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work);
  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dggev, DGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           par, pai, pbeta,
                           pvl, n, pvr, n,
                           pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dggev");

  if (info > 0)
    (*current_liboctave_error_handler) ("dggev failed to converge");

  lambda.resize (n);
  octave_idx_type nvr = calc_rev ? n : 0;
  v.resize (nvr, nvr);

  octave_idx_type nvl = calc_lev ? n : 0;
  w.resize (nvl, nvl);

  for (octave_idx_type j = 0; j < n; j++)
    {
      if (ai.elem (j) == 0.0)
        {
          lambda.elem (j) = Complex (ar.elem (j) / beta.elem (j));
          for (octave_idx_type i = 0; i < nvr; i++)
            v.elem (i, j) = vr.elem (i, j);
          for (octave_idx_type i = 0; i < nvl; i++)
            w.elem (i, j) = vl.elem (i, j);
        }
      else
        {
          if (j+1 >= n)
            (*current_liboctave_error_handler) ("EIG: internal error");

          lambda.elem (j) = Complex (ar.elem (j) / beta.elem (j),
                                     ai.elem (j) / beta.elem (j));
          lambda.elem (j+1) = Complex (ar.elem (j+1) / beta.elem (j+1),
                                       ai.elem (j+1) / beta.elem (j+1));

          for (octave_idx_type i = 0; i < nvr; i++)
            {
              double real_part = vr.elem (i, j);
              double imag_part = vr.elem (i, j+1);
              v.elem (i, j) = Complex (real_part, imag_part);
              v.elem (i, j+1) = Complex (real_part, -imag_part);
            }
          for (octave_idx_type i = 0; i < nvl; i++)
            {
              double real_part = vl.elem (i, j);
              double imag_part = vl.elem (i, j+1);
              w.elem (i, j) = Complex (real_part, imag_part);
              w.elem (i, j+1) = Complex (real_part, -imag_part);
            }
          j++;
        }
    }

  return info;
}

octave_idx_type
EIG::symmetric_init (const Matrix& a, const Matrix& b, bool calc_rev,
                     bool calc_lev)
{
  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  octave_idx_type info = 0;

  Matrix atmp = a;
  double *atmp_data = atmp.fortran_vec ();

  Matrix btmp = b;
  double *btmp_data = btmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  double dummy_work;

  F77_XFCN (dsygv, DSYGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("dsygv workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work);
  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dsygv, DSYGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in dsygv");

  if (info > 0)
    (*current_liboctave_error_handler) ("dsygv failed to converge");

  lambda = ComplexColumnVector (wr);
  v = calc_rev ? ComplexMatrix (atmp) : ComplexMatrix ();
  w = calc_lev ? ComplexMatrix (atmp) : ComplexMatrix ();

  return info;
}

octave_idx_type
EIG::init (const ComplexMatrix& a, const ComplexMatrix& b, bool calc_rev,
           bool calc_lev, bool force_qz)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  octave_idx_type info = 0;

  ComplexMatrix tmp = b;
  Complex*tmp_data = tmp.fortran_vec ();

  if (! force_qz)
    {
      F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                                 n, F77_DBLE_CMPLX_ARG (tmp_data), n,
                                 info
                                 F77_CHAR_ARG_LEN (1)));

      if (a.is_hermitian () && b.is_hermitian () && info == 0)
        return hermitian_init (a, b, calc_rev, calc_lev);
    }

  ComplexMatrix atmp = a;
  Complex *atmp_data = atmp.fortran_vec ();

  ComplexMatrix btmp = b;
  Complex *btmp_data = btmp.fortran_vec ();

  ComplexColumnVector alpha (n);
  Complex *palpha = alpha.fortran_vec ();

  ComplexColumnVector beta (n);
  Complex *pbeta = beta.fortran_vec ();

  octave_idx_type nvr = calc_rev ? n : 0;
  ComplexMatrix vrtmp (nvr, nvr);
  Complex *pvr = vrtmp.fortran_vec ();

  octave_idx_type nvl = calc_lev ? n : 0;
  ComplexMatrix vltmp (nvl, nvl);
  Complex *pvl = vltmp.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 8*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zggev, ZGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, F77_DBLE_CMPLX_ARG (atmp_data), n,
                           F77_DBLE_CMPLX_ARG (btmp_data), n,
                           F77_DBLE_CMPLX_ARG (palpha),
                           F77_DBLE_CMPLX_ARG (pbeta),
                           F77_DBLE_CMPLX_ARG (pvl), n,
                           F77_DBLE_CMPLX_ARG (pvr), n,
                           F77_DBLE_CMPLX_ARG (&dummy_work), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("zggev workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work.real ());
  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  F77_XFCN (zggev, ZGGEV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n,  F77_DBLE_CMPLX_ARG (atmp_data), n,
                           F77_DBLE_CMPLX_ARG (btmp_data), n,
                           F77_DBLE_CMPLX_ARG (palpha),
                           F77_DBLE_CMPLX_ARG (pbeta),
                           F77_DBLE_CMPLX_ARG (pvl), n,
                           F77_DBLE_CMPLX_ARG (pvr), n,
                           F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zggev");

  if (info > 0)
    (*current_liboctave_error_handler) ("zggev failed to converge");

  lambda.resize (n);

  for (octave_idx_type j = 0; j < n; j++)
    lambda.elem (j) = alpha.elem (j) / beta.elem (j);

  v = vrtmp;
  w = vltmp;

  return info;
}

octave_idx_type
EIG::hermitian_init (const ComplexMatrix& a, const ComplexMatrix& b,
                     bool calc_rev, bool calc_lev)
{
  octave_idx_type n = a.rows ();
  octave_idx_type nb = b.rows ();

  if (n != a.cols () || nb != b.cols ())
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  octave_idx_type info = 0;

  ComplexMatrix atmp = a;
  Complex *atmp_data = atmp.fortran_vec ();

  ComplexMatrix btmp = b;
  Complex *btmp_data = btmp.fortran_vec ();

  ColumnVector wr (n);
  double *pwr = wr.fortran_vec ();

  octave_idx_type lwork = -1;
  Complex dummy_work;

  octave_idx_type lrwork = 3*n;
  Array<double> rwork (dim_vector (lrwork, 1));
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (zhegv, ZHEGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_DBLE_CMPLX_ARG (atmp_data), n,
                           F77_DBLE_CMPLX_ARG (btmp_data), n,
                           pwr, F77_DBLE_CMPLX_ARG (&dummy_work), lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("zhegv workspace query failed");

  lwork = static_cast<octave_idx_type> (dummy_work.real ());
  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  F77_XFCN (zhegv, ZHEGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_DBLE_CMPLX_ARG (atmp_data), n,
                           F77_DBLE_CMPLX_ARG (btmp_data), n,
                           pwr, F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zhegv");

  if (info > 0)
    (*current_liboctave_error_handler) ("zhegv failed to converge");

  lambda = ComplexColumnVector (wr);
  v = calc_rev ? ComplexMatrix (atmp) : ComplexMatrix ();
  w = calc_lev ? ComplexMatrix (atmp) : ComplexMatrix ();

  return info;
}
