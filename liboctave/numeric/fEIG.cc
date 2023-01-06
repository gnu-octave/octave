////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "Array.h"
#include "fEIG.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"

octave_idx_type
FloatEIG::init (const FloatMatrix& a, bool calc_rev, bool calc_lev,
                bool balance)
{
  if (a.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  if (a.issymmetric ())
    return symmetric_init (a, calc_rev, calc_lev);

  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT a_nc = octave::to_f77_int (a.cols ());

  if (n != a_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  F77_INT info = 0;

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  Array<float> wr (dim_vector (n, 1));
  float *pwr = wr.fortran_vec ();

  Array<float> wi (dim_vector (n, 1));
  float *pwi = wi.fortran_vec ();

  volatile F77_INT nvr = (calc_rev ? n : 0);
  FloatMatrix vr (nvr, nvr);
  float *pvr = vr.fortran_vec ();

  volatile F77_INT nvl = (calc_lev ? n : 0);
  FloatMatrix vl (nvl, nvl);
  float *pvl = vl.fortran_vec ();

  F77_INT lwork = -1;
  float dummy_work;

  F77_INT ilo;
  F77_INT ihi;

  Array<float> scale (dim_vector (n, 1));
  float *pscale = scale.fortran_vec ();

  float abnrm;

  Array<float> rconde (dim_vector (n, 1));
  float *prconde = rconde.fortran_vec ();

  Array<float> rcondv (dim_vector (n, 1));
  float *prcondv = rcondv.fortran_vec ();

  F77_INT dummy_iwork;

  F77_XFCN (sgeevx, SGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, tmp_data, n, pwr, pwi,
                             pvl, n, pvr, n,
                             ilo, ihi, pscale, abnrm, prconde, prcondv,
                             &dummy_work, lwork, &dummy_iwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("sgeevx workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work);
  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (sgeevx, SGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, tmp_data, n, pwr, pwi,
                             pvl, n, pvr, n,
                             ilo, ihi, pscale, abnrm, prconde, prcondv,
                             pwork, lwork, &dummy_iwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in sgeevx");

  if (info > 0)
    (*current_liboctave_error_handler) ("sgeevx failed to converge");

  m_lambda.resize (n);
  m_v.resize (nvr, nvr);
  m_w.resize (nvl, nvl);

  for (F77_INT j = 0; j < n; j++)
    {
      if (wi.elem (j) == 0.0)
        {
          m_lambda.elem (j) = FloatComplex (wr.elem (j));
          for (octave_idx_type i = 0; i < nvr; i++)
            m_v.elem (i, j) = vr.elem (i, j);

          for (F77_INT i = 0; i < nvl; i++)
            m_w.elem (i, j) = vl.elem (i, j);
        }
      else
        {
          if (j+1 >= n)
            (*current_liboctave_error_handler) ("EIG: internal error");

          m_lambda.elem (j) = FloatComplex (wr.elem (j), wi.elem (j));
          m_lambda.elem (j+1) = FloatComplex (wr.elem (j+1), wi.elem (j+1));

          for (F77_INT i = 0; i < nvr; i++)
            {
              float real_part = vr.elem (i, j);
              float imag_part = vr.elem (i, j+1);
              m_v.elem (i, j) = FloatComplex (real_part, imag_part);
              m_v.elem (i, j+1) = FloatComplex (real_part, -imag_part);
            }
          for (F77_INT i = 0; i < nvl; i++)
            {
              float real_part = vl.elem (i, j);
              float imag_part = vl.elem (i, j+1);
              m_w.elem (i, j) = FloatComplex (real_part, imag_part);
              m_w.elem (i, j+1) = FloatComplex (real_part, -imag_part);
            }
          j++;
        }
    }

  return info;
}

octave_idx_type
FloatEIG::symmetric_init (const FloatMatrix& a, bool calc_rev, bool calc_lev)
{
  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT a_nc = octave::to_f77_int (a.cols ());

  if (n != a_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  F77_INT info = 0;

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  F77_INT lwork = -1;
  float dummy_work;

  F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("ssyev workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work);
  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (ssyev, SSYEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, tmp_data, n, pwr, pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in ssyev");

  if (info > 0)
    (*current_liboctave_error_handler) ("ssyev failed to converge");

  m_lambda = FloatComplexColumnVector (wr);
  m_v = (calc_rev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());
  m_w = (calc_lev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());

  return info;
}

octave_idx_type
FloatEIG::init (const FloatComplexMatrix& a, bool calc_rev, bool calc_lev,
                bool balance)
{
  if (a.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  if (a.ishermitian ())
    return hermitian_init (a, calc_rev, calc_lev);

  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT a_nc = octave::to_f77_int (a.cols ());

  if (n != a_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  F77_INT info = 0;

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatComplexColumnVector wr (n);
  FloatComplex *pw = wr.fortran_vec ();

  F77_INT nvr = (calc_rev ? n : 0);
  FloatComplexMatrix vrtmp (nvr, nvr);
  FloatComplex *pvr = vrtmp.fortran_vec ();

  F77_INT nvl = (calc_lev ? n : 0);
  FloatComplexMatrix vltmp (nvl, nvl);
  FloatComplex *pvl = vltmp.fortran_vec ();

  F77_INT lwork = -1;
  FloatComplex dummy_work;

  F77_INT lrwork = 2*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_INT ilo;
  F77_INT ihi;

  Array<float> scale (dim_vector (n, 1));
  float *pscale = scale.fortran_vec ();

  float abnrm;

  Array<float> rconde (dim_vector (n, 1));
  float *prconde = rconde.fortran_vec ();

  Array<float> rcondv (dim_vector (n, 1));
  float *prcondv = rcondv.fortran_vec ();

  F77_XFCN (cgeevx, CGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, F77_CMPLX_ARG (tmp_data), n, F77_CMPLX_ARG (pw),
                             F77_CMPLX_ARG (pvl), n, F77_CMPLX_ARG (pvr), n,
                             ilo, ihi, pscale, abnrm, prconde, prcondv,
                             F77_CMPLX_ARG (&dummy_work), lwork, prwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("cgeevx workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work.real ());
  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  F77_XFCN (cgeevx, CGEEVX, (F77_CONST_CHAR_ARG2 (balance ? "B" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             n, F77_CMPLX_ARG (tmp_data), n, F77_CMPLX_ARG (pw),
                             F77_CMPLX_ARG (pvl), n, F77_CMPLX_ARG (pvr), n,
                             ilo, ihi, pscale, abnrm, prconde, prcondv,
                             F77_CMPLX_ARG (pwork), lwork, prwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in cgeevx");

  if (info > 0)
    (*current_liboctave_error_handler) ("cgeevx failed to converge");

  m_lambda = wr;
  m_v = vrtmp;
  m_w = vltmp;

  return info;
}

octave_idx_type
FloatEIG::hermitian_init (const FloatComplexMatrix& a, bool calc_rev,
                          bool calc_lev)
{
  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT a_nc = octave::to_f77_int (a.cols ());

  if (n != a_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  F77_INT info = 0;

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  F77_INT lwork = -1;
  FloatComplex dummy_work;

  F77_INT lrwork = 3*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_CMPLX_ARG (tmp_data), n, pwr,
                           F77_CMPLX_ARG (&dummy_work), lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("cheev workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work.real ());
  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  F77_XFCN (cheev, CHEEV, (F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_CMPLX_ARG (tmp_data), n, pwr,
                           F77_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in cheev");

  if (info > 0)
    (*current_liboctave_error_handler) ("cheev failed to converge");

  m_lambda = FloatComplexColumnVector (wr);
  m_v = (calc_rev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());
  m_w = (calc_lev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());

  return info;
}

octave_idx_type
FloatEIG::init (const FloatMatrix& a, const FloatMatrix& b, bool calc_rev,
                bool calc_lev, bool force_qz)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT nb = octave::to_f77_int (b.rows ());

  F77_INT a_nc = octave::to_f77_int (a.cols ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (n != a_nc || nb != b_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  F77_INT info = 0;

  FloatMatrix tmp = b;
  float *tmp_data = tmp.fortran_vec ();
  if (! force_qz)
    {
      F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                                 n, tmp_data, n,
                                 info
                                 F77_CHAR_ARG_LEN (1)));

      if (a.issymmetric () && b.issymmetric () && info == 0)
        return symmetric_init (a, b, calc_rev, calc_lev);
    }

  FloatMatrix atmp = a;
  float *atmp_data = atmp.fortran_vec ();

  FloatMatrix btmp = b;
  float *btmp_data = btmp.fortran_vec ();

  Array<float> ar (dim_vector (n, 1));
  float *par = ar.fortran_vec ();

  Array<float> ai (dim_vector (n, 1));
  float *pai = ai.fortran_vec ();

  Array<float> beta (dim_vector (n, 1));
  float *pbeta = beta.fortran_vec ();

  volatile F77_INT nvr = (calc_rev ? n : 0);
  FloatMatrix vr (nvr, nvr);
  float *pvr = vr.fortran_vec ();

  volatile F77_INT nvl = (calc_lev ? n : 0);
  FloatMatrix vl (nvl, nvl);
  float *pvl = vl.fortran_vec ();

  F77_INT lwork = -1;
  float dummy_work;

  F77_XFCN (sggev, SGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           par, pai, pbeta,
                           pvl, n, pvr, n,
                           &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("sggev workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work);
  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (sggev, SGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, atmp_data, n, btmp_data, n,
                           par, pai, pbeta,
                           pvl, n, pvr, n,
                           pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in sggev");

  if (info > 0)
    (*current_liboctave_error_handler) ("sggev failed to converge");

  m_lambda.resize (n);
  m_v.resize (nvr, nvr);
  m_w.resize (nvl, nvl);


  for (F77_INT j = 0; j < n; j++)
    {
      if (ai.elem (j) == 0.0)
        {
          m_lambda.elem (j) = FloatComplex (ar.elem (j) / beta.elem (j));
          for (F77_INT i = 0; i < nvr; i++)
            m_v.elem (i, j) = vr.elem (i, j);

          for (F77_INT i = 0; i < nvl; i++)
            m_w.elem (i, j) = vl.elem (i, j);
        }
      else
        {
          if (j+1 >= n)
            (*current_liboctave_error_handler) ("EIG: internal error");

          m_lambda.elem (j) = FloatComplex (ar.elem (j) / beta.elem (j),
                                            ai.elem (j) / beta.elem (j));
          m_lambda.elem (j+1) = FloatComplex (ar.elem (j+1) / beta.elem (j+1),
                                              ai.elem (j+1) / beta.elem (j+1));

          for (F77_INT i = 0; i < nvr; i++)
            {
              float real_part = vr.elem (i, j);
              float imag_part = vr.elem (i, j+1);
              m_v.elem (i, j) = FloatComplex (real_part, imag_part);
              m_v.elem (i, j+1) = FloatComplex (real_part, -imag_part);
            }
          for (F77_INT i = 0; i < nvl; i++)
            {
              float real_part = vl.elem (i, j);
              float imag_part = vl.elem (i, j+1);
              m_w.elem (i, j) = FloatComplex (real_part, imag_part);
              m_w.elem (i, j+1) = FloatComplex (real_part, -imag_part);
            }
          j++;
        }
    }

  return info;
}

octave_idx_type
FloatEIG::symmetric_init (const FloatMatrix& a, const FloatMatrix& b,
                          bool calc_rev, bool calc_lev)
{
  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT nb = octave::to_f77_int (b.rows ());

  F77_INT a_nc = octave::to_f77_int (a.cols ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (n != a_nc || nb != b_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  F77_INT info = 0;

  FloatMatrix atmp = a;
  float *atmp_data = atmp.fortran_vec ();

  FloatMatrix btmp = b;
  float *btmp_data = btmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  F77_INT lwork = -1;
  float dummy_work;

  F77_XFCN (ssygv, SSYGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, &dummy_work, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("ssygv workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work);
  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (ssygv, SSYGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, atmp_data, n,
                           btmp_data, n,
                           pwr, pwork, lwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in ssygv");

  if (info > 0)
    (*current_liboctave_error_handler) ("ssygv failed to converge");

  m_lambda = FloatComplexColumnVector (wr);
  m_v = (calc_rev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());
  m_w = (calc_lev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());

  return info;
}

octave_idx_type
FloatEIG::init (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
                bool calc_rev, bool calc_lev, bool force_qz)
{
  if (a.any_element_is_inf_or_nan () || b.any_element_is_inf_or_nan ())
    (*current_liboctave_error_handler)
      ("EIG: matrix contains Inf or NaN values");

  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT nb = octave::to_f77_int (b.rows ());

  F77_INT a_nc = octave::to_f77_int (a.cols ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (n != a_nc || nb != b_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  F77_INT info = 0;

  FloatComplexMatrix tmp = b;
  FloatComplex *tmp_data = tmp.fortran_vec ();

  if (! force_qz)
    {
      F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1),
                                 n, F77_CMPLX_ARG (tmp_data), n,
                                 info
                                 F77_CHAR_ARG_LEN (1)));

      if (a.ishermitian () && b.ishermitian () && info == 0)
        return hermitian_init (a, b, calc_rev, calc_lev);
    }

  FloatComplexMatrix atmp = a;
  FloatComplex *atmp_data = atmp.fortran_vec ();

  FloatComplexMatrix btmp = b;
  FloatComplex *btmp_data = btmp.fortran_vec ();

  FloatComplexColumnVector alpha (n);
  FloatComplex *palpha = alpha.fortran_vec ();

  FloatComplexColumnVector beta (n);
  FloatComplex *pbeta = beta.fortran_vec ();

  F77_INT nvr = (calc_rev ? n : 0);
  FloatComplexMatrix vrtmp (nvr, nvr);
  FloatComplex *pvr = vrtmp.fortran_vec ();

  F77_INT nvl = (calc_lev ? n : 0);
  FloatComplexMatrix vltmp (nvl, nvl);
  FloatComplex *pvl = vltmp.fortran_vec ();

  F77_INT lwork = -1;
  FloatComplex dummy_work;

  F77_INT lrwork = 8*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (cggev, CGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, F77_CMPLX_ARG (atmp_data), n,
                           F77_CMPLX_ARG (btmp_data), n,
                           F77_CMPLX_ARG (palpha), F77_CMPLX_ARG (pbeta),
                           F77_CMPLX_ARG (pvl), n, F77_CMPLX_ARG (pvr), n,
                           F77_CMPLX_ARG (&dummy_work), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("cggev workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work.real ());
  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  F77_XFCN (cggev, CGGEV, (F77_CONST_CHAR_ARG2 (calc_lev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           n, F77_CMPLX_ARG (atmp_data), n,
                           F77_CMPLX_ARG (btmp_data), n,
                           F77_CMPLX_ARG (palpha), F77_CMPLX_ARG (pbeta),
                           F77_CMPLX_ARG (pvl), n, F77_CMPLX_ARG (pvr), n,
                           F77_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in cggev");

  if (info > 0)
    (*current_liboctave_error_handler) ("cggev failed to converge");

  m_lambda.resize (n);

  for (F77_INT j = 0; j < n; j++)
    m_lambda.elem (j) = alpha.elem (j) / beta.elem (j);

  m_v = vrtmp;
  m_w = vltmp;

  return info;
}

octave_idx_type
FloatEIG::hermitian_init (const FloatComplexMatrix& a,
                          const FloatComplexMatrix& b,
                          bool calc_rev, bool calc_lev)
{
  F77_INT n = octave::to_f77_int (a.rows ());
  F77_INT nb = octave::to_f77_int (b.rows ());

  F77_INT a_nc = octave::to_f77_int (a.cols ());
  F77_INT b_nc = octave::to_f77_int (b.cols ());

  if (n != a_nc || nb != b_nc)
    (*current_liboctave_error_handler) ("EIG requires square matrix");

  if (n != nb)
    (*current_liboctave_error_handler) ("EIG requires same size matrices");

  F77_INT info = 0;

  FloatComplexMatrix atmp = a;
  FloatComplex *atmp_data = atmp.fortran_vec ();

  FloatComplexMatrix btmp = b;
  FloatComplex *btmp_data = btmp.fortran_vec ();

  FloatColumnVector wr (n);
  float *pwr = wr.fortran_vec ();

  F77_INT lwork = -1;
  FloatComplex dummy_work;

  F77_INT lrwork = 3*n;
  Array<float> rwork (dim_vector (lrwork, 1));
  float *prwork = rwork.fortran_vec ();

  F77_XFCN (chegv, CHEGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_CMPLX_ARG (atmp_data), n,
                           F77_CMPLX_ARG (btmp_data), n,
                           pwr, F77_CMPLX_ARG (&dummy_work), lwork,
                           prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info != 0)
    (*current_liboctave_error_handler) ("zhegv workspace query failed");

  lwork = static_cast<F77_INT> (dummy_work.real ());
  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  F77_XFCN (chegv, CHEGV, (1, F77_CONST_CHAR_ARG2 (calc_rev ? "V" : "N", 1),
                           F77_CONST_CHAR_ARG2 ("U", 1),
                           n, F77_CMPLX_ARG (atmp_data), n,
                           F77_CMPLX_ARG (btmp_data), n,
                           pwr, F77_CMPLX_ARG (pwork), lwork, prwork, info
                           F77_CHAR_ARG_LEN (1)
                           F77_CHAR_ARG_LEN (1)));

  if (info < 0)
    (*current_liboctave_error_handler) ("unrecoverable error in zhegv");

  if (info > 0)
    (*current_liboctave_error_handler) ("zhegv failed to converge");

  m_lambda = FloatComplexColumnVector (wr);
  m_v = (calc_rev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());
  m_w = (calc_lev ? FloatComplexMatrix (atmp) : FloatComplexMatrix ());

  return info;
}
