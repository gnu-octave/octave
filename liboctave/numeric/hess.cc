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
#include "CMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "hess.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <>
OCTAVE_API octave_idx_type
hess<Matrix>::init (const Matrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("hess: requires square matrix");

  char job = 'N';
  char side = 'R';

  F77_INT n = a_nc;
  F77_INT lwork = 32 * n;
  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  m_hess_mat = a;
  double *h = m_hess_mat.fortran_vec ();

  Array<double> scale (dim_vector (n, 1));
  double *pscale = scale.fortran_vec ();

  F77_XFCN (dgebal, DGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, h, n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<double> tau (dim_vector (n-1, 1));
  double *ptau = tau.fortran_vec ();

  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  F77_XFCN (dgehrd, DGEHRD, (n, ilo, ihi, h, n, ptau, pwork,
                             lwork, info));

  m_unitary_hess_mat = m_hess_mat;
  double *z = m_unitary_hess_mat.fortran_vec ();

  F77_XFCN (dorghr, DORGHR, (n, ilo, ihi, z, n, ptau, pwork,
                             lwork, info));

  F77_XFCN (dgebak, DGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n, z,
                             n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of doing
  // this (or faster for that matter :-)), please let
  // me know!

  if (n > 2)
    for (F77_INT j = 0; j < a_nc; j++)
      for (F77_INT i = j+2; i < a_nr; i++)
        m_hess_mat.elem (i, j) = 0;

  return info;
}

template <>
OCTAVE_API octave_idx_type
hess<FloatMatrix>::init (const FloatMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("hess: requires square matrix");

  char job = 'N';
  char side = 'R';

  F77_INT n = a_nc;
  F77_INT lwork = 32 * n;
  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  m_hess_mat = a;
  float *h = m_hess_mat.fortran_vec ();

  Array<float> scale (dim_vector (n, 1));
  float *pscale = scale.fortran_vec ();

  F77_XFCN (sgebal, SGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, h, n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<float> tau (dim_vector (n-1, 1));
  float *ptau = tau.fortran_vec ();

  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  F77_XFCN (sgehrd, SGEHRD, (n, ilo, ihi, h, n, ptau, pwork,
                             lwork, info));

  m_unitary_hess_mat = m_hess_mat;
  float *z = m_unitary_hess_mat.fortran_vec ();

  F77_XFCN (sorghr, SORGHR, (n, ilo, ihi, z, n, ptau, pwork,
                             lwork, info));

  F77_XFCN (sgebak, SGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n, z,
                             n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of doing
  // this (or faster for that matter :-)), please let
  // me know!

  if (n > 2)
    for (F77_INT j = 0; j < a_nc; j++)
      for (F77_INT i = j+2; i < a_nr; i++)
        m_hess_mat.elem (i, j) = 0;

  return info;
}

template <>
OCTAVE_API octave_idx_type
hess<ComplexMatrix>::init (const ComplexMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("hess: requires square matrix");

  char job = 'N';
  char side = 'R';

  F77_INT n = a_nc;
  F77_INT lwork = 32 * n;
  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  m_hess_mat = a;
  Complex *h = m_hess_mat.fortran_vec ();

  Array<double> scale (dim_vector (n, 1));
  double *pscale = scale.fortran_vec ();

  F77_XFCN (zgebal, ZGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, F77_DBLE_CMPLX_ARG (h), n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<Complex> tau (dim_vector (n-1, 1));
  Complex *ptau = tau.fortran_vec ();

  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  F77_XFCN (zgehrd, ZGEHRD, (n, ilo, ihi, F77_DBLE_CMPLX_ARG (h), n,
                             F77_DBLE_CMPLX_ARG (ptau), F77_DBLE_CMPLX_ARG (pwork), lwork, info));

  m_unitary_hess_mat = m_hess_mat;
  Complex *z = m_unitary_hess_mat.fortran_vec ();

  F77_XFCN (zunghr, ZUNGHR, (n, ilo, ihi, F77_DBLE_CMPLX_ARG (z), n,
                             F77_DBLE_CMPLX_ARG (ptau), F77_DBLE_CMPLX_ARG (pwork),
                             lwork, info));

  F77_XFCN (zgebak, ZGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n, F77_DBLE_CMPLX_ARG (z), n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of
  // doing this (or faster for that matter :-)),
  // please let me know!

  if (n > 2)
    for (F77_INT j = 0; j < a_nc; j++)
      for (F77_INT i = j+2; i < a_nr; i++)
        m_hess_mat.elem (i, j) = 0;

  return info;
}

template <>
OCTAVE_API octave_idx_type
hess<FloatComplexMatrix>::init (const FloatComplexMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("hess: requires square matrix");
      return -1;
    }

  char job = 'N';
  char side = 'R';

  F77_INT n = a_nc;
  F77_INT lwork = 32 * n;
  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  m_hess_mat = a;
  FloatComplex *h = m_hess_mat.fortran_vec ();

  Array<float> scale (dim_vector (n, 1));
  float *pscale = scale.fortran_vec ();

  F77_XFCN (cgebal, CGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, F77_CMPLX_ARG (h), n, ilo, ihi, pscale, info
                             F77_CHAR_ARG_LEN (1)));

  Array<FloatComplex> tau (dim_vector (n-1, 1));
  FloatComplex *ptau = tau.fortran_vec ();

  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  F77_XFCN (cgehrd, CGEHRD, (n, ilo, ihi, F77_CMPLX_ARG (h), n,
                             F77_CMPLX_ARG (ptau), F77_CMPLX_ARG (pwork), lwork, info));

  m_unitary_hess_mat = m_hess_mat;
  FloatComplex *z = m_unitary_hess_mat.fortran_vec ();

  F77_XFCN (cunghr, CUNGHR, (n, ilo, ihi, F77_CMPLX_ARG (z), n,
                             F77_CMPLX_ARG (ptau), F77_CMPLX_ARG (pwork),
                             lwork, info));

  F77_XFCN (cgebak, CGEBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 (&side, 1),
                             n, ilo, ihi, pscale, n, F77_CMPLX_ARG (z), n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of
  // doing this (or faster for that matter :-)),
  // please let me know!

  if (n > 2)
    for (F77_INT j = 0; j < a_nc; j++)
      for (F77_INT i = j+2; i < a_nr; i++)
        m_hess_mat.elem (i, j) = 0;

  return info;
}

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
