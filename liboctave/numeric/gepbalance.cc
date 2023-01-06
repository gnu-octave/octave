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

#include "CMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "gepbalance.h"
#include "lo-array-errwarn.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"
#include "quit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <>
OCTAVE_API octave_idx_type
gepbalance<Matrix>::init (const Matrix& a, const Matrix& b,
                          const std::string& balance_job)
{
  F77_INT n = to_f77_int (a.cols ());

  if (a.rows () != n)
    (*current_liboctave_error_handler)
      ("GEPBALANCE requires square matrix");

  if (a.dims () != b.dims ())
    err_nonconformant ("GEPBALANCE", n, n, b.rows(), b.cols());

  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  OCTAVE_LOCAL_BUFFER (double, plscale, n);
  OCTAVE_LOCAL_BUFFER (double, prscale, n);
  OCTAVE_LOCAL_BUFFER (double, pwork, 6 * n);

  m_balanced_mat = a;
  double *p_balanced_mat = m_balanced_mat.fortran_vec ();
  m_balanced_mat2 = b;
  double *p_balanced_mat2 = m_balanced_mat2.fortran_vec ();

  char job = balance_job[0];

  F77_XFCN (dggbal, DGGBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, p_balanced_mat, n, p_balanced_mat2,
                             n, ilo, ihi, plscale, prscale, pwork, info
                             F77_CHAR_ARG_LEN  (1)));

  m_balancing_mat = Matrix (n, n, 0.0);
  m_balancing_mat2 = Matrix (n, n, 0.0);
  for (F77_INT i = 0; i < n; i++)
    {
      octave_quit ();
      m_balancing_mat.elem (i, i) = 1.0;
      m_balancing_mat2.elem (i, i) = 1.0;
    }

  double *p_balancing_mat = m_balancing_mat.fortran_vec ();
  double *p_balancing_mat2 = m_balancing_mat2.fortran_vec ();

  // first left
  F77_XFCN (dggbak, DGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("L", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // then right
  F77_XFCN (dggbak, DGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("R", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat2, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
OCTAVE_API octave_idx_type
gepbalance<FloatMatrix>::init (const FloatMatrix& a, const FloatMatrix& b,
                               const std::string& balance_job)
{
  F77_INT n = to_f77_int (a.cols ());

  if (a.rows () != n)
    (*current_liboctave_error_handler)
      ("FloatGEPBALANCE requires square matrix");

  if (a.dims () != b.dims ())
    err_nonconformant ("FloatGEPBALANCE",
                       n, n, b.rows(), b.cols());

  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  OCTAVE_LOCAL_BUFFER (float, plscale, n);
  OCTAVE_LOCAL_BUFFER (float, prscale, n);
  OCTAVE_LOCAL_BUFFER (float, pwork, 6 * n);

  m_balanced_mat = a;
  float *p_balanced_mat = m_balanced_mat.fortran_vec ();
  m_balanced_mat2 = b;
  float *p_balanced_mat2 = m_balanced_mat2.fortran_vec ();

  char job = balance_job[0];

  F77_XFCN (sggbal, SGGBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, p_balanced_mat, n, p_balanced_mat2,
                             n, ilo, ihi, plscale, prscale, pwork, info
                             F77_CHAR_ARG_LEN  (1)));

  m_balancing_mat = FloatMatrix (n, n, 0.0);
  m_balancing_mat2 = FloatMatrix (n, n, 0.0);
  for (F77_INT i = 0; i < n; i++)
    {
      octave_quit ();
      m_balancing_mat.elem (i, i) = 1.0;
      m_balancing_mat2.elem (i, i) = 1.0;
    }

  float *p_balancing_mat = m_balancing_mat.fortran_vec ();
  float *p_balancing_mat2 = m_balancing_mat2.fortran_vec ();

  // first left
  F77_XFCN (sggbak, SGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("L", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // then right
  F77_XFCN (sggbak, SGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("R", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat2, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
OCTAVE_API octave_idx_type
gepbalance<ComplexMatrix>::init (const ComplexMatrix& a,
                                 const ComplexMatrix& b,
                                 const std::string& balance_job)
{
  F77_INT n = to_f77_int (a.cols ());

  if (a.rows () != n)
    (*current_liboctave_error_handler)
      ("ComplexGEPBALANCE requires square matrix");

  if (a.dims () != b.dims ())
    err_nonconformant ("ComplexGEPBALANCE",
                       n, n, b.rows(), b.cols());

  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  OCTAVE_LOCAL_BUFFER (double, plscale, n);
  OCTAVE_LOCAL_BUFFER (double, prscale,  n);
  OCTAVE_LOCAL_BUFFER (double, pwork, 6 * n);

  m_balanced_mat = a;
  Complex *p_balanced_mat = m_balanced_mat.fortran_vec ();
  m_balanced_mat2 = b;
  Complex *p_balanced_mat2 = m_balanced_mat2.fortran_vec ();

  char job = balance_job[0];

  F77_XFCN (zggbal, ZGGBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, F77_DBLE_CMPLX_ARG (p_balanced_mat),
                             n, F77_DBLE_CMPLX_ARG (p_balanced_mat2),
                             n, ilo, ihi, plscale, prscale, pwork, info
                             F77_CHAR_ARG_LEN (1)));

  m_balancing_mat = Matrix (n, n, 0.0);
  m_balancing_mat2 = Matrix (n, n, 0.0);
  for (F77_INT i = 0; i < n; i++)
    {
      octave_quit ();
      m_balancing_mat.elem (i, i) = 1.0;
      m_balancing_mat2.elem (i, i) = 1.0;
    }

  double *p_balancing_mat = m_balancing_mat.fortran_vec ();
  double *p_balancing_mat2 = m_balancing_mat2.fortran_vec ();

  // first left
  F77_XFCN (dggbak, DGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("L", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // then right
  F77_XFCN (dggbak, DGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("R", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat2, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
OCTAVE_API octave_idx_type
gepbalance<FloatComplexMatrix>::init (const FloatComplexMatrix& a,
                                      const FloatComplexMatrix& b,
                                      const std::string& balance_job)
{
  F77_INT n = to_f77_int (a.cols ());

  if (a.rows () != n)
    {
      (*current_liboctave_error_handler)
        ("FloatComplexGEPBALANCE requires square matrix");
      return -1;
    }

  if (a.dims () != b.dims ())
    err_nonconformant ("FloatComplexGEPBALANCE",
                       n, n, b.rows(), b.cols());

  F77_INT info;
  F77_INT ilo;
  F77_INT ihi;

  OCTAVE_LOCAL_BUFFER (float, plscale, n);
  OCTAVE_LOCAL_BUFFER (float, prscale, n);
  OCTAVE_LOCAL_BUFFER (float, pwork, 6 * n);

  m_balanced_mat = a;
  FloatComplex *p_balanced_mat = m_balanced_mat.fortran_vec ();
  m_balanced_mat2 = b;
  FloatComplex *p_balanced_mat2 = m_balanced_mat2.fortran_vec ();

  char job = balance_job[0];

  F77_XFCN (cggbal, CGGBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
                             n, F77_CMPLX_ARG (p_balanced_mat),
                             n, F77_CMPLX_ARG (p_balanced_mat2),
                             n, ilo, ihi, plscale, prscale, pwork, info
                             F77_CHAR_ARG_LEN (1)));

  m_balancing_mat = FloatMatrix (n, n, 0.0);
  m_balancing_mat2 = FloatMatrix (n, n, 0.0);
  for (F77_INT i = 0; i < n; i++)
    {
      octave_quit ();
      m_balancing_mat.elem (i, i) = 1.0;
      m_balancing_mat2.elem (i, i) = 1.0;
    }

  float *p_balancing_mat = m_balancing_mat.fortran_vec ();
  float *p_balancing_mat2 = m_balancing_mat2.fortran_vec ();

  // first left
  F77_XFCN (sggbak, SGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("L", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // then right
  F77_XFCN (sggbak, SGGBAK, (F77_CONST_CHAR_ARG2 (&job, 1),
                             F77_CONST_CHAR_ARG2 ("R", 1),
                             n, ilo, ihi, plscale, prscale,
                             n, p_balancing_mat2, n, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

// Instantiations we need.

template class gepbalance<Matrix>;

template class gepbalance<FloatMatrix>;

template class gepbalance<ComplexMatrix>;

template class gepbalance<FloatComplexMatrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
