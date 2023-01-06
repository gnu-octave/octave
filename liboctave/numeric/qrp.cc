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

#include <cassert>

#include <algorithm>

#include "Array.h"
#include "CMatrix.h"
#include "MArray.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"
#include "qrp.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// Specialization.

template <>
OCTAVE_API
void
qrp<Matrix>::init (const Matrix& a, type qr_type)
{
  assert (qr_type != qr<Matrix>::raw);

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (double, tau, min_mn);

  F77_INT info = 0;

  Matrix afact = a;
  if (m > n && qr_type == qr<Matrix>::std)
    afact.resize (m, m);

  MArray<F77_INT> jpvt (dim_vector (n, 1), 0);

  if (m > 0)
    {
      // workspace query.
      double rlwork;
      F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (rlwork);
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (double, work, lwork);

      F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 work, lwork, info));
    }
  else
    {
      for (F77_INT i = 0; i < n; i++)
        jpvt(i) = i+1;
    }

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= static_cast<F77_INT> (1);
  m_p = PermMatrix (jpvt, true);

  form (n, afact, tau, qr_type);
}

template <>
OCTAVE_API
qrp<Matrix>::qrp (const Matrix& a, type qr_type)
  : qr<Matrix> (), m_p ()
{
  init (a, qr_type);
}

template <>
OCTAVE_API
RowVector
qrp<Matrix>::Pvec (void) const
{
  Array<double> pa (m_p.col_perm_vec ());
  RowVector pv (MArray<double> (pa) + 1.0);
  return pv;
}

template <>
OCTAVE_API
void
qrp<FloatMatrix>::init (const FloatMatrix& a, type qr_type)
{
  assert (qr_type != qr<FloatMatrix>::raw);

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (float, tau, min_mn);

  F77_INT info = 0;

  FloatMatrix afact = a;
  if (m > n && qr_type == qr<FloatMatrix>::std)
    afact.resize (m, m);

  MArray<F77_INT> jpvt (dim_vector (n, 1), 0);

  if (m > 0)
    {
      // workspace query.
      float rlwork;
      F77_XFCN (sgeqp3, SGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (rlwork);
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (float, work, lwork);

      F77_XFCN (sgeqp3, SGEQP3, (m, n, afact.fortran_vec (),
                                 m, jpvt.fortran_vec (), tau,
                                 work, lwork, info));
    }
  else
    {
      for (F77_INT i = 0; i < n; i++)
        jpvt(i) = i+1;
    }

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= static_cast<F77_INT> (1);
  m_p = PermMatrix (jpvt, true);

  form (n, afact, tau, qr_type);
}

template <>
OCTAVE_API
qrp<FloatMatrix>::qrp (const FloatMatrix& a, type qr_type)
  : qr<FloatMatrix> (), m_p ()
{
  init (a, qr_type);
}

template <>
OCTAVE_API
FloatRowVector
qrp<FloatMatrix>::Pvec (void) const
{
  Array<float> pa (m_p.col_perm_vec ());
  FloatRowVector pv (MArray<float> (pa) + 1.0f);
  return pv;
}

template <>
OCTAVE_API
void
qrp<ComplexMatrix>::init (const ComplexMatrix& a, type qr_type)
{
  assert (qr_type != qr<ComplexMatrix>::raw);

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (Complex, tau, min_mn);

  F77_INT info = 0;

  ComplexMatrix afact = a;
  if (m > n && qr_type == qr<ComplexMatrix>::std)
    afact.resize (m, m);

  MArray<F77_INT> jpvt (dim_vector (n, 1), 0);

  if (m > 0)
    {
      OCTAVE_LOCAL_BUFFER (double, rwork, 2*n);

      // workspace query.
      Complex clwork;
      F77_XFCN (zgeqp3, ZGEQP3, (m, n,
                                 F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                 m, jpvt.fortran_vec (),
                                 F77_DBLE_CMPLX_ARG (tau),
                                 F77_DBLE_CMPLX_ARG (&clwork),
                                 -1, rwork, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (clwork.real ());
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (Complex, work, lwork);

      F77_XFCN (zgeqp3, ZGEQP3, (m, n,
                                 F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                 m, jpvt.fortran_vec (),
                                 F77_DBLE_CMPLX_ARG (tau),
                                 F77_DBLE_CMPLX_ARG (work),
                                 lwork, rwork, info));
    }
  else
    {
      for (F77_INT i = 0; i < n; i++)
        jpvt(i) = i+1;
    }

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= static_cast<F77_INT> (1);
  m_p = PermMatrix (jpvt, true);

  form (n, afact, tau, qr_type);
}

template <>
OCTAVE_API
qrp<ComplexMatrix>::qrp (const ComplexMatrix& a, type qr_type)
  : qr<ComplexMatrix> (), m_p ()
{
  init (a, qr_type);
}

template <>
OCTAVE_API
RowVector
qrp<ComplexMatrix>::Pvec (void) const
{
  Array<double> pa (m_p.col_perm_vec ());
  RowVector pv (MArray<double> (pa) + 1.0);
  return pv;
}

template <>
OCTAVE_API
void
qrp<FloatComplexMatrix>::init (const FloatComplexMatrix& a, type qr_type)
{
  assert (qr_type != qr<FloatComplexMatrix>::raw);

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (FloatComplex, tau, min_mn);

  F77_INT info = 0;

  FloatComplexMatrix afact = a;
  if (m > n && qr_type == qr<FloatComplexMatrix>::std)
    afact.resize (m, m);

  MArray<F77_INT> jpvt (dim_vector (n, 1), 0);

  if (m > 0)
    {
      OCTAVE_LOCAL_BUFFER (float, rwork, 2*n);

      // workspace query.
      FloatComplex clwork;
      F77_XFCN (cgeqp3, CGEQP3, (m, n,
                                 F77_CMPLX_ARG (afact.fortran_vec ()),
                                 m, jpvt.fortran_vec (),
                                 F77_CMPLX_ARG (tau),
                                 F77_CMPLX_ARG (&clwork),
                                 -1, rwork, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (clwork.real ());
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);

      F77_XFCN (cgeqp3, CGEQP3, (m, n,
                                 F77_CMPLX_ARG (afact.fortran_vec ()),
                                 m, jpvt.fortran_vec (),
                                 F77_CMPLX_ARG (tau),
                                 F77_CMPLX_ARG (work),
                                 lwork, rwork, info));
    }
  else
    {
      for (F77_INT i = 0; i < n; i++)
        jpvt(i) = i+1;
    }

  // Form Permutation matrix (if economy is requested, return the
  // indices only!)

  jpvt -= static_cast<F77_INT> (1);
  m_p = PermMatrix (jpvt, true);

  form (n, afact, tau, qr_type);
}

template <>
OCTAVE_API
qrp<FloatComplexMatrix>::qrp (const FloatComplexMatrix& a, type qr_type)
  : qr<FloatComplexMatrix> (), m_p ()
{
  init (a, qr_type);
}

template <>
OCTAVE_API
FloatRowVector
qrp<FloatComplexMatrix>::Pvec (void) const
{
  Array<float> pa (m_p.col_perm_vec ());
  FloatRowVector pv (MArray<float> (pa) + 1.0f);
  return pv;
}

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
