/*

Copyright (C) 1994-2016 John W. Eatonn
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>

#include "CMatrix.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"
#include "qrp.h"

namespace octave
{
  namespace math
  {
    // Specialization.

    template <>
    void
    qrp<Matrix>::init (const Matrix& a, type qr_type)
    {
      assert (qr_type != qr<Matrix>::raw);

      octave_idx_type m = a.rows ();
      octave_idx_type n = a.cols ();

      octave_idx_type min_mn = m < n ? m : n;
      OCTAVE_LOCAL_BUFFER (double, tau, min_mn);

      octave_idx_type info = 0;

      Matrix afact = a;
      if (m > n && qr_type == qr<Matrix>::std)
        afact.resize (m, m);

      MArray<octave_idx_type> jpvt (dim_vector (n, 1), 0);

      if (m > 0)
        {
          // workspace query.
          double rlwork;
          F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                     m, jpvt.fortran_vec (), tau,
                                     &rlwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = rlwork;
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (double, work, lwork);
          F77_XFCN (dgeqp3, DGEQP3, (m, n, afact.fortran_vec (),
                                     m, jpvt.fortran_vec (), tau,
                                     work, lwork, info));
        }
      else
        for (octave_idx_type i = 0; i < n; i++) jpvt(i) = i+1;

      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      jpvt -= static_cast<octave_idx_type> (1);
      p = PermMatrix (jpvt, true);

      form (n, afact, tau, qr_type);
    }

    template <>
    qrp<Matrix>::qrp (const Matrix& a, type qr_type)
      : qr<Matrix> (), p ()
    {
      init (a, qr_type);
    }

    template <>
    RowVector
    qrp<Matrix>::Pvec (void) const
    {
      Array<double> pa (p.col_perm_vec ());
      RowVector pv (MArray<double> (pa) + 1.0);
      return pv;
    }

    template <>
    void
    qrp<FloatMatrix>::init (const FloatMatrix& a, type qr_type)
    {
      assert (qr_type != qr<FloatMatrix>::raw);

      octave_idx_type m = a.rows ();
      octave_idx_type n = a.cols ();

      octave_idx_type min_mn = m < n ? m : n;
      OCTAVE_LOCAL_BUFFER (float, tau, min_mn);

      octave_idx_type info = 0;

      FloatMatrix afact = a;
      if (m > n && qr_type == qr<FloatMatrix>::std)
        afact.resize (m, m);

      MArray<octave_idx_type> jpvt (dim_vector (n, 1), 0);

      if (m > 0)
        {
          // workspace query.
          float rlwork;
          F77_XFCN (sgeqp3, SGEQP3, (m, n, afact.fortran_vec (),
                                     m, jpvt.fortran_vec (), tau,
                                     &rlwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = rlwork;
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (float, work, lwork);
          F77_XFCN (sgeqp3, SGEQP3, (m, n, afact.fortran_vec (),
                                     m, jpvt.fortran_vec (), tau,
                                     work, lwork, info));
        }
      else
        for (octave_idx_type i = 0; i < n; i++) jpvt(i) = i+1;

      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      jpvt -= static_cast<octave_idx_type> (1);
      p = PermMatrix (jpvt, true);

      form (n, afact, tau, qr_type);
    }

    template <>
    qrp<FloatMatrix>::qrp (const FloatMatrix& a, type qr_type)
      : qr<FloatMatrix> (), p ()
    {
      init (a, qr_type);
    }

    template <>
    FloatRowVector
    qrp<FloatMatrix>::Pvec (void) const
    {
      Array<float> pa (p.col_perm_vec ());
      FloatRowVector pv (MArray<float> (pa) + 1.0f);
      return pv;
    }

    template <>
    void
    qrp<ComplexMatrix>::init (const ComplexMatrix& a, type qr_type)
    {
      assert (qr_type != qr<ComplexMatrix>::raw);

      octave_idx_type m = a.rows ();
      octave_idx_type n = a.cols ();

      octave_idx_type min_mn = m < n ? m : n;
      OCTAVE_LOCAL_BUFFER (Complex, tau, min_mn);

      octave_idx_type info = 0;

      ComplexMatrix afact = a;
      if (m > n && qr_type == qr<ComplexMatrix>::std)
        afact.resize (m, m);

      MArray<octave_idx_type> jpvt (dim_vector (n, 1), 0);

      if (m > 0)
        {
          OCTAVE_LOCAL_BUFFER (double, rwork, 2*n);

          // workspace query.
          Complex clwork;
          F77_XFCN (zgeqp3, ZGEQP3, (m, n, F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                     m, jpvt.fortran_vec (), F77_DBLE_CMPLX_ARG (tau),
                                     F77_DBLE_CMPLX_ARG (&clwork), -1, rwork, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = clwork.real ();
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (Complex, work, lwork);
          F77_XFCN (zgeqp3, ZGEQP3, (m, n, F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                     m, jpvt.fortran_vec (), F77_DBLE_CMPLX_ARG (tau),
                                     F77_DBLE_CMPLX_ARG (work), lwork, rwork, info));
        }
      else
        for (octave_idx_type i = 0; i < n; i++) jpvt(i) = i+1;

      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      jpvt -= static_cast<octave_idx_type> (1);
      p = PermMatrix (jpvt, true);

      form (n, afact, tau, qr_type);
    }

    template <>
    qrp<ComplexMatrix>::qrp (const ComplexMatrix& a, type qr_type)
      : qr<ComplexMatrix> (), p ()
    {
      init (a, qr_type);
    }

    template <>
    RowVector
    qrp<ComplexMatrix>::Pvec (void) const
    {
      Array<double> pa (p.col_perm_vec ());
      RowVector pv (MArray<double> (pa) + 1.0);
      return pv;
    }

    template <>
    void
    qrp<FloatComplexMatrix>::init (const FloatComplexMatrix& a, type qr_type)
    {
      assert (qr_type != qr<FloatComplexMatrix>::raw);

      octave_idx_type m = a.rows ();
      octave_idx_type n = a.cols ();

      octave_idx_type min_mn = m < n ? m : n;
      OCTAVE_LOCAL_BUFFER (FloatComplex, tau, min_mn);

      octave_idx_type info = 0;

      FloatComplexMatrix afact = a;
      if (m > n && qr_type == qr<FloatComplexMatrix>::std)
        afact.resize (m, m);

      MArray<octave_idx_type> jpvt (dim_vector (n, 1), 0);

      if (m > 0)
        {
          OCTAVE_LOCAL_BUFFER (float, rwork, 2*n);

          // workspace query.
          FloatComplex clwork;
          F77_XFCN (cgeqp3, CGEQP3, (m, n, F77_CMPLX_ARG (afact.fortran_vec ()),
                                     m, jpvt.fortran_vec (), F77_CMPLX_ARG (tau),
                                     F77_CMPLX_ARG (&clwork), -1, rwork, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = clwork.real ();
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);
          F77_XFCN (cgeqp3, CGEQP3, (m, n, F77_CMPLX_ARG (afact.fortran_vec ()),
                                     m, jpvt.fortran_vec (), F77_CMPLX_ARG (tau),
                                     F77_CMPLX_ARG (work), lwork, rwork, info));
        }
      else
        for (octave_idx_type i = 0; i < n; i++) jpvt(i) = i+1;

      // Form Permutation matrix (if economy is requested, return the
      // indices only!)

      jpvt -= static_cast<octave_idx_type> (1);
      p = PermMatrix (jpvt, true);

      form (n, afact, tau, qr_type);
    }

    template <>
    qrp<FloatComplexMatrix>::qrp (const FloatComplexMatrix& a, type qr_type)
      : qr<FloatComplexMatrix> (), p ()
    {
      init (a, qr_type);
    }

    template <>
    FloatRowVector
    qrp<FloatComplexMatrix>::Pvec (void) const
    {
      Array<float> pa (p.col_perm_vec ());
      FloatRowVector pv (MArray<float> (pa) + 1.0f);
      return pv;
    }
  }
}

