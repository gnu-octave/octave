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

#include <algorithm>

#include "Array.h"
#include "CColVector.h"
#include "CMatrix.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "lo-qrupdate-proto.h"
#include "oct-cmplx.h"
#include "oct-locbuf.h"
#include "oct-sort.h"
#include "qr.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
qr<T>::qr (const T& q_arg, const T& r_arg)
  : m_q (q_arg), m_r (r_arg)
{
  octave_idx_type q_nr = m_q.rows ();
  octave_idx_type q_nc = m_q.cols ();

  octave_idx_type r_nr = m_r.rows ();
  octave_idx_type r_nc = m_r.cols ();

  if (! (q_nc == r_nr && (q_nr == q_nc || (q_nr > q_nc && r_nr == r_nc))))
    (*current_liboctave_error_handler) ("QR dimensions mismatch");
}

template <typename T>
typename qr<T>::type
qr<T>::get_type (void) const
{
  type retval;

  if (! m_q.isempty () && m_q.issquare ())
    retval = qr<T>::std;
  else if (m_q.rows () > m_q.cols () && m_r.issquare ())
    retval = qr<T>::economy;
  else
    retval = qr<T>::raw;

  return retval;
}

template <typename T>
bool
qr<T>::regular (void) const
{
  bool retval = true;

  octave_idx_type k = std::min (m_r.rows (), m_r.cols ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (m_r(i, i) == ELT_T ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

#if ! defined (HAVE_QRUPDATE)

// Replacement update methods.

void
warn_qrupdate_once (void)
{
  static bool warned = false;

  if (! warned)
    {
      (*current_liboctave_warning_with_id_handler)
        ("Octave:missing-dependency",
         "In this version of Octave, QR & Cholesky updating routines "
         "simply update the matrix and recalculate factorizations. "
         "To use fast algorithms, link Octave with the qrupdate library. "
         "See <http://sourceforge.net/projects/qrupdate>.");

      warned = true;
    }
}

template <typename T>
void
qr<T>::update (const CV_T& u, const CV_T& v)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_q.rows ();
  octave_idx_type n = m_r.cols ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  init (m_q*m_r + T (u) * T (v).hermitian (), get_type ());
}

template <typename T>
void
qr<T>::update (const T& u, const T& v)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_q.rows ();
  octave_idx_type n = m_r.cols ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  init (m_q*m_r + u * v.hermitian (), get_type ());
}

template <typename T, typename CV_T>
static
T
insert_col (const T& a, octave_idx_type i, const CV_T& x)
{
  T retval (a.rows (), a.cols () + 1);
  retval.assign (idx_vector::colon, idx_vector (0, i),
                 a.index (idx_vector::colon, idx_vector (0, i)));
  retval.assign (idx_vector::colon, idx_vector (i), x);
  retval.assign (idx_vector::colon, idx_vector (i+1, retval.cols ()),
                 a.index (idx_vector::colon, idx_vector (i, a.cols ())));
  return retval;
}

template <typename T, typename RV_T>
static
T
insert_row (const T& a, octave_idx_type i, const RV_T& x)
{
  T retval (a.rows () + 1, a.cols ());
  retval.assign (idx_vector (0, i), idx_vector::colon,
                 a.index (idx_vector (0, i), idx_vector::colon));
  retval.assign (idx_vector (i), idx_vector::colon, x);
  retval.assign (idx_vector (i+1, retval.rows ()), idx_vector::colon,
                 a.index (idx_vector (i, a.rows ()), idx_vector::colon));
  return retval;
}

template <typename T>
static
T
delete_col (const T& a, octave_idx_type i)
{
  T retval = a;
  retval.delete_elements (1, idx_vector (i));
  return retval;
}

template <typename T>
static
T
delete_row (const T& a, octave_idx_type i)
{
  T retval = a;
  retval.delete_elements (0, idx_vector (i));
  return retval;
}

template <typename T>
static
T
shift_cols (const T& a, octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = a.cols ();
  Array<octave_idx_type> p (dim_vector (n, 1));
  for (octave_idx_type k = 0; k < n; k++) p(k) = k;
  if (i < j)
    {
      for (octave_idx_type k = i; k < j; k++) p(k) = k+1;
      p(j) = i;
    }
  else if (j < i)
    {
      p(j) = i;
      for (octave_idx_type k = j+1; k < i+1; k++) p(k) = k-1;
    }

  return a.index (idx_vector::colon, idx_vector (p));
}

template <typename T>
void
qr<T>::insert_col (const CV_T& u, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_q.rows ();
  octave_idx_type n = m_r.cols ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  init (math::insert_col (m_q*m_r, j, u), get_type ());
}

template <typename T>
void
qr<T>::insert_col (const T& u, const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_q.rows ();
  octave_idx_type n = m_r.cols ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  if (u.numel () != m || u.cols () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      T a = m_q*m_r;
      for (octave_idx_type i = 0; i < nj; i++)
        a = math::insert_col (a, js(i), u.column (i));

      init (a, get_type ());
    }
}

template <typename T>
void
qr<T>::delete_col (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = m_r.cols ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  init (math::delete_col (m_q*m_r, j), get_type ());
}

template <typename T>
void
qr<T>::delete_col (const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type n = m_r.cols ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  if (nj > 0 && (js(0) > n-1 || js(nj-1) < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      T a = m_q*m_r;
      for (octave_idx_type i = 0; i < nj; i++)
        a = math::delete_col (a, js(i));

      init (a, get_type ());
    }
}

template <typename T>
void
qr<T>::insert_row (const RV_T& u, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_r.rows ();
  octave_idx_type n = m_r.cols ();

  if (! m_q.issquare () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  init (math::insert_row (m_q*m_r, j, u), get_type ());
}

template <typename T>
void
qr<T>::delete_row (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = m_r.rows ();

  if (! m_q.issquare ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");

  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  init (math::delete_row (m_q*m_r, j), get_type ());
}

template <typename T>
void
qr<T>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = m_r.cols ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  init (math::shift_cols (m_q*m_r, i, j), get_type ());
}

#endif

// Specializations.

template <>
OCTAVE_API void
qr<Matrix>::form (octave_idx_type n_arg, Matrix& afact, double *tau,
                  type qr_type)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT m = to_f77_int (afact.rows ());
  F77_INT min_mn = std::min (m, n);
  F77_INT info;

  if (qr_type == qr<Matrix>::raw)
    {
      for (F77_INT j = 0; j < min_mn; j++)
        {
          F77_INT limit = (j < min_mn - 1 ? j : min_mn - 1);
          for (F77_INT i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      m_r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become m_q.
          m_q = afact;
          F77_INT k = (qr_type == qr<Matrix>::economy ? n : m);
          m_r = Matrix (k, n);
          for (F77_INT j = 0; j < n; j++)
            {
              F77_INT i = 0;
              for (; i <= j; i++)
                m_r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                m_r.xelem (i, j) = 0;
            }
          afact = Matrix (); // optimize memory
        }
      else
        {
          // afact will become m_r.
          m_q = Matrix (m, m);
          for (F77_INT j = 0; j < m; j++)
            for (F77_INT i = j + 1; i < m; i++)
              {
                m_q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          m_r = afact;
        }

      if (m > 0)
        {
          F77_INT k = to_f77_int (m_q.cols ());
          // workspace query.
          double rlwork;
          F77_XFCN (dorgqr, DORGQR, (m, k, min_mn, m_q.fortran_vec (), m,
                                     tau, &rlwork, -1, info));

          // allocate buffer and do the job.
          F77_INT lwork = static_cast<F77_INT> (rlwork);
          lwork = std::max (lwork, static_cast<F77_INT> (1));
          OCTAVE_LOCAL_BUFFER (double, work, lwork);
          F77_XFCN (dorgqr, DORGQR, (m, k, min_mn, m_q.fortran_vec (), m,
                                     tau, work, lwork, info));
        }
    }
}

template <>
OCTAVE_API void
qr<Matrix>::init (const Matrix& a, type qr_type)
{
  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (double, tau, min_mn);

  F77_INT info = 0;

  Matrix afact = a;
  if (m > n && qr_type == qr<Matrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      double rlwork;
      F77_XFCN (dgeqrf, DGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (rlwork);
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (double, work, lwork);
      F77_XFCN (dgeqrf, DGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
OCTAVE_API void
qr<Matrix>::update (const ColumnVector& u, const ColumnVector& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  F77_XFCN (dqr1up, DQR1UP, (m, n, k, m_q.fortran_vec (), m,
                             m_r.fortran_vec (), k, utmp.fortran_vec (),
                             vtmp.fortran_vec (), w));
}

template <>
OCTAVE_API void
qr<Matrix>::update (const Matrix& u, const Matrix& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_rows = to_f77_int (u.rows ());
  F77_INT u_cols = to_f77_int (u.cols ());

  F77_INT v_rows = to_f77_int (v.rows ());
  F77_INT v_cols = to_f77_int (v.cols ());

  if (u_rows != m || v_rows != n || u_cols != v_cols)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  for (volatile F77_INT i = 0; i < u_cols; i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dqr1up, DQR1UP, (m, n, k, m_q.fortran_vec (), m,
                                 m_r.fortran_vec (), k, utmp.fortran_vec (),
                                 vtmp.fortran_vec (), w));
    }
}

template <>
OCTAVE_API void
qr<Matrix>::insert_col (const ColumnVector& u, octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());

  if (u_nel != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      m_q.resize (m, k+1);
      m_r.resize (k+1, n+1);
    }
  else
    m_r.resize (k, n+1);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  ColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrinc, DQRINC, (m, n, k, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1,
                             utmp.data (), w));
}

template <>
OCTAVE_API void
qr<Matrix>::insert_col (const Matrix& u, const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT u_cols = to_f77_int (u.cols ());

  if (u_nel != m || u_cols != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg < 0 || js_end > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT kmax = std::min (k + nj, m);
      if (k < m)
        {
          m_q.resize (m, kmax);
          m_r.resize (kmax, n + nj);
        }
      else
        m_r.resize (k, n + nj);

      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (double, w, kmax);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          ColumnVector utmp = u.column (jsi(i));
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (dqrinc, DQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     m_q.fortran_vec (), ldq,
                                     m_r.fortran_vec (), ldr, js_elt + 1,
                                     utmp.data (), w));
        }
    }
}

template <>
OCTAVE_API void
qr<Matrix>::delete_col (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrdec, DQRDEC, (m, n, k, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1, w));

  if (k < m)
    {
      m_q.resize (m, k-1);
      m_r.resize (k-1, n-1);
    }
  else
    m_r.resize (k, n-1);
}

template <>
OCTAVE_API void
qr<Matrix>::delete_col (const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg > n-1 || js_end < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (double, w, k);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (dqrdec, DQRDEC, (m, n - ii, (k == m ? k : k - ii),
                                     m_q.fortran_vec (), ldq,
                                     m_r.fortran_vec (), ldr,
                                     js_elt + 1, w));
        }

      if (k < m)
        {
          m_q.resize (m, k - nj);
          m_r.resize (k - nj, n - nj);
        }
      else
        m_r.resize (k, n - nj);
    }
}

template <>
OCTAVE_API void
qr<Matrix>::insert_row (const RowVector& u, octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = std::min (m, n);

  F77_INT u_nel = to_f77_int (u.numel ());

  if (! m_q.issquare () || u_nel != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  m_q.resize (m + 1, m + 1);
  m_r.resize (m + 1, n);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  RowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrinr, DQRINR, (m, n, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr,
                             j + 1, utmp.fortran_vec (), w));

}

template <>
OCTAVE_API void
qr<Matrix>::delete_row (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (! m_q.issquare ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");

  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (double, w, 2*m);
  F77_XFCN (dqrder, DQRDER, (m, n, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1, w));

  m_q.resize (m - 1, m - 1);
  m_r.resize (m - 1, n);
}

template <>
OCTAVE_API void
qr<Matrix>::shift_cols (octave_idx_type i_arg, octave_idx_type j_arg)
{
  F77_INT i = to_f77_int (i_arg);
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  F77_XFCN (dqrshc, DQRSHC, (m, n, k,
                             m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr,
                             i + 1, j + 1, w));
}

#endif

template <>
OCTAVE_API void
qr<FloatMatrix>::form (octave_idx_type n_arg, FloatMatrix& afact,
                       float *tau, type qr_type)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT m = to_f77_int (afact.rows ());
  F77_INT min_mn = std::min (m, n);
  F77_INT info;

  if (qr_type == qr<FloatMatrix>::raw)
    {
      for (F77_INT j = 0; j < min_mn; j++)
        {
          F77_INT limit = (j < min_mn - 1 ? j : min_mn - 1);
          for (F77_INT i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      m_r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become m_q.
          m_q = afact;
          F77_INT k = (qr_type == qr<FloatMatrix>::economy ? n : m);
          m_r = FloatMatrix (k, n);
          for (F77_INT j = 0; j < n; j++)
            {
              F77_INT i = 0;
              for (; i <= j; i++)
                m_r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                m_r.xelem (i, j) = 0;
            }
          afact = FloatMatrix (); // optimize memory
        }
      else
        {
          // afact will become m_r.
          m_q = FloatMatrix (m, m);
          for (F77_INT j = 0; j < m; j++)
            for (F77_INT i = j + 1; i < m; i++)
              {
                m_q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          m_r = afact;
        }

      if (m > 0)
        {
          F77_INT k = to_f77_int (m_q.cols ());
          // workspace query.
          float rlwork;
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, m_q.fortran_vec (), m,
                                     tau, &rlwork, -1, info));

          // allocate buffer and do the job.
          F77_INT lwork = static_cast<F77_INT> (rlwork);
          lwork = std::max (lwork, static_cast<F77_INT> (1));
          OCTAVE_LOCAL_BUFFER (float, work, lwork);
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, m_q.fortran_vec (), m,
                                     tau, work, lwork, info));
        }
    }
}

template <>
OCTAVE_API void
qr<FloatMatrix>::init (const FloatMatrix& a, type qr_type)
{
  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (float, tau, min_mn);

  F77_INT info = 0;

  FloatMatrix afact = a;
  if (m > n && qr_type == qr<FloatMatrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      float rlwork;
      F77_XFCN (sgeqrf, SGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 &rlwork, -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (rlwork);
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (float, work, lwork);
      F77_XFCN (sgeqrf, SGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
OCTAVE_API void
qr<FloatMatrix>::update (const FloatColumnVector& u, const FloatColumnVector& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  F77_XFCN (sqr1up, SQR1UP, (m, n, k, m_q.fortran_vec (), m,
                             m_r.fortran_vec (), k, utmp.fortran_vec (),
                             vtmp.fortran_vec (), w));
}

template <>
OCTAVE_API void
qr<FloatMatrix>::update (const FloatMatrix& u, const FloatMatrix& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_rows = to_f77_int (u.rows ());
  F77_INT u_cols = to_f77_int (u.cols ());

  F77_INT v_rows = to_f77_int (v.rows ());
  F77_INT v_cols = to_f77_int (v.cols ());

  if (u_rows != m || v_rows != n || u_cols != v_cols)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  for (volatile F77_INT i = 0; i < u_cols; i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (sqr1up, SQR1UP, (m, n, k, m_q.fortran_vec (), m,
                                 m_r.fortran_vec (), k, utmp.fortran_vec (),
                                 vtmp.fortran_vec (), w));
    }
}

template <>
OCTAVE_API void
qr<FloatMatrix>::insert_col (const FloatColumnVector& u,
                             octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());

  if (u_nel != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      m_q.resize (m, k+1);
      m_r.resize (k+1, n+1);
    }
  else
    m_r.resize (k, n+1);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  FloatColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrinc, SQRINC, (m, n, k, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1,
                             utmp.data (), w));
}

template <>
OCTAVE_API void
qr<FloatMatrix>::insert_col (const FloatMatrix& u,
                             const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT u_cols = to_f77_int (u.cols ());

  if (u_nel != m || u_cols != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg < 0 || js_end > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT kmax = std::min (k + nj, m);
      if (k < m)
        {
          m_q.resize (m, kmax);
          m_r.resize (kmax, n + nj);
        }
      else
        m_r.resize (k, n + nj);

      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (float, w, kmax);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          FloatColumnVector utmp = u.column (jsi(i));
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (sqrinc, SQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     m_q.fortran_vec (), ldq,
                                     m_r.fortran_vec (), ldr, js_elt + 1,
                                     utmp.data (), w));
        }
    }
}

template <>
OCTAVE_API void
qr<FloatMatrix>::delete_col (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrdec, SQRDEC, (m, n, k, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1, w));

  if (k < m)
    {
      m_q.resize (m, k-1);
      m_r.resize (k-1, n-1);
    }
  else
    m_r.resize (k, n-1);
}

template <>
OCTAVE_API void
qr<FloatMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg > n-1 || js_end < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (float, w, k);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (sqrdec, SQRDEC, (m, n - ii, (k == m ? k : k - ii),
                                     m_q.fortran_vec (), ldq,
                                     m_r.fortran_vec (), ldr,
                                     js_elt + 1, w));
        }

      if (k < m)
        {
          m_q.resize (m, k - nj);
          m_r.resize (k - nj, n - nj);
        }
      else
        m_r.resize (k, n - nj);
    }
}

template <>
OCTAVE_API void
qr<FloatMatrix>::insert_row (const FloatRowVector& u,
                             octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = std::min (m, n);

  F77_INT u_nel = to_f77_int (u.numel ());

  if (! m_q.issquare () || u_nel != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  m_q.resize (m + 1, m + 1);
  m_r.resize (m + 1, n);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  FloatRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrinr, SQRINR, (m, n, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr,
                             j + 1, utmp.fortran_vec (), w));

}

template <>
OCTAVE_API void
qr<FloatMatrix>::delete_row (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (! m_q.issquare ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");

  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (float, w, 2*m);
  F77_XFCN (sqrder, SQRDER, (m, n, m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr, j + 1,
                             w));

  m_q.resize (m - 1, m - 1);
  m_r.resize (m - 1, n);
}

template <>
OCTAVE_API void
qr<FloatMatrix>::shift_cols (octave_idx_type i_arg, octave_idx_type j_arg)
{
  F77_INT i = to_f77_int (i_arg);
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  F77_XFCN (sqrshc, SQRSHC, (m, n, k,
                             m_q.fortran_vec (), ldq,
                             m_r.fortran_vec (), ldr,
                             i + 1, j + 1, w));
}

#endif

template <>
OCTAVE_API void
qr<ComplexMatrix>::form (octave_idx_type n_arg, ComplexMatrix& afact,
                         Complex *tau, type qr_type)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT m = to_f77_int (afact.rows ());
  F77_INT min_mn = std::min (m, n);
  F77_INT info;

  if (qr_type == qr<ComplexMatrix>::raw)
    {
      for (F77_INT j = 0; j < min_mn; j++)
        {
          F77_INT limit = (j < min_mn - 1 ? j : min_mn - 1);
          for (F77_INT i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      m_r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become m_q.
          m_q = afact;
          F77_INT k = (qr_type == qr<ComplexMatrix>::economy ? n : m);
          m_r = ComplexMatrix (k, n);
          for (F77_INT j = 0; j < n; j++)
            {
              F77_INT i = 0;
              for (; i <= j; i++)
                m_r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                m_r.xelem (i, j) = 0;
            }
          afact = ComplexMatrix (); // optimize memory
        }
      else
        {
          // afact will become m_r.
          m_q = ComplexMatrix (m, m);
          for (F77_INT j = 0; j < m; j++)
            for (F77_INT i = j + 1; i < m; i++)
              {
                m_q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          m_r = afact;
        }

      if (m > 0)
        {
          F77_INT k = to_f77_int (m_q.cols ());
          // workspace query.
          Complex clwork;
          F77_XFCN (zungqr, ZUNGQR, (m, k, min_mn,
                                     F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                                     m, F77_DBLE_CMPLX_ARG (tau),
                                     F77_DBLE_CMPLX_ARG (&clwork), -1,
                                     info));

          // allocate buffer and do the job.
          F77_INT lwork = static_cast<F77_INT> (clwork.real ());
          lwork = std::max (lwork, static_cast<F77_INT> (1));
          OCTAVE_LOCAL_BUFFER (Complex, work, lwork);
          F77_XFCN (zungqr, ZUNGQR, (m, k, min_mn,
                                     F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                                     m, F77_DBLE_CMPLX_ARG (tau),
                                     F77_DBLE_CMPLX_ARG (work), lwork,
                                     info));
        }
    }
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::init (const ComplexMatrix& a, type qr_type)
{
  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (Complex, tau, min_mn);

  F77_INT info = 0;

  ComplexMatrix afact = a;
  if (m > n && qr_type == qr<ComplexMatrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      Complex clwork;
      F77_XFCN (zgeqrf, ZGEQRF, (m, n,
                                 F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                 m, F77_DBLE_CMPLX_ARG (tau),
                                 F77_DBLE_CMPLX_ARG (&clwork), -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (clwork.real ());
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (Complex, work, lwork);
      F77_XFCN (zgeqrf, ZGEQRF, (m, n,
                                 F77_DBLE_CMPLX_ARG (afact.fortran_vec ()),
                                 m, F77_DBLE_CMPLX_ARG (tau),
                                 F77_DBLE_CMPLX_ARG (work), lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
OCTAVE_API void
qr<ComplexMatrix>::update (const ComplexColumnVector& u,
                           const ComplexColumnVector& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqr1up, ZQR1UP, (m, n, k, F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                             m, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()), k,
                             F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()),
                             F77_DBLE_CMPLX_ARG (vtmp.fortran_vec ()),
                             F77_DBLE_CMPLX_ARG (w), rw));
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::update (const ComplexMatrix& u, const ComplexMatrix& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_rows = to_f77_int (u.rows ());
  F77_INT u_cols = to_f77_int (u.cols ());

  F77_INT v_rows = to_f77_int (v.rows ());
  F77_INT v_cols = to_f77_int (v.cols ());

  if (u_rows != m || v_rows != n || u_cols != v_cols)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  for (volatile F77_INT i = 0; i < u_cols; i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zqr1up, ZQR1UP, (m, n, k,
                                 F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                                 m, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()), k,
                                 F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()),
                                 F77_DBLE_CMPLX_ARG (vtmp.fortran_vec ()),
                                 F77_DBLE_CMPLX_ARG (w), rw));
    }
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::insert_col (const ComplexColumnVector& u,
                               octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());

  if (u_nel != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      m_q.resize (m, k+1);
      m_r.resize (k+1, n+1);
    }
  else
    m_r.resize (k, n+1);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  ComplexColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrinc, ZQRINC, (m, n, k, F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                             ldq, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                             ldr, j + 1,
                             F77_CONST_DBLE_CMPLX_ARG (utmp.data ()), rw));
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::insert_col (const ComplexMatrix& u,
                               const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT u_cols = to_f77_int (u.cols ());

  if (u_nel != m || u_cols != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg < 0 || js_end > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT kmax = std::min (k + nj, m);
      if (k < m)
        {
          m_q.resize (m, kmax);
          m_r.resize (kmax, n + nj);
        }
      else
        m_r.resize (k, n + nj);

      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (double, rw, kmax);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          ComplexColumnVector utmp = u.column (jsi(i));
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (zqrinc, ZQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                                     ldq,
                                     F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                                     ldr, js_elt + 1,
                                     F77_CONST_DBLE_CMPLX_ARG (utmp.data ()),
                                     rw));
        }
    }
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::delete_col (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrdec, ZQRDEC, (m, n, k, F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                             ldq, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                             ldr, j + 1, rw));

  if (k < m)
    {
      m_q.resize (m, k-1);
      m_r.resize (k-1, n-1);
    }
  else
    m_r.resize (k, n-1);
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg > n-1 || js_end < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (double, rw, k);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (zqrdec, ZQRDEC, (m, n - ii, (k == m ? k : k - ii),
                                     F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                                     ldq,
                                     F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                                     ldr, js_elt + 1, rw));
        }

      if (k < m)
        {
          m_q.resize (m, k - nj);
          m_r.resize (k - nj, n - nj);
        }
      else
        m_r.resize (k, n - nj);
    }
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::insert_row (const ComplexRowVector& u,
                               octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = std::min (m, n);

  F77_INT u_nel = to_f77_int (u.numel ());

  if (! m_q.issquare () || u_nel != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  m_q.resize (m + 1, m + 1);
  m_r.resize (m + 1, n);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  ComplexRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrinr, ZQRINR, (m, n, F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                             ldq, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                             ldr, j + 1,
                             F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()), rw));

}

template <>
OCTAVE_API void
qr<ComplexMatrix>::delete_row (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (! m_q.issquare ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");

  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  OCTAVE_LOCAL_BUFFER (double, rw, m);
  F77_XFCN (zqrder, ZQRDER, (m, n, F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()),
                             ldq, F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()),
                             ldr, j + 1, F77_DBLE_CMPLX_ARG (w), rw));

  m_q.resize (m - 1, m - 1);
  m_r.resize (m - 1, n);
}

template <>
OCTAVE_API void
qr<ComplexMatrix>::shift_cols (octave_idx_type i_arg,
                               octave_idx_type j_arg)
{
  F77_INT i = to_f77_int (i_arg);
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrshc, ZQRSHC, (m, n, k,
                             F77_DBLE_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_DBLE_CMPLX_ARG (m_r.fortran_vec ()), ldr,
                             i + 1, j + 1, F77_DBLE_CMPLX_ARG (w), rw));
}

#endif

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::form (octave_idx_type n_arg, FloatComplexMatrix& afact,
                              FloatComplex *tau, type qr_type)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT m = to_f77_int (afact.rows ());
  F77_INT min_mn = std::min (m, n);
  F77_INT info;

  if (qr_type == qr<FloatComplexMatrix>::raw)
    {
      for (F77_INT j = 0; j < min_mn; j++)
        {
          F77_INT limit = (j < min_mn - 1 ? j : min_mn - 1);
          for (F77_INT i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      m_r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become m_q.
          m_q = afact;
          F77_INT k = (qr_type == qr<FloatComplexMatrix>::economy ? n : m);
          m_r = FloatComplexMatrix (k, n);
          for (F77_INT j = 0; j < n; j++)
            {
              F77_INT i = 0;
              for (; i <= j; i++)
                m_r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                m_r.xelem (i, j) = 0;
            }
          afact = FloatComplexMatrix (); // optimize memory
        }
      else
        {
          // afact will become m_r.
          m_q = FloatComplexMatrix (m, m);
          for (F77_INT j = 0; j < m; j++)
            for (F77_INT i = j + 1; i < m; i++)
              {
                m_q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          m_r = afact;
        }

      if (m > 0)
        {
          F77_INT k = to_f77_int (m_q.cols ());
          // workspace query.
          FloatComplex clwork;
          F77_XFCN (cungqr, CUNGQR, (m, k, min_mn,
                                     F77_CMPLX_ARG (m_q.fortran_vec ()), m,
                                     F77_CMPLX_ARG (tau),
                                     F77_CMPLX_ARG (&clwork), -1, info));

          // allocate buffer and do the job.
          F77_INT lwork = static_cast<F77_INT> (clwork.real ());
          lwork = std::max (lwork, static_cast<F77_INT> (1));
          OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);
          F77_XFCN (cungqr, CUNGQR, (m, k, min_mn,
                                     F77_CMPLX_ARG (m_q.fortran_vec ()), m,
                                     F77_CMPLX_ARG (tau),
                                     F77_CMPLX_ARG (work), lwork, info));
        }
    }
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::init (const FloatComplexMatrix& a, type qr_type)
{
  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  F77_INT min_mn = (m < n ? m : n);
  OCTAVE_LOCAL_BUFFER (FloatComplex, tau, min_mn);

  F77_INT info = 0;

  FloatComplexMatrix afact = a;
  if (m > n && qr_type == qr<FloatComplexMatrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      FloatComplex clwork;
      F77_XFCN (cgeqrf, CGEQRF, (m, n, F77_CMPLX_ARG (afact.fortran_vec ()),
                                 m, F77_CMPLX_ARG (tau),
                                 F77_CMPLX_ARG (&clwork), -1, info));

      // allocate buffer and do the job.
      F77_INT lwork = static_cast<F77_INT> (clwork.real ());
      lwork = std::max (lwork, static_cast<F77_INT> (1));
      OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);
      F77_XFCN (cgeqrf, CGEQRF, (m, n, F77_CMPLX_ARG (afact.fortran_vec ()),
                                 m, F77_CMPLX_ARG (tau),
                                 F77_CMPLX_ARG (work), lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::update (const FloatComplexColumnVector& u,
                                const FloatComplexColumnVector& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  FloatComplexColumnVector utmp = u;
  FloatComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqr1up, CQR1UP, (m, n, k, F77_CMPLX_ARG (m_q.fortran_vec ()),
                             m, F77_CMPLX_ARG (m_r.fortran_vec ()), k,
                             F77_CMPLX_ARG (utmp.fortran_vec ()),
                             F77_CMPLX_ARG (vtmp.fortran_vec ()),
                             F77_CMPLX_ARG (w), rw));
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::update (const FloatComplexMatrix& u,
                                const FloatComplexMatrix& v)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_rows = to_f77_int (u.rows ());
  F77_INT u_cols = to_f77_int (u.cols ());

  F77_INT v_rows = to_f77_int (v.rows ());
  F77_INT v_cols = to_f77_int (v.cols ());

  if (u_rows != m || v_rows != n || u_cols != v_cols)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  for (volatile F77_INT i = 0; i < u_cols; i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (cqr1up, CQR1UP, (m, n, k, F77_CMPLX_ARG (m_q.fortran_vec ()),
                                 m, F77_CMPLX_ARG (m_r.fortran_vec ()), k,
                                 F77_CMPLX_ARG (utmp.fortran_vec ()),
                                 F77_CMPLX_ARG (vtmp.fortran_vec ()),
                                 F77_CMPLX_ARG (w), rw));
    }
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::insert_col (const FloatComplexColumnVector& u,
                                    octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  F77_INT u_nel = to_f77_int (u.numel ());

  if (u_nel != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      m_q.resize (m, k+1);
      m_r.resize (k+1, n+1);
    }
  else
    m_r.resize (k, n+1);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  FloatComplexColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrinc, CQRINC, (m, n, k, F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_CMPLX_ARG (m_r.fortran_vec ()), ldr, j + 1,
                             F77_CONST_CMPLX_ARG (utmp.data ()), rw));
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::insert_col (const FloatComplexMatrix& u,
                                    const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT u_cols = to_f77_int (u.cols ());

  if (u_nel != m || u_cols != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg < 0 || js_end > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT kmax = std::min (k + nj, m);
      if (k < m)
        {
          m_q.resize (m, kmax);
          m_r.resize (kmax, n + nj);
        }
      else
        m_r.resize (k, n + nj);

      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (float, rw, kmax);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (cqrinc, CQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                                     F77_CMPLX_ARG (m_r.fortran_vec ()), ldr,
                                     js_elt + 1,
                                     F77_CONST_CMPLX_ARG (u.column (jsi(i)).data ()),
                                     rw));
        }
    }
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::delete_col (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrdec, CQRDEC, (m, n, k, F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_CMPLX_ARG (m_r.fortran_vec ()), ldr, j + 1,
                             rw));

  if (k < m)
    {
      m_q.resize (m, k-1);
      m_r.resize (k-1, n-1);
    }
  else
    m_r.resize (k, n-1);
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = to_f77_int (m_q.cols ());

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, DESCENDING);
  F77_INT nj = to_f77_int (js.numel ());
  bool dups = false;
  for (F77_INT i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");

  F77_INT js_beg = to_f77_int (js(0));
  F77_INT js_end = to_f77_int (js(nj-1));

  if (nj > 0 && (js_beg > n-1 || js_end < 0))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      F77_INT ldq = to_f77_int (m_q.rows ());
      F77_INT ldr = to_f77_int (m_r.rows ());

      OCTAVE_LOCAL_BUFFER (float, rw, k);
      for (volatile F77_INT i = 0; i < nj; i++)
        {
          F77_INT ii = i;
          F77_INT js_elt = to_f77_int (js(ii));
          F77_XFCN (cqrdec, CQRDEC, (m, n - ii, (k == m ? k : k - ii),
                                     F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                                     F77_CMPLX_ARG (m_r.fortran_vec ()), ldr,
                                     js_elt + 1, rw));
        }

      if (k < m)
        {
          m_q.resize (m, k - nj);
          m_r.resize (k - nj, n - nj);
        }
      else
        m_r.resize (k, n - nj);
    }
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::insert_row (const FloatComplexRowVector& u,
                                    octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());
  F77_INT k = std::min (m, n);

  F77_INT u_nel = to_f77_int (u.numel ());

  if (! m_q.issquare () || u_nel != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");

  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  m_q.resize (m + 1, m + 1);
  m_r.resize (m + 1, n);

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  FloatComplexRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrinr, CQRINR, (m, n, F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_CMPLX_ARG (m_r.fortran_vec ()), ldr,
                             j + 1, F77_CMPLX_ARG (utmp.fortran_vec ()),
                             rw));

}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::delete_row (octave_idx_type j_arg)
{
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (! m_q.issquare ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");

  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  OCTAVE_LOCAL_BUFFER (float, rw, m);
  F77_XFCN (cqrder, CQRDER, (m, n, F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_CMPLX_ARG (m_r.fortran_vec ()), ldr, j + 1,
                             F77_CMPLX_ARG (w), rw));

  m_q.resize (m - 1, m - 1);
  m_r.resize (m - 1, n);
}

template <>
OCTAVE_API void
qr<FloatComplexMatrix>::shift_cols (octave_idx_type i_arg,
                                    octave_idx_type j_arg)
{
  F77_INT i = to_f77_int (i_arg);
  F77_INT j = to_f77_int (j_arg);

  F77_INT m = to_f77_int (m_q.rows ());
  F77_INT k = to_f77_int (m_r.rows ());
  F77_INT n = to_f77_int (m_r.cols ());

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  F77_INT ldq = to_f77_int (m_q.rows ());
  F77_INT ldr = to_f77_int (m_r.rows ());

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrshc, CQRSHC, (m, n, k,
                             F77_CMPLX_ARG (m_q.fortran_vec ()), ldq,
                             F77_CMPLX_ARG (m_r.fortran_vec ()), ldr,
                             i + 1, j + 1, F77_CMPLX_ARG (w), rw));
}

#endif

// Instantiations we need.

template class qr<Matrix>;

template class qr<FloatMatrix>;

template class qr<ComplexMatrix>;

template class qr<FloatComplexMatrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
