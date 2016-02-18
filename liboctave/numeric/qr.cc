/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague

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
#  include "config.h"
#endif

#include "CColVector.h"
#include "CMatrix.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "f77-fcn.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "qr.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeqrf, DGEQRF) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&, double*,
                             double*, const octave_idx_type&,
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (dorgqr, DORGQR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*, double*,
                             const octave_idx_type&, octave_idx_type&);

#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (dqr1up, DQR1UP) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*, double*, double*);

  F77_RET_T
  F77_FUNC (dqrinc, DQRINC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, double*);

  F77_RET_T
  F77_FUNC (dqrdec, DQRDEC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type&,
                             double*);

  F77_RET_T
  F77_FUNC (dqrinr, DQRINR) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, double*);

  F77_RET_T
  F77_FUNC (dqrder, DQRDER) (const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type&,
                             double*);

  F77_RET_T
  F77_FUNC (dqrshc, DQRSHC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, double*);

#endif

  F77_RET_T
  F77_FUNC (sgeqrf, SGEQRF) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (sorgqr, SORGQR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*,
                             const octave_idx_type&, octave_idx_type&);

#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (sqr1up, SQR1UP) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*, float*, float*);

  F77_RET_T
  F77_FUNC (sqrinc, SQRINC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, const float*, float*);

  F77_RET_T
  F77_FUNC (sqrdec, SQRDEC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, float*);

  F77_RET_T
  F77_FUNC (sqrinr, SQRINR) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             const octave_idx_type&, const float*, float*);

  F77_RET_T
  F77_FUNC (sqrder, SQRDER) (const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             float*, const octave_idx_type&,
                             const octave_idx_type&, float*);

  F77_RET_T
  F77_FUNC (sqrshc, SQRSHC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             float*);

#endif

  F77_RET_T
  F77_FUNC (zgeqrf, ZGEQRF) (const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*,
                             Complex*, const octave_idx_type&,
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (zungqr, ZUNGQR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*, Complex*,
                             const octave_idx_type&, octave_idx_type&);

#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (zqr1up, ZQR1UP) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*,
                             Complex*, Complex*, double*);

  F77_RET_T
  F77_FUNC (zqrinc, ZQRINC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, const octave_idx_type&,
                             const Complex*, double*);

  F77_RET_T
  F77_FUNC (zqrdec, ZQRDEC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, const octave_idx_type&,
                             double*);

  F77_RET_T
  F77_FUNC (zqrinr, ZQRINR) (const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, const octave_idx_type&,
                             const Complex*, double*);

  F77_RET_T
  F77_FUNC (zqrder, ZQRDER) (const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, const octave_idx_type&,
                             Complex*, double*);

  F77_RET_T
  F77_FUNC (zqrshc, ZQRSHC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, Complex*,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, Complex*, double*);

#endif

  F77_RET_T
  F77_FUNC (cgeqrf, CGEQRF) (const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cungqr, CUNGQR) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             FloatComplex*, const octave_idx_type&,
                             octave_idx_type&);

#ifdef HAVE_QRUPDATE

  F77_RET_T
  F77_FUNC (cqr1up, CQR1UP) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             FloatComplex*, FloatComplex*, float*);

  F77_RET_T
  F77_FUNC (cqrinc, CQRINC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&,const octave_idx_type&,
                             const FloatComplex*, float*);

  F77_RET_T
  F77_FUNC (cqrdec, CQRDEC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type&,
                             float*);

  F77_RET_T
  F77_FUNC (cqrinr, CQRINR) (const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             const octave_idx_type&, const FloatComplex*,
                             float*);

  F77_RET_T
  F77_FUNC (cqrder, CQRDER) (const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*, float*);

  F77_RET_T
  F77_FUNC (cqrshc, CQRSHC) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             float*);

#endif
}

template <typename T>
qr<T>::qr (const T& q_arg, const T& r_arg)
  : q (q_arg), r (r_arg)
{
  octave_idx_type q_nr = q.rows ();
  octave_idx_type q_nc = q.columns ();

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.columns ();

  if (! (q_nc == r_nr && (q_nr == q_nc || (q_nr > q_nc && r_nr == r_nc))))
    (*current_liboctave_error_handler) ("QR dimensions mismatch");
}

template <typename T>
typename qr<T>::type
qr<T>::get_type (void) const
{
  type retval;

  if (! q.is_empty () && q.is_square ())
    retval = qr<T>::std;
  else if (q.rows () > q.columns () && r.is_square ())
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

  octave_idx_type k = std::min (r.rows (), r.columns ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (r(i, i) == ELT_T ())
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

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  init (q*r + T (u) * T (v).hermitian (), get_type ());
}

template <typename T>
void
qr<T>::update (const T& u, const T& v)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  init (q*r + u * v.hermitian (), get_type ());
}

template <typename T, typename CV_T>
static
T
insert_col (const T& a, octave_idx_type i, const CV_T& x)
{
  T retval (a.rows (), a.columns () + 1);
  retval.assign (idx_vector::colon, idx_vector (0, i),
                 a.index (idx_vector::colon, idx_vector (0, i)));
  retval.assign (idx_vector::colon, idx_vector (i), x);
  retval.assign (idx_vector::colon, idx_vector (i+1, retval.columns ()),
                 a.index (idx_vector::colon, idx_vector (i, a.columns ())));
  return retval;
}

template <typename T, typename RV_T>
static
T
insert_row (const T& a, octave_idx_type i, const RV_T& x)
{
  T retval (a.rows () + 1, a.columns ());
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
  octave_idx_type n = a.columns ();
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

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  init (::insert_col (q*r, j, u), get_type ());
}

template <typename T>
void
qr<T>::insert_col (const T& u, const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  if (u.numel () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      T a = q*r;
      for (octave_idx_type i = 0; i < js.numel (); i++)
        a = ::insert_col (a, js(i), u.column (i));
      init (a, get_type ());
    }
}

template <typename T>
void
qr<T>::delete_col (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  init (::delete_col (q*r, j), get_type ());
}

template <typename T>
void
qr<T>::delete_col (const Array<octave_idx_type>& j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

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
      T a = q*r;
      for (octave_idx_type i = 0; i < js.numel (); i++)
        a = ::delete_col (a, js(i));
      init (a, get_type ());
    }
}

template <typename T>
void
qr<T>::insert_row (const RV_T& u, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  init (::insert_row (q*r, j, u), get_type ());
}

template <typename T>
void
qr<T>::delete_row (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type m = r.rows ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  init (::delete_row (q*r, j), get_type ());
}

template <typename T>
void
qr<T>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  init (::shift_cols (q*r, i, j), get_type ());
}

#endif

// Specializations.

template <>
void
qr<Matrix>::form (octave_idx_type n, Matrix& afact, double *tau, type qr_type)
{
  octave_idx_type m = afact.rows ();
  octave_idx_type min_mn = std::min (m, n);
  octave_idx_type info;

  if (qr_type == qr<Matrix>::raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
        {
          octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
          for (octave_idx_type i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become q.
          q = afact;
          octave_idx_type k = qr_type == qr<Matrix>::economy ? n : m;
          r = Matrix (k, n);
          for (octave_idx_type j = 0; j < n; j++)
            {
              octave_idx_type i = 0;
              for (; i <= j; i++)
                r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                r.xelem (i, j) = 0;
            }
          afact = Matrix (); // optimize memory
        }
      else
        {
          // afact will become r.
          q = Matrix (m, m);
          for (octave_idx_type j = 0; j < m; j++)
            for (octave_idx_type i = j + 1; i < m; i++)
              {
                q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          r = afact;
        }


      if (m > 0)
        {
          octave_idx_type k = q.columns ();
          // workspace query.
          double rlwork;
          F77_XFCN (dorgqr, DORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     &rlwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = rlwork;
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (double, work, lwork);
          F77_XFCN (dorgqr, DORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     work, lwork, info));
        }
    }
}

template <>
void
qr<Matrix>::init (const Matrix& a, type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (double, tau, min_mn);

  octave_idx_type info = 0;

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
      octave_idx_type lwork = rlwork;
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (double, work, lwork);
      F77_XFCN (dgeqrf, DGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
void
qr<Matrix>::update (const ColumnVector& u, const ColumnVector& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  F77_XFCN (dqr1up, DQR1UP, (m, n, k, q.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec (), w));
}

template <>
void
qr<Matrix>::update (const Matrix& u, const Matrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dqr1up, DQR1UP, (m, n, k, q.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec (),
                                 w));
    }
}

template <>
void
qr<Matrix>::insert_col (const ColumnVector& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      q.resize (m, k+1);
      r.resize (k+1, n+1);
    }
  else
    {
      r.resize (k, n+1);
    }

  ColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrinc, DQRINC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             utmp.data (), w));
}

template <>
void
qr<Matrix>::insert_col (const Matrix& u, const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  if (u.numel () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      octave_idx_type kmax = std::min (k + nj, m);
      if (k < m)
        {
          q.resize (m, kmax);
          r.resize (kmax, n + nj);
        }
      else
        {
          r.resize (k, n + nj);
        }

      OCTAVE_LOCAL_BUFFER (double, w, kmax);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          ColumnVector utmp = u.column (jsi(i));
          F77_XFCN (dqrinc, DQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (), js(ii) + 1,
                                     utmp.data (), w));
        }
    }
}

template <>
void
qr<Matrix>::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrdec, DQRDEC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1, w));

  if (k < m)
    {
      q.resize (m, k-1);
      r.resize (k-1, n-1);
    }
  else
    {
      r.resize (k, n-1);
    }
}

template <>
void
qr<Matrix>::delete_col (const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

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
      OCTAVE_LOCAL_BUFFER (double, w, k);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (dqrdec, DQRDEC, (m, n - ii, k == m ? k : k - ii,
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (),
                                     js(ii) + 1, w));
        }
      if (k < m)
        {
          q.resize (m, k - nj);
          r.resize (k - nj, n - nj);
        }
      else
        {
          r.resize (k, n - nj);
        }

    }
}

template <>
void
qr<Matrix>::insert_row (const RowVector& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = std::min (m, n);

  if (! q.is_square () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  q.resize (m + 1, m + 1);
  r.resize (m + 1, n);
  RowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, w, k);
  F77_XFCN (dqrinr, DQRINR, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             j + 1, utmp.fortran_vec (), w));

}

template <>
void
qr<Matrix>::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (double, w, 2*m);
  F77_XFCN (dqrder, DQRDER, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             w));

  q.resize (m - 1, m - 1);
  r.resize (m - 1, n);
}

template <>
void
qr<Matrix>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  OCTAVE_LOCAL_BUFFER (double, w, 2*k);
  F77_XFCN (dqrshc, DQRSHC, (m, n, k,
                             q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             i + 1, j + 1, w));
}

#endif

template <>
void
qr<FloatMatrix>::form (octave_idx_type n, FloatMatrix& afact, float *tau, type qr_type)
{
  octave_idx_type m = afact.rows ();
  octave_idx_type min_mn = std::min (m, n);
  octave_idx_type info;

  if (qr_type == qr<FloatMatrix>::raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
        {
          octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
          for (octave_idx_type i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become q.
          q = afact;
          octave_idx_type k = qr_type == qr<FloatMatrix>::economy ? n : m;
          r = FloatMatrix (k, n);
          for (octave_idx_type j = 0; j < n; j++)
            {
              octave_idx_type i = 0;
              for (; i <= j; i++)
                r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                r.xelem (i, j) = 0;
            }
          afact = FloatMatrix (); // optimize memory
        }
      else
        {
          // afact will become r.
          q = FloatMatrix (m, m);
          for (octave_idx_type j = 0; j < m; j++)
            for (octave_idx_type i = j + 1; i < m; i++)
              {
                q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          r = afact;
        }


      if (m > 0)
        {
          octave_idx_type k = q.columns ();
          // workspace query.
          float rlwork;
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     &rlwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = rlwork;
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (float, work, lwork);
          F77_XFCN (sorgqr, SORGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     work, lwork, info));
        }
    }
}

template <>
void
qr<FloatMatrix>::init (const FloatMatrix& a, type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (float, tau, min_mn);

  octave_idx_type info = 0;

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
      octave_idx_type lwork = rlwork;
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (float, work, lwork);
      F77_XFCN (sgeqrf, SGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#ifdef HAVE_QRUPDATE

template <>
void
qr<FloatMatrix>::update (const FloatColumnVector& u, const FloatColumnVector& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  F77_XFCN (sqr1up, SQR1UP, (m, n, k, q.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec (), w));
}

template <>
void
qr<FloatMatrix>::update (const FloatMatrix& u, const FloatMatrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (sqr1up, SQR1UP, (m, n, k, q.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec (),
                                 w));
    }
}

template <>
void
qr<FloatMatrix>::insert_col (const FloatColumnVector& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      q.resize (m, k+1);
      r.resize (k+1, n+1);
    }
  else
    {
      r.resize (k, n+1);
    }

  FloatColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrinc, SQRINC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             utmp.data (), w));
}

template <>
void
qr<FloatMatrix>::insert_col (const FloatMatrix& u, const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  if (u.numel () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      octave_idx_type kmax = std::min (k + nj, m);
      if (k < m)
        {
          q.resize (m, kmax);
          r.resize (kmax, n + nj);
        }
      else
        {
          r.resize (k, n + nj);
        }

      OCTAVE_LOCAL_BUFFER (float, w, kmax);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          FloatColumnVector utmp = u.column (jsi(i));
          F77_XFCN (sqrinc, SQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (), js(ii) + 1,
                                     utmp.data (), w));
        }
    }
}

template <>
void
qr<FloatMatrix>::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrdec, SQRDEC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1, w));

  if (k < m)
    {
      q.resize (m, k-1);
      r.resize (k-1, n-1);
    }
  else
    {
      r.resize (k, n-1);
    }
}

template <>
void
qr<FloatMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

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
      OCTAVE_LOCAL_BUFFER (float, w, k);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (sqrdec, SQRDEC, (m, n - ii, k == m ? k : k - ii,
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (),
                                     js(ii) + 1, w));
        }
      if (k < m)
        {
          q.resize (m, k - nj);
          r.resize (k - nj, n - nj);
        }
      else
        {
          r.resize (k, n - nj);
        }

    }
}

template <>
void
qr<FloatMatrix>::insert_row (const FloatRowVector& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = std::min (m, n);

  if (! q.is_square () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  q.resize (m + 1, m + 1);
  r.resize (m + 1, n);
  FloatRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, w, k);
  F77_XFCN (sqrinr, SQRINR, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             j + 1, utmp.fortran_vec (), w));

}

template <>
void
qr<FloatMatrix>::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (float, w, 2*m);
  F77_XFCN (sqrder, SQRDER, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             w));

  q.resize (m - 1, m - 1);
  r.resize (m - 1, n);
}

template <>
void
qr<FloatMatrix>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  OCTAVE_LOCAL_BUFFER (float, w, 2*k);
  F77_XFCN (sqrshc, SQRSHC, (m, n, k,
                             q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             i + 1, j + 1, w));
}

#endif

template <>
void
qr<ComplexMatrix>::form (octave_idx_type n, ComplexMatrix& afact,
                 Complex *tau, type qr_type)
{
  octave_idx_type m = afact.rows ();
  octave_idx_type min_mn = std::min (m, n);
  octave_idx_type info;

  if (qr_type == qr<ComplexMatrix>::raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
        {
          octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
          for (octave_idx_type i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become q.
          q = afact;
          octave_idx_type k = qr_type == qr<ComplexMatrix>::economy ? n : m;
          r = ComplexMatrix (k, n);
          for (octave_idx_type j = 0; j < n; j++)
            {
              octave_idx_type i = 0;
              for (; i <= j; i++)
                r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                r.xelem (i, j) = 0;
            }
          afact = ComplexMatrix (); // optimize memory
        }
      else
        {
          // afact will become r.
          q = ComplexMatrix (m, m);
          for (octave_idx_type j = 0; j < m; j++)
            for (octave_idx_type i = j + 1; i < m; i++)
              {
                q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          r = afact;
        }


      if (m > 0)
        {
          octave_idx_type k = q.columns ();
          // workspace query.
          Complex clwork;
          F77_XFCN (zungqr, ZUNGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     &clwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = clwork.real ();
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (Complex, work, lwork);
          F77_XFCN (zungqr, ZUNGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     work, lwork, info));
        }
    }
}

template <>
void
qr<ComplexMatrix>::init (const ComplexMatrix& a, type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (Complex, tau, min_mn);

  octave_idx_type info = 0;

  ComplexMatrix afact = a;
  if (m > n && qr_type == qr<ComplexMatrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      Complex clwork;
      F77_XFCN (zgeqrf, ZGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 &clwork, -1, info));

      // allocate buffer and do the job.
      octave_idx_type lwork = clwork.real ();
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (Complex, work, lwork);
      F77_XFCN (zgeqrf, ZGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#if defined (HAVE_QRUPDATE)

template <>
void
qr<ComplexMatrix>::update (const ComplexColumnVector& u, const ComplexColumnVector& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqr1up, ZQR1UP, (m, n, k, q.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec (),
                             w, rw));
}

template <>
void
qr<ComplexMatrix>::update (const ComplexMatrix& u, const ComplexMatrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zqr1up, ZQR1UP, (m, n, k, q.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec (),
                                 w, rw));
    }
}

template <>
void
qr<ComplexMatrix>::insert_col (const ComplexColumnVector& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      q.resize (m, k+1);
      r.resize (k+1, n+1);
    }
  else
    {
      r.resize (k, n+1);
    }

  ComplexColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrinc, ZQRINC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             utmp.data (), rw));
}

template <>
void
qr<ComplexMatrix>::insert_col (const ComplexMatrix& u, const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  if (u.numel () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      octave_idx_type kmax = std::min (k + nj, m);
      if (k < m)
        {
          q.resize (m, kmax);
          r.resize (kmax, n + nj);
        }
      else
        {
          r.resize (k, n + nj);
        }

      OCTAVE_LOCAL_BUFFER (double, rw, kmax);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          ComplexColumnVector utmp = u.column (jsi(i));
          F77_XFCN (zqrinc, ZQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (), js(ii) + 1,
                                     utmp.data (), rw));
        }
    }
}

template <>
void
qr<ComplexMatrix>::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrdec, ZQRDEC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1, rw));

  if (k < m)
    {
      q.resize (m, k-1);
      r.resize (k-1, n-1);
    }
  else
    {
      r.resize (k, n-1);
    }
}

template <>
void
qr<ComplexMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

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
      OCTAVE_LOCAL_BUFFER (double, rw, k);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (zqrdec, ZQRDEC, (m, n - ii, k == m ? k : k - ii,
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (),
                                     js(ii) + 1, rw));
        }
      if (k < m)
        {
          q.resize (m, k - nj);
          r.resize (k - nj, n - nj);
        }
      else
        {
          r.resize (k, n - nj);
        }

    }
}

template <>
void
qr<ComplexMatrix>::insert_row (const ComplexRowVector& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = std::min (m, n);

  if (! q.is_square () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  q.resize (m + 1, m + 1);
  r.resize (m + 1, n);
  ComplexRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrinr, ZQRINR, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             j + 1, utmp.fortran_vec (), rw));

}

template <>
void
qr<ComplexMatrix>::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  OCTAVE_LOCAL_BUFFER (double, rw, m);
  F77_XFCN (zqrder, ZQRDER, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             w, rw));

  q.resize (m - 1, m - 1);
  r.resize (m - 1, n);
}

template <>
void
qr<ComplexMatrix>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  OCTAVE_LOCAL_BUFFER (Complex, w, k);
  OCTAVE_LOCAL_BUFFER (double, rw, k);
  F77_XFCN (zqrshc, ZQRSHC, (m, n, k,
                             q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             i + 1, j + 1, w, rw));
}

#endif

template <>
void
qr<FloatComplexMatrix>::form (octave_idx_type n, FloatComplexMatrix& afact, FloatComplex *tau, type qr_type)
{
  octave_idx_type m = afact.rows ();
  octave_idx_type min_mn = std::min (m, n);
  octave_idx_type info;

  if (qr_type == qr<FloatComplexMatrix>::raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
        {
          octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
          for (octave_idx_type i = limit + 1; i < m; i++)
            afact.elem (i, j) *= tau[j];
        }

      r = afact;
    }
  else
    {
      // Attempt to minimize copying.
      if (m >= n)
        {
          // afact will become q.
          q = afact;
          octave_idx_type k = qr_type == qr<FloatComplexMatrix>::economy ? n : m;
          r = FloatComplexMatrix (k, n);
          for (octave_idx_type j = 0; j < n; j++)
            {
              octave_idx_type i = 0;
              for (; i <= j; i++)
                r.xelem (i, j) = afact.xelem (i, j);
              for (; i < k; i++)
                r.xelem (i, j) = 0;
            }
          afact = FloatComplexMatrix (); // optimize memory
        }
      else
        {
          // afact will become r.
          q = FloatComplexMatrix (m, m);
          for (octave_idx_type j = 0; j < m; j++)
            for (octave_idx_type i = j + 1; i < m; i++)
              {
                q.xelem (i, j) = afact.xelem (i, j);
                afact.xelem (i, j) = 0;
              }
          r = afact;
        }


      if (m > 0)
        {
          octave_idx_type k = q.columns ();
          // workspace query.
          FloatComplex clwork;
          F77_XFCN (cungqr, CUNGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     &clwork, -1, info));

          // allocate buffer and do the job.
          octave_idx_type lwork = clwork.real ();
          lwork = std::max (lwork, static_cast<octave_idx_type> (1));
          OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);
          F77_XFCN (cungqr, CUNGQR, (m, k, min_mn, q.fortran_vec (), m, tau,
                                     work, lwork, info));
        }
    }
}

template <>
void
qr<FloatComplexMatrix>::init (const FloatComplexMatrix& a, type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  octave_idx_type min_mn = m < n ? m : n;
  OCTAVE_LOCAL_BUFFER (FloatComplex, tau, min_mn);

  octave_idx_type info = 0;

  FloatComplexMatrix afact = a;
  if (m > n && qr_type == qr<FloatComplexMatrix>::std)
    afact.resize (m, m);

  if (m > 0)
    {
      // workspace query.
      FloatComplex clwork;
      F77_XFCN (cgeqrf, CGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 &clwork, -1, info));

      // allocate buffer and do the job.
      octave_idx_type lwork = clwork.real ();
      lwork = std::max (lwork, static_cast<octave_idx_type> (1));
      OCTAVE_LOCAL_BUFFER (FloatComplex, work, lwork);
      F77_XFCN (cgeqrf, CGEQRF, (m, n, afact.fortran_vec (), m, tau,
                                 work, lwork, info));
    }

  form (n, afact, tau, qr_type);
}

#ifdef HAVE_QRUPDATE

template <>
void
qr<FloatComplexMatrix>::update (const FloatComplexColumnVector& u, const FloatComplexColumnVector& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  FloatComplexColumnVector utmp = u;
  FloatComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqr1up, CQR1UP, (m, n, k, q.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec (),
                             w, rw));
}

template <>
void
qr<FloatComplexMatrix>::update (const FloatComplexMatrix& u, const FloatComplexMatrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("qrupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (cqr1up, CQR1UP, (m, n, k, q.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec (),
                                 w, rw));
    }
}

template <>
void
qr<FloatComplexMatrix>::insert_col (const FloatComplexColumnVector& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.numel () != m)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (k < m)
    {
      q.resize (m, k+1);
      r.resize (k+1, n+1);
    }
  else
    {
      r.resize (k, n+1);
    }

  FloatComplexColumnVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrinc, CQRINC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             utmp.data (), rw));
}

template <>
void
qr<FloatComplexMatrix>::insert_col (const FloatComplexMatrix& u, const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  Array<octave_idx_type> jsi;
  Array<octave_idx_type> js = j.sort (jsi, 0, ASCENDING);
  octave_idx_type nj = js.numel ();
  bool dups = false;
  for (octave_idx_type i = 0; i < nj - 1; i++)
    dups = dups && js(i) == js(i+1);

  if (dups)
    (*current_liboctave_error_handler) ("qrinsert: duplicate index detected");
  if (u.numel () != m || u.columns () != nj)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (nj > 0 && (js(0) < 0 || js(nj-1) > n))
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  if (nj > 0)
    {
      octave_idx_type kmax = std::min (k + nj, m);
      if (k < m)
        {
          q.resize (m, kmax);
          r.resize (kmax, n + nj);
        }
      else
        {
          r.resize (k, n + nj);
        }

      OCTAVE_LOCAL_BUFFER (float, rw, kmax);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (cqrinc, CQRINC, (m, n + ii, std::min (kmax, k + ii),
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (), js(ii) + 1,
                                     u.column (jsi(i)).data (), rw));
        }
    }
}

template <>
void
qr<FloatComplexMatrix>::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrdec, CQRDEC, (m, n, k, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1, rw));

  if (k < m)
    {
      q.resize (m, k-1);
      r.resize (k-1, n-1);
    }
  else
    {
      r.resize (k, n-1);
    }
}

template <>
void
qr<FloatComplexMatrix>::delete_col (const Array<octave_idx_type>& j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

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
      OCTAVE_LOCAL_BUFFER (float, rw, k);
      for (volatile octave_idx_type i = 0; i < js.numel (); i++)
        {
          octave_idx_type ii = i;
          F77_XFCN (cqrdec, CQRDEC, (m, n - ii, k == m ? k : k - ii,
                                     q.fortran_vec (), q.rows (),
                                     r.fortran_vec (), r.rows (),
                                     js(ii) + 1, rw));
        }
      if (k < m)
        {
          q.resize (m, k - nj);
          r.resize (k - nj, n - nj);
        }
      else
        {
          r.resize (k, n - nj);
        }

    }
}

template <>
void
qr<FloatComplexMatrix>::insert_row (const FloatComplexRowVector& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = std::min (m, n);

  if (! q.is_square () || u.numel () != n)
    (*current_liboctave_error_handler) ("qrinsert: dimensions mismatch");
  if (j < 0 || j > m)
    (*current_liboctave_error_handler) ("qrinsert: index out of range");

  q.resize (m + 1, m + 1);
  r.resize (m + 1, n);
  FloatComplexRowVector utmp = u;
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrinr, CQRINR, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             j + 1, utmp.fortran_vec (), rw));

}

template <>
void
qr<FloatComplexMatrix>::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("qrdelete: dimensions mismatch");
  if (j < 0 || j > m-1)
    (*current_liboctave_error_handler) ("qrdelete: index out of range");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  OCTAVE_LOCAL_BUFFER (float, rw, m);
  F77_XFCN (cqrder, CQRDER, (m, n, q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (), j + 1,
                             w, rw));

  q.resize (m - 1, m - 1);
  r.resize (m - 1, n);
}

template <>
void
qr<FloatComplexMatrix>::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("qrshift: index out of range");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, k);
  OCTAVE_LOCAL_BUFFER (float, rw, k);
  F77_XFCN (cqrshc, CQRSHC, (m, n, k,
                             q.fortran_vec (), q.rows (),
                             r.fortran_vec (), r.rows (),
                             i + 1, j + 1, w, rw));
}

#endif

// Instantiations we need.

template class qr<Matrix>;

template class qr<FloatMatrix>;

template class qr<ComplexMatrix>;

template class qr<FloatComplexMatrix>;
