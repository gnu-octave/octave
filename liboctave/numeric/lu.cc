////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include "CColVector.h"
#include "CMatrix.h"
#include "PermMatrix.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "lo-qrupdate-proto.h"
#include "lu.h"
#include "oct-locbuf.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// FIXME: PermMatrix::col_perm_vec returns Array<octave_idx_type>
// but m_ipvt is an Array<octave_f77_int_type>.  This could cause
// trouble for large arrays if octave_f77_int_type is 32-bits but
// octave_idx_type is 64.  Since this constructor is called from
// Fluupdate, it could be given values that are out of range.  We
// should ensure that the values are within range here.

template <typename T>
lu<T>::lu (const T& l, const T& u, const PermMatrix& p)
  : m_a_fact (u), m_L (l), m_ipvt (p.transpose ().col_perm_vec ())
{
  if (l.columns () != u.rows ())
    (*current_liboctave_error_handler) ("lu: dimension mismatch");
}

template <typename T>
bool
lu<T>::packed (void) const
{
  return m_L.dims () == dim_vector ();
}

template <typename T>
void
lu<T>::unpack (void)
{
  if (packed ())
    {
      m_L = L ();
      m_a_fact = U (); // FIXME: sub-optimal

      // FIXME: getp returns Array<octave_idx_type> but m_ipvt is
      // Array<octave_f77_int_type>.  However, getp produces its
      // result from a valid m_ipvt array so validation should not be
      // necessary.  OTOH, it might be better to have a version of
      // getp that doesn't cause us to convert from
      // Array<octave_f77_int_type> to Array<octave_idx_type> and
      // back again.

      m_ipvt = getp ();
    }
}

template <typename T>
T
lu<T>::L (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = m_a_fact.rows ();
      octave_idx_type a_nc = m_a_fact.columns ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      T l (a_nr, mn, ELT_T (0.0));

      for (octave_idx_type i = 0; i < a_nr; i++)
        {
          if (i < a_nc)
            l.xelem (i, i) = 1.0;

          for (octave_idx_type j = 0; j < (i < a_nc ? i : a_nc); j++)
            l.xelem (i, j) = m_a_fact.xelem (i, j);
        }

      return l;
    }
  else
    return m_L;
}

template <typename T>
T
lu<T>::U (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = m_a_fact.rows ();
      octave_idx_type a_nc = m_a_fact.columns ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      T u (mn, a_nc, ELT_T (0.0));

      for (octave_idx_type i = 0; i < mn; i++)
        {
          for (octave_idx_type j = i; j < a_nc; j++)
            u.xelem (i, j) = m_a_fact.xelem (i, j);
        }

      return u;
    }
  else
    return m_a_fact;
}

template <typename T>
T
lu<T>::Y (void) const
{
  if (! packed ())
    (*current_liboctave_error_handler)
      ("lu: Y () not implemented for unpacked form");

  return m_a_fact;
}

template <typename T>
Array<octave_idx_type>
lu<T>::getp (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = m_a_fact.rows ();

      Array<octave_idx_type> pvt (dim_vector (a_nr, 1));

      for (octave_idx_type i = 0; i < a_nr; i++)
        pvt.xelem (i) = i;

      for (octave_idx_type i = 0; i < m_ipvt.numel (); i++)
        {
          octave_idx_type k = m_ipvt.xelem (i);

          if (k != i)
            {
              octave_idx_type tmp = pvt.xelem (k);
              pvt.xelem (k) = pvt.xelem (i);
              pvt.xelem (i) = tmp;
            }
        }

      return pvt;
    }
  else
    return m_ipvt;
}

template <typename T>
PermMatrix
lu<T>::P (void) const
{
  return PermMatrix (getp (), false);
}

template <typename T>
ColumnVector
lu<T>::P_vec (void) const
{
  octave_idx_type a_nr = m_a_fact.rows ();

  ColumnVector p (a_nr);

  Array<octave_idx_type> pvt = getp ();

  for (octave_idx_type i = 0; i < a_nr; i++)
    p.xelem (i) = static_cast<double> (pvt.xelem (i) + 1);

  return p;
}

template <typename T>
bool
lu<T>::regular (void) const
{
  bool retval = true;

  octave_idx_type k = std::min (m_a_fact.rows (), m_a_fact.columns ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (m_a_fact(i, i) == ELT_T ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

#if ! defined (HAVE_QRUPDATE_LUU)

template <typename T>
void
lu<T>::update (const VT&, const VT&)
{
  (*current_liboctave_error_handler)
    ("luupdate: support for qrupdate with LU updates "
     "was unavailable or disabled when liboctave was built");
}

template <typename T>
void
lu<T>::update (const T&, const T&)
{
  (*current_liboctave_error_handler)
    ("luupdate: support for qrupdate with LU updates "
     "was unavailable or disabled when liboctave was built");
}

template <typename T>
void
lu<T>::update_piv (const VT&, const VT&)
{
  (*current_liboctave_error_handler)
    ("luupdate: support for qrupdate with LU updates "
     "was unavailable or disabled when liboctave was built");
}

template <typename T>
void
lu<T>::update_piv (const T&, const T&)
{
  (*current_liboctave_error_handler)
    ("luupdate: support for qrupdate with LU updates "
     "was unavailable or disabled when liboctave was built");
}

#endif

// Specializations.

template <>
OCTAVE_API
lu<Matrix>::lu (const Matrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.columns ());
  F77_INT mn = (a_nr < a_nc ? a_nr : a_nc);

  m_ipvt.resize (dim_vector (mn, 1));
  F77_INT *pipvt = m_ipvt.fortran_vec ();

  m_a_fact = a;
  double *tmp_data = m_a_fact.fortran_vec ();

  F77_INT info = 0;

  F77_XFCN (dgetrf, DGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (F77_INT i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
OCTAVE_API void
lu<Matrix>::update (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = m_L;
  Matrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (),
                             k, utmp.fortran_vec (), vtmp.fortran_vec ()));
}

template <>
OCTAVE_API void
lu<Matrix>::update (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = m_L;
  Matrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
OCTAVE_API void
lu<Matrix>::update_piv (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = m_L;
  Matrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (double, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               m_ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

template <>
OCTAVE_API void
lu<Matrix>::update_piv (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = m_L;
  Matrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (double, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   m_ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

#endif

template <>
OCTAVE_API
lu<FloatMatrix>::lu (const FloatMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.columns ());
  F77_INT mn = (a_nr < a_nc ? a_nr : a_nc);

  m_ipvt.resize (dim_vector (mn, 1));
  F77_INT *pipvt = m_ipvt.fortran_vec ();

  m_a_fact = a;
  float *tmp_data = m_a_fact.fortran_vec ();

  F77_INT info = 0;

  F77_XFCN (sgetrf, SGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (F77_INT i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
OCTAVE_API void
lu<FloatMatrix>::update (const FloatColumnVector& u,
                         const FloatColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = m_L;
  FloatMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  F77_XFCN (slu1up, SLU1UP, (m, n, l.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec ()));
}

template <>
OCTAVE_API void
lu<FloatMatrix>::update (const FloatMatrix& u, const FloatMatrix& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = m_L;
  FloatMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (slu1up, SLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
OCTAVE_API void
lu<FloatMatrix>::update_piv (const FloatColumnVector& u,
                             const FloatColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = m_L;
  FloatMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (float, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  F77_XFCN (slup1up, SLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               m_ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

template <>
OCTAVE_API void
lu<FloatMatrix>::update_piv (const FloatMatrix& u, const FloatMatrix& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = m_L;
  FloatMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (float, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (slup1up, SLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   m_ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

#endif

template <>
OCTAVE_API
lu<ComplexMatrix>::lu (const ComplexMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.columns ());
  F77_INT mn = (a_nr < a_nc ? a_nr : a_nc);

  m_ipvt.resize (dim_vector (mn, 1));
  F77_INT *pipvt = m_ipvt.fortran_vec ();

  m_a_fact = a;
  Complex *tmp_data = m_a_fact.fortran_vec ();

  F77_INT info = 0;

  F77_XFCN (zgetrf, ZGETRF, (a_nr, a_nc, F77_DBLE_CMPLX_ARG (tmp_data),
                             a_nr, pipvt, info));

  for (F77_INT i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
OCTAVE_API void
lu<ComplexMatrix>::update (const ComplexColumnVector& u,
                           const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = m_L;
  ComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  F77_XFCN (zlu1up, ZLU1UP, (m, n, F77_DBLE_CMPLX_ARG (l.fortran_vec ()), m,
                             F77_DBLE_CMPLX_ARG (r.fortran_vec ()), k,
                             F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()),
                             F77_DBLE_CMPLX_ARG (vtmp.fortran_vec ())));
}

template <>
OCTAVE_API void
lu<ComplexMatrix>::update (const ComplexMatrix& u, const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = m_L;
  ComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zlu1up, ZLU1UP, (m, n,
                                 F77_DBLE_CMPLX_ARG (l.fortran_vec ()),
                                 m, F77_DBLE_CMPLX_ARG (r.fortran_vec ()),
                                 k,
                                 F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()),
                                 F77_DBLE_CMPLX_ARG (vtmp.fortran_vec ())));
    }
}

template <>
OCTAVE_API void
lu<ComplexMatrix>::update_piv (const ComplexColumnVector& u,
                               const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = m_L;
  ComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  F77_XFCN (zlup1up, ZLUP1UP, (m, n, F77_DBLE_CMPLX_ARG (l.fortran_vec ()),
                               m, F77_DBLE_CMPLX_ARG (r.fortran_vec ()), k,
                               m_ipvt.fortran_vec (),
                               F77_CONST_DBLE_CMPLX_ARG (utmp.data ()),
                               F77_CONST_DBLE_CMPLX_ARG (vtmp.data ()),
                               F77_DBLE_CMPLX_ARG (w)));
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

template <>
OCTAVE_API void
lu<ComplexMatrix>::update_piv (const ComplexMatrix& u,
                               const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = m_L;
  ComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zlup1up, ZLUP1UP, (m, n,
                                   F77_DBLE_CMPLX_ARG (l.fortran_vec ()),
                                   m,
                                   F77_DBLE_CMPLX_ARG (r.fortran_vec ()),
                                   k, m_ipvt.fortran_vec (),
                                   F77_CONST_DBLE_CMPLX_ARG (utmp.data ()),
                                   F77_CONST_DBLE_CMPLX_ARG (vtmp.data ()),
                                   F77_DBLE_CMPLX_ARG (w)));
    }
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

#endif

template <>
OCTAVE_API
lu<FloatComplexMatrix>::lu (const FloatComplexMatrix& a)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.columns ());
  F77_INT mn = (a_nr < a_nc ? a_nr : a_nc);

  m_ipvt.resize (dim_vector (mn, 1));
  F77_INT *pipvt = m_ipvt.fortran_vec ();

  m_a_fact = a;
  FloatComplex *tmp_data = m_a_fact.fortran_vec ();

  F77_INT info = 0;

  F77_XFCN (cgetrf, CGETRF, (a_nr, a_nc, F77_CMPLX_ARG (tmp_data), a_nr,
                             pipvt, info));

  for (F77_INT i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
OCTAVE_API void
lu<FloatComplexMatrix>::update (const FloatComplexColumnVector& u,
                                const FloatComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = m_L;
  FloatComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatComplexColumnVector utmp = u;
  FloatComplexColumnVector vtmp = v;
  F77_XFCN (clu1up, CLU1UP, (m, n, F77_CMPLX_ARG (l.fortran_vec ()), m,
                             F77_CMPLX_ARG (r.fortran_vec ()), k,
                             F77_CMPLX_ARG (utmp.fortran_vec ()),
                             F77_CMPLX_ARG (vtmp.fortran_vec ())));
}

template <>
OCTAVE_API void
lu<FloatComplexMatrix>::update (const FloatComplexMatrix& u,
                                const FloatComplexMatrix& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = m_L;
  FloatComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (clu1up, CLU1UP, (m, n, F77_CMPLX_ARG (l.fortran_vec ()),
                                 m, F77_CMPLX_ARG (r.fortran_vec ()), k,
                                 F77_CMPLX_ARG (utmp.fortran_vec ()),
                                 F77_CMPLX_ARG (vtmp.fortran_vec ())));
    }
}

template <>
OCTAVE_API void
lu<FloatComplexMatrix>::update_piv (const FloatComplexColumnVector& u,
                                    const FloatComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = m_L;
  FloatComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nel = to_f77_int (u.numel ());
  F77_INT v_nel = to_f77_int (v.numel ());

  if (u_nel != m || v_nel != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatComplexColumnVector utmp = u;
  FloatComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  F77_XFCN (clup1up, CLUP1UP, (m, n, F77_CMPLX_ARG (l.fortran_vec ()),
                               m, F77_CMPLX_ARG (r.fortran_vec ()), k,
                               m_ipvt.fortran_vec (),
                               F77_CONST_CMPLX_ARG (utmp.data ()),
                               F77_CONST_CMPLX_ARG (vtmp.data ()),
                               F77_CMPLX_ARG (w)));
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

template <>
OCTAVE_API void
lu<FloatComplexMatrix>::update_piv (const FloatComplexMatrix& u,
                                    const FloatComplexMatrix& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = m_L;
  FloatComplexMatrix& r = m_a_fact;

  F77_INT m = to_f77_int (l.rows ());
  F77_INT n = to_f77_int (r.columns ());
  F77_INT k = to_f77_int (l.columns ());

  F77_INT u_nr = to_f77_int (u.rows ());
  F77_INT u_nc = to_f77_int (u.columns ());

  F77_INT v_nr = to_f77_int (v.rows ());
  F77_INT v_nc = to_f77_int (v.columns ());

  if (u_nr != m || v_nr != n || u_nc != v_nc)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) += 1; // increment
  for (volatile F77_INT i = 0; i < u_nc; i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (clup1up, CLUP1UP, (m, n, F77_CMPLX_ARG (l.fortran_vec ()),
                                   m, F77_CMPLX_ARG (r.fortran_vec ()), k,
                                   m_ipvt.fortran_vec (),
                                   F77_CONST_CMPLX_ARG (utmp.data ()),
                                   F77_CONST_CMPLX_ARG (vtmp.data ()),
                                   F77_CMPLX_ARG (w)));
    }
  for (F77_INT i = 0; i < m; i++) m_ipvt(i) -= 1; // decrement
}

#endif

// Instantiations we need.

template class lu<Matrix>;

template class lu<FloatMatrix>;

template class lu<ComplexMatrix>;

template class lu<FloatComplexMatrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
