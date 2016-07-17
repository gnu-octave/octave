/*

Copyright (C) 1996-2015 John W. Eaton
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "CColVector.h"
#include "CMatrix.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lu.h"
#include "oct-locbuf.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE*, const F77_INT&,
                             F77_INT*, F77_INT&);

#if defined (HAVE_QRUPDATE_LUU)
  F77_RET_T
  F77_FUNC (dlu1up, DLU1UP) (const F77_INT&, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, const F77_INT&,
                             F77_DBLE *, F77_DBLE *);

  F77_RET_T
  F77_FUNC (dlup1up, DLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_DBLE *, const F77_INT&,
                               F77_DBLE *, const F77_INT&,
                               F77_INT *, const F77_DBLE *,
                               const F77_DBLE *, F77_DBLE *);
#endif

  F77_RET_T
  F77_FUNC (sgetrf, SGETRF) (const F77_INT&, const F77_INT&,
                             F77_REAL*, const F77_INT&, F77_INT*,
                             F77_INT&);

#if defined (HAVE_QRUPDATE_LUU)
  F77_RET_T
  F77_FUNC (slu1up, SLU1UP) (const F77_INT&, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, const F77_INT&,
                             F77_REAL *, F77_REAL *);

  F77_RET_T
  F77_FUNC (slup1up, SLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_REAL *, const F77_INT&,
                               F77_REAL *, const F77_INT&,
                               F77_INT *, const F77_REAL *,
                               const F77_REAL *, F77_REAL *);
#endif

  F77_RET_T
  F77_FUNC (zgetrf, ZGETRF) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX*, const F77_INT&,
                             F77_INT*, F77_INT&);

#if defined (HAVE_QRUPDATE_LUU)
  F77_RET_T
  F77_FUNC (zlu1up, ZLU1UP) (const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, const F77_INT&,
                             F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);

  F77_RET_T
  F77_FUNC (zlup1up, ZLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_DBLE_CMPLX *, const F77_INT&,
                               F77_DBLE_CMPLX *, const F77_INT&,
                               F77_INT *, const F77_DBLE_CMPLX *,
                               const F77_DBLE_CMPLX *, F77_DBLE_CMPLX *);
#endif

  F77_RET_T
  F77_FUNC (cgetrf, CGETRF) (const F77_INT&, const F77_INT&,
                             F77_CMPLX*, const F77_INT&,
                             F77_INT*, F77_INT&);

#if defined (HAVE_QRUPDATE_LUU)
  F77_RET_T
  F77_FUNC (clu1up, CLU1UP) (const F77_INT&, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, const F77_INT&,
                             F77_CMPLX *, F77_CMPLX *);

  F77_RET_T
  F77_FUNC (clup1up, CLUP1UP) (const F77_INT&, const F77_INT&,
                               F77_CMPLX *, const F77_INT&,
                               F77_CMPLX *, const F77_INT&,
                               F77_INT *, const F77_CMPLX *,
                               const F77_CMPLX *, F77_CMPLX *);
#endif
}

template <typename T>
lu<T>::lu (const T& l, const T& u,
                           const PermMatrix& p)
  : a_fact (u), l_fact (l), ipvt (p.transpose ().col_perm_vec ())
{
  if (l.columns () != u.rows ())
    (*current_liboctave_error_handler) ("lu: dimension mismatch");
}

template <typename T>
bool
lu<T>::packed (void) const
{
  return l_fact.dims () == dim_vector ();
}

template <typename T>
void
lu<T>::unpack (void)
{
  if (packed ())
    {
      l_fact = L ();
      a_fact = U (); // FIXME: sub-optimal
      ipvt = getp ();
    }
}

template <typename T>
T
lu<T>::L (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();
      octave_idx_type a_nc = a_fact.cols ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      T l (a_nr, mn, ELT_T (0.0));

      for (octave_idx_type i = 0; i < a_nr; i++)
        {
          if (i < a_nc)
            l.xelem (i, i) = 1.0;

          for (octave_idx_type j = 0; j < (i < a_nc ? i : a_nc); j++)
            l.xelem (i, j) = a_fact.xelem (i, j);
        }

      return l;
    }
  else
    return l_fact;
}

template <typename T>
T
lu<T>::U (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();
      octave_idx_type a_nc = a_fact.cols ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      T u (mn, a_nc, ELT_T (0.0));

      for (octave_idx_type i = 0; i < mn; i++)
        {
          for (octave_idx_type j = i; j < a_nc; j++)
            u.xelem (i, j) = a_fact.xelem (i, j);
        }

      return u;
    }
  else
    return a_fact;
}

template <typename T>
T
lu<T>::Y (void) const
{
  if (! packed ())
    (*current_liboctave_error_handler)
      ("lu: Y () not implemented for unpacked form");

  return a_fact;
}

template <typename T>
Array<octave_idx_type>
lu<T>::getp (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();

      Array<octave_idx_type> pvt (dim_vector (a_nr, 1));

      for (octave_idx_type i = 0; i < a_nr; i++)
        pvt.xelem (i) = i;

      for (octave_idx_type i = 0; i < ipvt.numel (); i++)
        {
          octave_idx_type k = ipvt.xelem (i);

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
    return ipvt;
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
  octave_idx_type a_nr = a_fact.rows ();

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

  octave_idx_type k = std::min (a_fact.rows (), a_fact.columns ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (a_fact(i, i) == ELT_T ())
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
lu<Matrix>::lu (const Matrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  double *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (dgetrf, DGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
void
lu<Matrix>::update (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec ()));
}

template <>
void
lu<Matrix>::update (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dlu1up, DLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
void
lu<Matrix>::update_piv (const ColumnVector& u, const ColumnVector& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ColumnVector utmp = u;
  ColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (double, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

template <>
void
lu<Matrix>::update_piv (const Matrix& u, const Matrix& v)
{
  if (packed ())
    unpack ();

  Matrix& l = l_fact;
  Matrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (double, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ColumnVector utmp = u.column (i);
      ColumnVector vtmp = v.column (i);
      F77_XFCN (dlup1up, DLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

#endif

template <>
lu<FloatMatrix>::lu (const FloatMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  float *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (sgetrf, SGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
void
lu<FloatMatrix>::update (const FloatColumnVector& u, const FloatColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = l_fact;
  FloatMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  F77_XFCN (slu1up, SLU1UP, (m, n, l.fortran_vec (),
                             m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec ()));
}

template <>
void
lu<FloatMatrix>::update (const FloatMatrix& u, const FloatMatrix& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = l_fact;
  FloatMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (slu1up, SLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
void
lu<FloatMatrix>::update_piv (const FloatColumnVector& u,
                          const FloatColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = l_fact;
  FloatMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatColumnVector utmp = u;
  FloatColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (float, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  F77_XFCN (slup1up, SLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

template <>
void
lu<FloatMatrix>::update_piv (const FloatMatrix& u, const FloatMatrix& v)
{
  if (packed ())
    unpack ();

  FloatMatrix& l = l_fact;
  FloatMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (float, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatColumnVector utmp = u.column (i);
      FloatColumnVector vtmp = v.column (i);
      F77_XFCN (slup1up, SLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

#endif

template <>
lu<ComplexMatrix>::lu (const ComplexMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  Complex *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (zgetrf, ZGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
void
lu<ComplexMatrix>::update (const ComplexColumnVector& u,
                           const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  F77_XFCN (zlu1up, ZLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (), k,
                             utmp.fortran_vec (), vtmp.fortran_vec ()));
}

template <>
void
lu<ComplexMatrix>::update (const ComplexMatrix& u, const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zlu1up, ZLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
void
lu<ComplexMatrix>::update_piv (const ComplexColumnVector& u,
                               const ComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  ComplexColumnVector utmp = u;
  ComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  F77_XFCN (zlup1up, ZLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

template <>
void
lu<ComplexMatrix>::update_piv (const ComplexMatrix& u, const ComplexMatrix& v)
{
  if (packed ())
    unpack ();

  ComplexMatrix& l = l_fact;
  ComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (Complex, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      ComplexColumnVector utmp = u.column (i);
      ComplexColumnVector vtmp = v.column (i);
      F77_XFCN (zlup1up, ZLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

#endif

template <>
lu<FloatComplexMatrix>::lu (const FloatComplexMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (dim_vector (mn, 1));
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  FloatComplex *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (cgetrf, CGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  for (octave_idx_type i = 0; i < mn; i++)
    pipvt[i] -= 1;
}

#if defined (HAVE_QRUPDATE_LUU)

template <>
void
lu<FloatComplexMatrix>::update (const FloatComplexColumnVector& u,
                             const FloatComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = l_fact;
  FloatComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () == m && v.numel () == n)
    {
      FloatComplexColumnVector utmp = u;
      FloatComplexColumnVector vtmp = v;
      F77_XFCN (clu1up, CLU1UP, (m, n, l.fortran_vec (), m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
  else
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");
}

template <>
void
lu<FloatComplexMatrix>::update (const FloatComplexMatrix& u,
                             const FloatComplexMatrix& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = l_fact;
  FloatComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (clu1up, CLU1UP, (m, n, l.fortran_vec (),
                                 m, r.fortran_vec (), k,
                                 utmp.fortran_vec (), vtmp.fortran_vec ()));
    }
}

template <>
void
lu<FloatComplexMatrix>::update_piv (const FloatComplexColumnVector& u,
                                 const FloatComplexColumnVector& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = l_fact;
  FloatComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.numel () != m || v.numel () != n)
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  FloatComplexColumnVector utmp = u;
  FloatComplexColumnVector vtmp = v;
  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  F77_XFCN (clup1up, CLUP1UP, (m, n, l.fortran_vec (),
                               m, r.fortran_vec (), k,
                               ipvt.fortran_vec (),
                               utmp.data (), vtmp.data (), w));
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

template <>
void
lu<FloatComplexMatrix>::update_piv (const FloatComplexMatrix& u,
                                 const FloatComplexMatrix& v)
{
  if (packed ())
    unpack ();

  FloatComplexMatrix& l = l_fact;
  FloatComplexMatrix& r = a_fact;

  octave_idx_type m = l.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = l.columns ();

  if (u.rows () != m || v.rows () != n || u.cols () != v.cols ())
    (*current_liboctave_error_handler) ("luupdate: dimensions mismatch");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, m);
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) += 1; // increment
  for (volatile octave_idx_type i = 0; i < u.cols (); i++)
    {
      FloatComplexColumnVector utmp = u.column (i);
      FloatComplexColumnVector vtmp = v.column (i);
      F77_XFCN (clup1up, CLUP1UP, (m, n, l.fortran_vec (),
                                   m, r.fortran_vec (), k,
                                   ipvt.fortran_vec (),
                                   utmp.data (), vtmp.data (), w));
    }
  for (octave_idx_type i = 0; i < m; i++) ipvt(i) -= 1; // decrement
}

#endif

// Instantiations we need.

template class lu<Matrix>;

template class lu<FloatMatrix>;

template class lu<ComplexMatrix>;

template class lu<FloatComplexMatrix>;
