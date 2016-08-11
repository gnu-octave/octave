/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#include <vector>

#include "CColVector.h"
#include "CMatrix.h"
#include "CRowVector.h"
#include "chol.h"
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
#include "lo-error.h"
#include "oct-locbuf.h"
#include "oct-norm.h"

#if ! defined (HAVE_QRUPDATE)
#  include "qr.h"
#endif

extern "C"
{
  F77_RET_T
  F77_FUNC (dpotrf, DPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpotri, DPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpocon, DPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE*,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE&, F77_DBLE*, F77_INT*,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (dch1up, DCH1UP) (const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_DBLE*, F77_DBLE*);

  F77_RET_T
  F77_FUNC (dch1dn, DCH1DN) (const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_DBLE*, F77_DBLE*,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (dchinx, DCHINX) (const F77_INT&, F77_DBLE*,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE*, F77_DBLE*, F77_INT&);

  F77_RET_T
  F77_FUNC (dchdex, DCHDEX) (const F77_INT&, F77_DBLE*,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE*);

  F77_RET_T
  F77_FUNC (dchshx, DCHSHX) (const F77_INT&, F77_DBLE*,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE*);
#endif

  F77_RET_T
  F77_FUNC (spotrf, SPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spotri, SPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spocon, SPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL*,
                             const F77_INT&, const F77_REAL&,
                             F77_REAL&, F77_REAL*, F77_INT*,
                             F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (sch1up, SCH1UP) (const F77_INT&, F77_REAL*,
                             const F77_INT&, F77_REAL*, F77_REAL*);

  F77_RET_T
  F77_FUNC (sch1dn, SCH1DN) (const F77_INT&, F77_REAL*,
                             const F77_INT&, F77_REAL*, F77_REAL*,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (schinx, SCHINX) (const F77_INT&, F77_REAL*,
                             const F77_INT&, const F77_INT&,
                             F77_REAL*, F77_REAL*, F77_INT&);

  F77_RET_T
  F77_FUNC (schdex, SCHDEX) (const F77_INT&, F77_REAL*,
                             const F77_INT&, const F77_INT&,
                             F77_REAL*);

  F77_RET_T
  F77_FUNC (schshx, SCHSHX) (const F77_INT&, F77_REAL*,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_REAL*);
#endif

  F77_RET_T
  F77_FUNC (zpotrf, ZPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (zpotri, ZPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpocon, ZPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, const F77_DBLE&,
                             F77_DBLE&, F77_DBLE_CMPLX*, F77_DBLE*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (zch1up, ZCH1UP) (const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_DBLE_CMPLX*, F77_DBLE*);

  F77_RET_T
  F77_FUNC (zch1dn, ZCH1DN) (const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_DBLE_CMPLX*, F77_DBLE*,
                             F77_INT&);

  F77_RET_T
  F77_FUNC (zchinx, ZCHINX) (const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE_CMPLX*, F77_DBLE*, F77_INT&);

  F77_RET_T
  F77_FUNC (zchdex, ZCHDEX) (const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             F77_DBLE*);

  F77_RET_T
  F77_FUNC (zchshx, ZCHSHX) (const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_DBLE_CMPLX*, F77_DBLE*);
#endif

  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (cpotri, CPOTRI) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX*,
                             const F77_INT&, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpocon, CPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX*,
                             const F77_INT&, const F77_REAL&,
                             F77_REAL&, F77_CMPLX*, F77_REAL*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL);
#if defined (HAVE_QRUPDATE)

  F77_RET_T
  F77_FUNC (cch1up, CCH1UP) (const F77_INT&, F77_CMPLX*,
                             const F77_INT&, F77_CMPLX*, F77_REAL*);

  F77_RET_T
  F77_FUNC (cch1dn, CCH1DN) (const F77_INT&, F77_CMPLX*,
                             const F77_INT&, F77_CMPLX*,
                             F77_REAL*, F77_INT&);

  F77_RET_T
  F77_FUNC (cchinx, CCHINX) (const F77_INT&, F77_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             F77_CMPLX*, F77_REAL*, F77_INT&);

  F77_RET_T
  F77_FUNC (cchdex, CCHDEX) (const F77_INT&, F77_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             F77_REAL*);

  F77_RET_T
  F77_FUNC (cchshx, CCHSHX) (const F77_INT&, F77_CMPLX*,
                             const F77_INT&, const F77_INT&,
                             const F77_INT&, F77_CMPLX*, F77_REAL*);
#endif
}

static Matrix
chol2inv_internal (const Matrix& r, bool is_upper = true)
{
  Matrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr != r_nc)
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  octave_idx_type n = r_nc;
  octave_idx_type info = 0;

  Matrix tmp = r;
  double *v = tmp.fortran_vec ();

  if (info == 0)
    {
      if (is_upper)
        F77_XFCN (dpotri, DPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
                                   v, n, info
                                   F77_CHAR_ARG_LEN (1)));
      else
        F77_XFCN (dpotri, DPOTRI, (F77_CONST_CHAR_ARG2 ("L", 1), n,
                                   v, n, info
                                   F77_CHAR_ARG_LEN (1)));

      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
        {
          if (is_upper)
            for (octave_idx_type j = 0; j < r_nc; j++)
              for (octave_idx_type i = j+1; i < r_nr; i++)
                tmp.xelem (i, j) = tmp.xelem (j, i);
          else
            for (octave_idx_type j = 0; j < r_nc; j++)
              for (octave_idx_type i = j+1; i < r_nr; i++)
                tmp.xelem (j, i) = tmp.xelem (i, j);
        }

      retval = tmp;
    }

  return retval;
}

static FloatMatrix
chol2inv_internal (const FloatMatrix& r, bool is_upper = true)
{
  FloatMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr != r_nc)
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  octave_idx_type n = r_nc;
  octave_idx_type info = 0;

  FloatMatrix tmp = r;
  float *v = tmp.fortran_vec ();

  if (info == 0)
    {
      if (is_upper)
        F77_XFCN (spotri, SPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
                                   v, n, info
                                   F77_CHAR_ARG_LEN (1)));
      else
        F77_XFCN (spotri, SPOTRI, (F77_CONST_CHAR_ARG2 ("L", 1), n,
                                   v, n, info
                                   F77_CHAR_ARG_LEN (1)));

      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
        {
          if (is_upper)
            for (octave_idx_type j = 0; j < r_nc; j++)
              for (octave_idx_type i = j+1; i < r_nr; i++)
                tmp.xelem (i, j) = tmp.xelem (j, i);
          else
            for (octave_idx_type j = 0; j < r_nc; j++)
              for (octave_idx_type i = j+1; i < r_nr; i++)
                tmp.xelem (j, i) = tmp.xelem (i, j);
        }

      retval = tmp;
    }

  return retval;
}

static ComplexMatrix
chol2inv_internal (const ComplexMatrix& r, bool is_upper = true)
{
  ComplexMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr != r_nc)
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  octave_idx_type n = r_nc;
  octave_idx_type info;

  ComplexMatrix tmp = r;

  if (is_upper)
    F77_XFCN (zpotri, ZPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
                               F77_DBLE_CMPLX_ARG (tmp.fortran_vec ()), n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (zpotri, ZPOTRI, (F77_CONST_CHAR_ARG2 ("L", 1), n,
                               F77_DBLE_CMPLX_ARG (tmp.fortran_vec ()), n, info
                               F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of doing this (or
  // faster for that matter :-)), please let me know!

  if (n > 1)
    {
      if (is_upper)
        for (octave_idx_type j = 0; j < r_nc; j++)
          for (octave_idx_type i = j+1; i < r_nr; i++)
            tmp.xelem (i, j) = tmp.xelem (j, i);
      else
        for (octave_idx_type j = 0; j < r_nc; j++)
          for (octave_idx_type i = j+1; i < r_nr; i++)
            tmp.xelem (j, i) = tmp.xelem (i, j);
    }

  retval = tmp;

  return retval;
}

static FloatComplexMatrix
chol2inv_internal (const FloatComplexMatrix& r, bool is_upper = true)
{
  FloatComplexMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr != r_nc)
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  octave_idx_type n = r_nc;
  octave_idx_type info;

  FloatComplexMatrix tmp = r;

  if (is_upper)
    F77_XFCN (cpotri, CPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
                               F77_CMPLX_ARG (tmp.fortran_vec ()), n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (cpotri, CPOTRI, (F77_CONST_CHAR_ARG2 ("L", 1), n,
                               F77_CMPLX_ARG (tmp.fortran_vec ()), n, info
                               F77_CHAR_ARG_LEN (1)));

  // If someone thinks of a more graceful way of doing this (or
  // faster for that matter :-)), please let me know!

  if (n > 1)
    {
      if (is_upper)
        for (octave_idx_type j = 0; j < r_nc; j++)
          for (octave_idx_type i = j+1; i < r_nr; i++)
            tmp.xelem (i, j) = tmp.xelem (j, i);
      else
        for (octave_idx_type j = 0; j < r_nc; j++)
          for (octave_idx_type i = j+1; i < r_nr; i++)
            tmp.xelem (j, i) = tmp.xelem (i, j);
    }

  retval = tmp;

  return retval;
}

template <typename T>
T
chol2inv (const T& r)
{
  return chol2inv_internal (r);
}

// Compute the inverse of a matrix using the Cholesky factorization.
template <typename T>
T
chol<T>::inverse (void) const
{
  return chol2inv_internal (chol_mat, is_upper);
}

template <typename T>
void
chol<T>::set (const T& R)
{
  if (! R.is_square ())
    (*current_liboctave_error_handler) ("chol: requires square matrix");

  chol_mat = R;
}

#if ! defined (HAVE_QRUPDATE)

template <typename T>
void
chol<T>::update (const VT& u)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  init (chol_mat.hermitian () * chol_mat + T (u) * T (u).hermitian (),
        true, false);
}

template <typename T>
static bool
singular (const T& a)
{
  static typename T::element_type zero (0);
  for (octave_idx_type i = 0; i < a.rows (); i++)
    if (a(i,i) == zero) return true;
  return false;
}

template <typename T>
octave_idx_type
chol<T>::downdate (const VT& u)
{
  warn_qrupdate_once ();

  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  if (singular (chol_mat))
    info = 2;
  else
    {
      info = init (chol_mat.hermitian () * chol_mat
                   - T (u) * T (u).hermitian (), true, false);
      if (info) info = 1;
    }

  return info;
}

template <typename T>
octave_idx_type
chol<T>::insert_sym (const VT& u, octave_idx_type j)
{
  static typename T::element_type zero (0);

  warn_qrupdate_once ();

  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");

  if (singular (chol_mat))
    info = 2;
  else if (std::imag (u(j)) != zero)
    info = 3;
  else
    {
      T a = chol_mat.hermitian () * chol_mat;
      T a1 (n+1, n+1);
      for (octave_idx_type k = 0; k < n+1; k++)
        for (octave_idx_type l = 0; l < n+1; l++)
          {
            if (l == j)
              a1(k, l) = u(k);
            else if (k == j)
              a1(k, l) = std::conj (u(l));
            else
              a1(k, l) = a(k < j ? k : k-1, l < j ? l : l-1);
          }
      info = init (a1, true, false);
      if (info) info = 1;
    }

  return info;
}

template <typename T>
void
chol<T>::delete_sym (octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");

  T a = chol_mat.hermitian () * chol_mat;
  a.delete_elements (1, idx_vector (j));
  a.delete_elements (0, idx_vector (j));
  init (a, true, false);
}

template <typename T>
void
chol<T>::shift_sym (octave_idx_type i, octave_idx_type j)
{
  warn_qrupdate_once ();

  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");

  T a = chol_mat.hermitian () * chol_mat;
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

  init (a.index (idx_vector (p), idx_vector (p)), true, false);
}

#endif

// Specializations.

template <>
octave_idx_type
chol<Matrix>::init (const Matrix& a, bool upper, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("chol: requires square matrix");

  octave_idx_type n = a_nc;
  octave_idx_type info;

  is_upper = upper;

  chol_mat.clear (n, n);
  if (is_upper)
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i <= j; i++)
          chol_mat.xelem (i, j) = a(i, j);
        for (octave_idx_type i = j+1; i < n; i++)
          chol_mat.xelem (i, j) = 0.0;
      }
  else
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i < j; i++)
          chol_mat.xelem (i, j) = 0.0;
        for (octave_idx_type i = j; i < n; i++)
          chol_mat.xelem (i, j) = a(i, j);
      }
  double *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  double anorm = 0;
  if (calc_cond)
    anorm = xnorm (a, 1);

  if (is_upper)
    F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1), n, h, n, info
                               F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info > 0)
    chol_mat.resize (info - 1, info - 1);
  else if (calc_cond)
    {
      octave_idx_type dpocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<double> z (dim_vector (3*n, 1));
      double *pz = z.fortran_vec ();
      Array<octave_idx_type> iz (dim_vector (n, 1));
      octave_idx_type *piz = iz.fortran_vec ();
      if (is_upper)
        F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
                                   n, anorm, xrcond, pz, piz, dpocon_info
                                   F77_CHAR_ARG_LEN (1)));
      else
        F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 ("L", 1), n, h,
                                   n, anorm, xrcond, pz, piz, dpocon_info
                                   F77_CHAR_ARG_LEN (1)));

      if (dpocon_info != 0)
        info = -1;
    }

  return info;
}

#if defined (HAVE_QRUPDATE)

template <>
void
chol<Matrix>::update (const ColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  ColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, w, n);

  F77_XFCN (dch1up, DCH1UP, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             utmp.fortran_vec (), w));
}

template <>
octave_idx_type
chol<Matrix>::downdate (const ColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  ColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, w, n);

  F77_XFCN (dch1dn, DCH1DN, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             utmp.fortran_vec (), w, info));

  return info;
}

template <>
octave_idx_type
chol<Matrix>::insert_sym (const ColumnVector& u, octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");

  ColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, w, n);

  chol_mat.resize (n+1, n+1);

  F77_XFCN (dchinx, DCHINX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             j + 1, utmp.fortran_vec (), w, info));

  return info;
}

template <>
void
chol<Matrix>::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");

  OCTAVE_LOCAL_BUFFER (double, w, n);

  F77_XFCN (dchdex, DCHDEX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             j + 1, w));

  chol_mat.resize (n-1, n-1);
}

template <>
void
chol<Matrix>::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");

  OCTAVE_LOCAL_BUFFER (double, w, 2*n);

  F77_XFCN (dchshx, DCHSHX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             i + 1, j + 1, w));
}

#endif

template <>
octave_idx_type
chol<FloatMatrix>::init (const FloatMatrix& a, bool upper, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("chol: requires square matrix");

  octave_idx_type n = a_nc;
  octave_idx_type info;

  is_upper = upper;

  chol_mat.clear (n, n);
  if (is_upper)
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i <= j; i++)
          chol_mat.xelem (i, j) = a(i, j);
        for (octave_idx_type i = j+1; i < n; i++)
          chol_mat.xelem (i, j) = 0.0f;
      }
  else
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i < j; i++)
          chol_mat.xelem (i, j) = 0.0f;
        for (octave_idx_type i = j; i < n; i++)
          chol_mat.xelem (i, j) = a(i, j);
      }
  float *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  float anorm = 0;
  if (calc_cond)
    anorm = xnorm (a, 1);

  if (is_upper)
    F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1), n, h, n, info
                               F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info > 0)
    chol_mat.resize (info - 1, info - 1);
  else if (calc_cond)
    {
      octave_idx_type spocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<float> z (dim_vector (3*n, 1));
      float *pz = z.fortran_vec ();
      Array<octave_idx_type> iz (dim_vector (n, 1));
      octave_idx_type *piz = iz.fortran_vec ();
      if (is_upper)
        F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
                                   n, anorm, xrcond, pz, piz, spocon_info
                                   F77_CHAR_ARG_LEN (1)));
      else
        F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 ("L", 1), n, h,
                                   n, anorm, xrcond, pz, piz, spocon_info
                                   F77_CHAR_ARG_LEN (1)));

      if (spocon_info != 0)
        info = -1;
    }

  return info;
}

#if defined (HAVE_QRUPDATE)

template <>
void
chol<FloatMatrix>::update (const FloatColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  FloatColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, w, n);

  F77_XFCN (sch1up, SCH1UP, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             utmp.fortran_vec (), w));
}

template <>
octave_idx_type
chol<FloatMatrix>::downdate (const FloatColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  FloatColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, w, n);

  F77_XFCN (sch1dn, SCH1DN, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             utmp.fortran_vec (), w, info));

  return info;
}

template <>
octave_idx_type
chol<FloatMatrix>::insert_sym (const FloatColumnVector& u, octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");

  FloatColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, w, n);

  chol_mat.resize (n+1, n+1);

  F77_XFCN (schinx, SCHINX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             j + 1, utmp.fortran_vec (), w, info));

  return info;
}

template <>
void
chol<FloatMatrix>::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");

  OCTAVE_LOCAL_BUFFER (float, w, n);

  F77_XFCN (schdex, SCHDEX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             j + 1, w));

  chol_mat.resize (n-1, n-1);
}

template <>
void
chol<FloatMatrix>::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");

  OCTAVE_LOCAL_BUFFER (float, w, 2*n);

  F77_XFCN (schshx, SCHSHX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                             i + 1, j + 1, w));
}

#endif

template <>
octave_idx_type
chol<ComplexMatrix>::init (const ComplexMatrix& a, bool upper, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("chol: requires square matrix");

  octave_idx_type n = a_nc;
  octave_idx_type info;

  is_upper = upper;

  chol_mat.clear (n, n);
  if (is_upper)
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i <= j; i++)
          chol_mat.xelem (i, j) = a(i, j);
        for (octave_idx_type i = j+1; i < n; i++)
          chol_mat.xelem (i, j) = 0.0;
      }
  else
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i < j; i++)
          chol_mat.xelem (i, j) = 0.0;
        for (octave_idx_type i = j; i < n; i++)
          chol_mat.xelem (i, j) = a(i, j);
      }
  Complex *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  double anorm = 0;
  if (calc_cond)
    anorm = xnorm (a, 1);

  if (is_upper)
    F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, F77_DBLE_CMPLX_ARG (h), n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1), n, F77_DBLE_CMPLX_ARG (h), n, info
                               F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info > 0)
    chol_mat.resize (info - 1, info - 1);
  else if (calc_cond)
    {
      octave_idx_type zpocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<Complex> z (dim_vector (2*n, 1));
      Complex *pz = z.fortran_vec ();
      Array<double> rz (dim_vector (n, 1));
      double *prz = rz.fortran_vec ();
      F77_XFCN (zpocon, ZPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, F77_DBLE_CMPLX_ARG (h),
                                 n, anorm, xrcond, F77_DBLE_CMPLX_ARG (pz), prz, zpocon_info
                                 F77_CHAR_ARG_LEN (1)));

      if (zpocon_info != 0)
        info = -1;
    }

  return info;
}

#if defined (HAVE_QRUPDATE)

template <>
void
chol<ComplexMatrix>::update (const ComplexColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  ComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, rw, n);

  F77_XFCN (zch1up, ZCH1UP, (n, F77_DBLE_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()), rw));
}

template <>
octave_idx_type
chol<ComplexMatrix>::downdate (const ComplexColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  ComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, rw, n);

  F77_XFCN (zch1dn, ZCH1DN, (n, F77_DBLE_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()), rw, info));

  return info;
}

template <>
octave_idx_type
chol<ComplexMatrix>::insert_sym (const ComplexColumnVector& u,
                                 octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");

  ComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (double, rw, n);

  chol_mat.resize (n+1, n+1);

  F77_XFCN (zchinx, ZCHINX, (n, F77_DBLE_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             j + 1, F77_DBLE_CMPLX_ARG (utmp.fortran_vec ()), rw, info));

  return info;
}

template <>
void
chol<ComplexMatrix>::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");

  OCTAVE_LOCAL_BUFFER (double, rw, n);

  F77_XFCN (zchdex, ZCHDEX, (n, F77_DBLE_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             j + 1, rw));

  chol_mat.resize (n-1, n-1);
}

template <>
void
chol<ComplexMatrix>::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");

  OCTAVE_LOCAL_BUFFER (Complex, w, n);
  OCTAVE_LOCAL_BUFFER (double, rw, n);

  F77_XFCN (zchshx, ZCHSHX, (n, F77_DBLE_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             i + 1, j + 1, F77_DBLE_CMPLX_ARG (w), rw));
}

#endif

template <>
octave_idx_type
chol<FloatComplexMatrix>::init (const FloatComplexMatrix& a, bool upper,
                                bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("chol: requires square matrix");

  octave_idx_type n = a_nc;
  octave_idx_type info;

  is_upper = upper;

  chol_mat.clear (n, n);
  if (is_upper)
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i <= j; i++)
          chol_mat.xelem (i, j) = a(i, j);
        for (octave_idx_type i = j+1; i < n; i++)
          chol_mat.xelem (i, j) = 0.0f;
      }
  else
    for (octave_idx_type j = 0; j < n; j++)
      {
        for (octave_idx_type i = 0; i < j; i++)
          chol_mat.xelem (i, j) = 0.0f;
        for (octave_idx_type i = j; i < n; i++)
          chol_mat.xelem (i, j) = a(i, j);
      }
  FloatComplex *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  float anorm = 0;
  if (calc_cond)
    anorm = xnorm (a, 1);

  if (is_upper)
    F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, F77_CMPLX_ARG (h), n, info
                               F77_CHAR_ARG_LEN (1)));
  else
    F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("L", 1), n, F77_CMPLX_ARG (h), n, info
                               F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info > 0)
    chol_mat.resize (info - 1, info - 1);
  else if (calc_cond)
    {
      octave_idx_type cpocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<FloatComplex> z (dim_vector (2*n, 1));
      FloatComplex *pz = z.fortran_vec ();
      Array<float> rz (dim_vector (n, 1));
      float *prz = rz.fortran_vec ();
      F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, F77_CMPLX_ARG (h),
                                 n, anorm, xrcond, F77_CMPLX_ARG (pz), prz, cpocon_info
                                 F77_CHAR_ARG_LEN (1)));

      if (cpocon_info != 0)
        info = -1;
    }

  return info;
}

#if defined (HAVE_QRUPDATE)

template <>
void
chol<FloatComplexMatrix>::update (const FloatComplexColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  FloatComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, rw, n);

  F77_XFCN (cch1up, CCH1UP, (n, F77_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             F77_CMPLX_ARG (utmp.fortran_vec ()), rw));
}

template <>
octave_idx_type
chol<FloatComplexMatrix>::downdate (const FloatComplexColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n)
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  FloatComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, rw, n);

  F77_XFCN (cch1dn, CCH1DN, (n, F77_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             F77_CMPLX_ARG (utmp.fortran_vec ()), rw, info));

  return info;
}

template <>
octave_idx_type
chol<FloatComplexMatrix>::insert_sym (const FloatComplexColumnVector& u,
                                      octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.numel () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");

  FloatComplexColumnVector utmp = u;

  OCTAVE_LOCAL_BUFFER (float, rw, n);

  chol_mat.resize (n+1, n+1);

  F77_XFCN (cchinx, CCHINX, (n, F77_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             j + 1, F77_CMPLX_ARG (utmp.fortran_vec ()), rw, info));

  return info;
}

template <>
void
chol<FloatComplexMatrix>::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");

  OCTAVE_LOCAL_BUFFER (float, rw, n);

  F77_XFCN (cchdex, CCHDEX, (n, F77_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             j + 1, rw));

  chol_mat.resize (n-1, n-1);
}

template <>
void
chol<FloatComplexMatrix>::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("cholshift: index out of range");

  OCTAVE_LOCAL_BUFFER (FloatComplex, w, n);
  OCTAVE_LOCAL_BUFFER (float, rw, n);

  F77_XFCN (cchshx, CCHSHX, (n, F77_CMPLX_ARG (chol_mat.fortran_vec ()), chol_mat.rows (),
                             i + 1, j + 1, F77_CMPLX_ARG (w), rw));
}

#endif

// Instantiations we need.

template class chol<Matrix>;

template class chol<FloatMatrix>;

template class chol<ComplexMatrix>;

template class chol<FloatComplexMatrix>;

template Matrix
chol2inv<Matrix> (const Matrix& r);

template ComplexMatrix
chol2inv<ComplexMatrix> (const ComplexMatrix& r);

template FloatMatrix
chol2inv<FloatMatrix> (const FloatMatrix& r);

template FloatComplexMatrix
chol2inv<FloatComplexMatrix> (const FloatComplexMatrix& r);
