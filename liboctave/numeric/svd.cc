/*

Copyright (C) 1994-2015 John W. Eaton

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

#include <cassert>

#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "fDiagMatrix.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "svd.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgesvd, DGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&, double*,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgesdd, DGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             double*, const octave_idx_type&, double*,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type *,
                             octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgesvd, SGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&, float*,
                             float*, const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgesdd, SGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             float*, const octave_idx_type&, float*,
                             float*, const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type *,
                             octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgesvd, ZGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&,
                             double*, Complex*, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, double*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgesdd, ZGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             Complex*, const octave_idx_type&,
                             double*, Complex*, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*,
                             const octave_idx_type&, double*,
                             octave_idx_type *, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (cgesvd, CGESVD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&, float*,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgesdd, CGESDD) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&, float*,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             float*, octave_idx_type *, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);
}

template <typename T>
T
svd<T>::left_singular_matrix (void) const
{
  if (type_computed == svd::sigma_only)
    (*current_liboctave_error_handler)
      ("svd: U not computed because type == svd::sigma_only");

  return left_sm;
}

template <typename T>
T
svd<T>::right_singular_matrix (void) const
{
  if (type_computed == svd::sigma_only)
    (*current_liboctave_error_handler)
      ("svd: V not computed because type == svd::sigma_only");

  return right_sm;
}

template <typename T>
octave_idx_type
svd<T>::empty_init (octave_idx_type nr, octave_idx_type nc, svd::type svd_type)
{
  assert (nr == 0 || nc == 0);

  static typename T::element_type zero (0);
  static typename T::element_type one (1);

  switch (svd_type)
    {
    case svd::std:
      left_sm = T (nr, nr, zero);
      for (octave_idx_type i = 0; i < nr; i++)
        left_sm.xelem (i, i) = one;
      sigma = DM_T (nr, nc);
      right_sm = T (nc, nc, zero);
      for (octave_idx_type i = 0; i < nc; i++)
        right_sm.xelem (i, i) = one;
      break;

    case svd::economy:
      left_sm = T (nr, 0, zero);
      sigma = DM_T (0, 0);
      right_sm = T (0, nc, zero);
      break;

    case svd::sigma_only:
    default:
      sigma = DM_T (0, 1);
      break;
    }

  return 0;
}

// Specializations.

template <>
octave_idx_type
svd<Matrix>::init (const Matrix& a, svd::type svd_type, svd::driver svd_driver)
{
  octave_idx_type info = 0;

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    return empty_init (m, n, svd_type);

  Matrix atmp = a;
  double *tmp_data = atmp.fortran_vec ();

  octave_idx_type min_mn = m < n ? m : n;

  char jobu = 'A';
  char jobv = 'A';

  octave_idx_type ncol_u = m;
  octave_idx_type nrow_vt = n;
  octave_idx_type nrow_s = m;
  octave_idx_type ncol_s = n;

  switch (svd_type)
    {
    case svd::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case svd::sigma_only:

      // Note:  for this case, both jobu and jobv should be 'N', but
      // there seems to be a bug in dgesvd from Lapack V2.0.  To
      // demonstrate the bug, set both jobu and jobv to 'N' and find
      // the singular values of [eye(3), eye(3)].  The result is
      // [-sqrt(2), -sqrt(2), -sqrt(2)].
      //
      // For Lapack 3.0, this problem seems to be fixed.

      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  type_computed = svd_type;

  if (! (jobu == 'N' || jobu == 'O'))
    left_sm.resize (m, ncol_u);

  double *u = left_sm.fortran_vec ();

  sigma.resize (nrow_s, ncol_s);
  double *s_vec = sigma.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm.resize (nrow_vt, n);

  double *vt = right_sm.fortran_vec ();

  // Query DGESVD for the correct dimension of WORK.

  octave_idx_type lwork = -1;

  Array<double> work (dim_vector (1, 1));

  octave_idx_type one = 1;
  octave_idx_type m1 = std::max (m, one);
  octave_idx_type nrow_vt1 = std::max (nrow_vt, one);

  if (svd_driver == svd::GESVD)
    {
      F77_XFCN (dgesvd, DGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (dgesvd, DGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

    }
  else if (svd_driver == svd::GESDD)
    {
      assert (jobu == jobv);
      char jobz = jobu;
      OCTAVE_LOCAL_BUFFER (octave_idx_type, iwork, 8*min_mn);

      F77_XFCN (dgesdd, DGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt, nrow_vt1,
                                 work.fortran_vec (), lwork, iwork, info
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (dgesdd, DGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt, nrow_vt1,
                                 work.fortran_vec (), lwork, iwork, info
                                 F77_CHAR_ARG_LEN (1)));

    }
  else
    abort ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm = right_sm.transpose ();

  return info;
}

template <>
octave_idx_type
svd<FloatMatrix>::init (const FloatMatrix& a, svd::type svd_type,
                        svd::driver svd_driver)
{
  octave_idx_type info;

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    return empty_init (m, n, svd_type);

  FloatMatrix atmp = a;
  float *tmp_data = atmp.fortran_vec ();

  octave_idx_type min_mn = m < n ? m : n;

  char jobu = 'A';
  char jobv = 'A';

  octave_idx_type ncol_u = m;
  octave_idx_type nrow_vt = n;
  octave_idx_type nrow_s = m;
  octave_idx_type ncol_s = n;

  switch (svd_type)
    {
    case svd::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case svd::sigma_only:

      // Note:  for this case, both jobu and jobv should be 'N', but
      // there seems to be a bug in dgesvd from Lapack V2.0.  To
      // demonstrate the bug, set both jobu and jobv to 'N' and find
      // the singular values of [eye(3), eye(3)].  The result is
      // [-sqrt(2), -sqrt(2), -sqrt(2)].
      //
      // For Lapack 3.0, this problem seems to be fixed.

      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  type_computed = svd_type;

  if (! (jobu == 'N' || jobu == 'O'))
    left_sm.resize (m, ncol_u);

  float *u = left_sm.fortran_vec ();

  sigma.resize (nrow_s, ncol_s);
  float *s_vec = sigma.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm.resize (nrow_vt, n);

  float *vt = right_sm.fortran_vec ();

  // Query SGESVD for the correct dimension of WORK.

  octave_idx_type lwork = -1;

  Array<float> work (dim_vector (1, 1));

  octave_idx_type one = 1;
  octave_idx_type m1 = std::max (m, one);
  octave_idx_type nrow_vt1 = std::max (nrow_vt, one);

  if (svd_driver == svd::GESVD)
    {
      F77_XFCN (sgesvd, SGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (sgesvd, SGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

    }
  else if (svd_driver == svd::GESDD)
    {
      assert (jobu == jobv);
      char jobz = jobu;
      OCTAVE_LOCAL_BUFFER (octave_idx_type, iwork, 8*min_mn);

      F77_XFCN (sgesdd, SGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt, nrow_vt1,
                                 work.fortran_vec (), lwork, iwork, info
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (sgesdd, SGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt, nrow_vt1,
                                 work.fortran_vec (), lwork, iwork, info
                                 F77_CHAR_ARG_LEN (1)));

    }
  else
    abort ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm = right_sm.transpose ();

  return info;
}

template <>
octave_idx_type
svd<ComplexMatrix>::init (const ComplexMatrix& a, svd::type svd_type,
                          svd::driver svd_driver)
{
  octave_idx_type info;

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    return empty_init (m, n, svd_type);

  ComplexMatrix atmp = a;
  Complex *tmp_data = atmp.fortran_vec ();

  octave_idx_type min_mn = m < n ? m : n;
  octave_idx_type max_mn = m > n ? m : n;

  char jobu = 'A';
  char jobv = 'A';

  octave_idx_type ncol_u = m;
  octave_idx_type nrow_vt = n;
  octave_idx_type nrow_s = m;
  octave_idx_type ncol_s = n;

  switch (svd_type)
    {
    case svd::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case svd::sigma_only:

      // Note:  for this case, both jobu and jobv should be 'N', but
      // there seems to be a bug in dgesvd from Lapack V2.0.  To
      // demonstrate the bug, set both jobu and jobv to 'N' and find
      // the singular values of [eye(3), eye(3)].  The result is
      // [-sqrt(2), -sqrt(2), -sqrt(2)].
      //
      // For Lapack 3.0, this problem seems to be fixed.

      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  type_computed = svd_type;

  if (! (jobu == 'N' || jobu == 'O'))
    left_sm.resize (m, ncol_u);

  Complex *u = left_sm.fortran_vec ();

  sigma.resize (nrow_s, ncol_s);
  double *s_vec = sigma.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm.resize (nrow_vt, n);

  Complex *vt = right_sm.fortran_vec ();

  // Query ZGESVD for the correct dimension of WORK.

  octave_idx_type lwork = -1;

  Array<Complex> work (dim_vector (1, 1));

  octave_idx_type one = 1;
  octave_idx_type m1 = std::max (m, one);
  octave_idx_type nrow_vt1 = std::max (nrow_vt, one);

  if (svd_driver == svd::GESVD)
    {
      octave_idx_type lrwork = 5*max_mn;
      Array<double> rwork (dim_vector (lrwork, 1));

      F77_XFCN (zgesvd, ZGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0).real ());
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (zgesvd, ZGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
    }
  else if (svd_driver == svd::GESDD)
    {
      assert (jobu == jobv);
      char jobz = jobu;

      octave_idx_type lrwork;
      if (jobz == 'N')
        lrwork = 7*min_mn;
      else
        lrwork = 5*min_mn*min_mn + 5*min_mn;
      Array<double> rwork (dim_vector (lrwork, 1));

      OCTAVE_LOCAL_BUFFER (octave_idx_type, iwork, 8*min_mn);

      F77_XFCN (zgesdd, ZGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0).real ());
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (zgesdd, ZGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)));
    }
  else
    abort ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm = right_sm.hermitian ();

  return info;
}

template <>
octave_idx_type
svd<FloatComplexMatrix>::init (const FloatComplexMatrix& a, svd::type svd_type,
                              svd::driver svd_driver)
{
  octave_idx_type info;

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    return empty_init (m, n, svd_type);

  FloatComplexMatrix atmp = a;
  FloatComplex *tmp_data = atmp.fortran_vec ();

  octave_idx_type min_mn = m < n ? m : n;
  octave_idx_type max_mn = m > n ? m : n;

  char jobu = 'A';
  char jobv = 'A';

  octave_idx_type ncol_u = m;
  octave_idx_type nrow_vt = n;
  octave_idx_type nrow_s = m;
  octave_idx_type ncol_s = n;

  switch (svd_type)
    {
    case svd::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case svd::sigma_only:

      // Note:  for this case, both jobu and jobv should be 'N', but
      // there seems to be a bug in dgesvd from Lapack V2.0.  To
      // demonstrate the bug, set both jobu and jobv to 'N' and find
      // the singular values of [eye(3), eye(3)].  The result is
      // [-sqrt(2), -sqrt(2), -sqrt(2)].
      //
      // For Lapack 3.0, this problem seems to be fixed.

      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  type_computed = svd_type;

  if (! (jobu == 'N' || jobu == 'O'))
    left_sm.resize (m, ncol_u);

  FloatComplex *u = left_sm.fortran_vec ();

  sigma.resize (nrow_s, ncol_s);
  float *s_vec = sigma.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm.resize (nrow_vt, n);

  FloatComplex *vt = right_sm.fortran_vec ();

  // Query CGESVD for the correct dimension of WORK.

  octave_idx_type lwork = -1;

  Array<FloatComplex> work (dim_vector (1, 1));

  octave_idx_type one = 1;
  octave_idx_type m1 = std::max (m, one);
  octave_idx_type nrow_vt1 = std::max (nrow_vt, one);

  if (svd_driver == svd::GESVD)
    {
      octave_idx_type lrwork = 5*max_mn;
      Array<float> rwork (dim_vector (lrwork, 1));

      F77_XFCN (cgesvd, CGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0).real ());
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (cgesvd, CGESVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
    }
  else if (svd_driver == svd::GESDD)
    {
      assert (jobu == jobv);
      char jobz = jobu;

      octave_idx_type lrwork;
      if (jobz == 'N')
        lrwork = 5*min_mn;
      else
        lrwork = min_mn * std::max (5*min_mn+7, 2*max_mn+2*min_mn+1);
      Array<float> rwork (dim_vector (lrwork, 1));

      OCTAVE_LOCAL_BUFFER (octave_idx_type, iwork, 8*min_mn);

      F77_XFCN (cgesdd, CGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)));

      lwork = static_cast<octave_idx_type> (work(0).real ());
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (cgesdd, CGESDD, (F77_CONST_CHAR_ARG2 (&jobz, 1),
                                 m, n, tmp_data, m1, s_vec, u, m1, vt,
                                 nrow_vt1, work.fortran_vec (), lwork,
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)));
    }
  else
    abort ();

  if (! (jobv == 'N' || jobv == 'O'))
    right_sm = right_sm.hermitian ();

  return info;
}

// Instantiations we need.

template class svd<Matrix>;

template class svd<FloatMatrix>;

template class svd<ComplexMatrix>;

template class svd<FloatComplexMatrix>;
