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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "CMatrix.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "schur.h"

typedef octave_idx_type (*double_selector) (const double&, const double&);
typedef octave_idx_type (*float_selector) (const float&, const float&);
typedef octave_idx_type (*complex_selector) (const Complex&);
typedef octave_idx_type (*float_complex_selector) (const FloatComplex&);

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeesx, DGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             double_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE*,
                             const F77_INT&, F77_INT&,
                             F77_DBLE*, F77_DBLE*, F77_DBLE*, const F77_INT&,
                             F77_DBLE&, F77_DBLE&, F77_DBLE*, const F77_INT&,
                             F77_INT*, const F77_INT&,
                             F77_INT*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgeesx, SGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             float_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_REAL*,
                             const F77_INT&, F77_INT&,
                             F77_REAL*, F77_REAL*, F77_REAL*, const F77_INT&,
                             F77_REAL&, F77_REAL&, F77_REAL*, const F77_INT&,
                             F77_INT*, const F77_INT&,
                             F77_INT*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgeesx, ZGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             complex_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_INT&,
                             F77_DBLE_CMPLX*, F77_DBLE_CMPLX*, const F77_INT&,
                             F77_DBLE&, F77_DBLE&, F77_DBLE_CMPLX*,
                             const F77_INT&, F77_DBLE*,
                             F77_INT*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zrsf2csf, ZRSF2CSF) (const F77_INT&, F77_DBLE_CMPLX *,
                                 F77_DBLE_CMPLX *, F77_DBLE *, F77_DBLE *);
  F77_RET_T
  F77_FUNC (cgeesx, CGEESX) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             float_complex_selector,
                             F77_CONST_CHAR_ARG_DECL,
                             const F77_INT&, F77_CMPLX*,
                             const F77_INT&, F77_INT&,
                             F77_CMPLX*, F77_CMPLX*,
                             const F77_INT&, F77_REAL&, F77_REAL&,
                             F77_CMPLX*, const F77_INT&,
                             F77_REAL*, F77_INT*, F77_INT&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (crsf2csf, CRSF2CSF) (const F77_INT&, F77_CMPLX *,
                                 F77_CMPLX *, F77_REAL *, F77_REAL *);
}

// For real types.

template <typename T>
static octave_idx_type
select_ana (const T& a, const T&)
{
  return (a < 0.0);
}

template <typename T>
static octave_idx_type
select_dig (const T& a, const T& b)
{
  return (hypot (a, b) < 1.0);
}

// For complex types.

template <typename T>
static octave_idx_type
select_ana (const T& a)
{
  return a.real () < 0.0;
}

template <typename T>
static octave_idx_type
select_dig (const T& a)
{
  return (abs (a) < 1.0);
}

template <>
octave_idx_type
schur<Matrix>::init (const Matrix& a, const std::string& ord, bool calc_unitary)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("schur: requires square matrix");

  if (a_nr == 0)
    {
      schur_mat.clear ();
      unitary_mat.clear ();
      return 0;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile double_selector selector = 0;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana<double>;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig<double>;

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 8 * n;
  octave_idx_type liwork = 1;
  octave_idx_type info;
  octave_idx_type sdim;
  double rconde;
  double rcondv;

  schur_mat = a;

  if (calc_unitary)
    unitary_mat.clear (n, n);

  double *s = schur_mat.fortran_vec ();
  double *q = unitary_mat.fortran_vec ();

  Array<double> wr (dim_vector (n, 1));
  double *pwr = wr.fortran_vec ();

  Array<double> wi (dim_vector (n, 1));
  double *pwi = wi.fortran_vec ();

  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  // BWORK is not referenced for the non-ordered Schur routine.
  octave_idx_type ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<octave_idx_type> bwork (dim_vector (ntmp, 1));
  octave_idx_type *pbwork = bwork.fortran_vec ();

  Array<octave_idx_type> iwork (dim_vector (liwork, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  F77_XFCN (dgeesx, DGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
                             F77_CONST_CHAR_ARG2 (&sort, 1),
                             selector,
                             F77_CONST_CHAR_ARG2 (&sense, 1),
                             n, s, n, sdim, pwr, pwi, q, n, rconde, rcondv,
                             pwork, lwork, piwork, liwork, pbwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
octave_idx_type
schur<FloatMatrix>::init (const FloatMatrix& a, const std::string& ord,
                          bool calc_unitary)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      schur_mat.clear ();
      unitary_mat.clear ();
      return 0;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile float_selector selector = 0;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana<float>;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig<float>;

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 8 * n;
  octave_idx_type liwork = 1;
  octave_idx_type info;
  octave_idx_type sdim;
  float rconde;
  float rcondv;

  schur_mat = a;

  if (calc_unitary)
    unitary_mat.clear (n, n);

  float *s = schur_mat.fortran_vec ();
  float *q = unitary_mat.fortran_vec ();

  Array<float> wr (dim_vector (n, 1));
  float *pwr = wr.fortran_vec ();

  Array<float> wi (dim_vector (n, 1));
  float *pwi = wi.fortran_vec ();

  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  // BWORK is not referenced for the non-ordered Schur routine.
  octave_idx_type ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<octave_idx_type> bwork (dim_vector (ntmp, 1));
  octave_idx_type *pbwork = bwork.fortran_vec ();

  Array<octave_idx_type> iwork (dim_vector (liwork, 1));
  octave_idx_type *piwork = iwork.fortran_vec ();

  F77_XFCN (sgeesx, SGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
                             F77_CONST_CHAR_ARG2 (&sort, 1),
                             selector,
                             F77_CONST_CHAR_ARG2 (&sense, 1),
                             n, s, n, sdim, pwr, pwi, q, n, rconde, rcondv,
                             pwork, lwork, piwork, liwork, pbwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
octave_idx_type
schur<ComplexMatrix>::init (const ComplexMatrix& a, const std::string& ord,
                            bool calc_unitary)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      schur_mat.clear ();
      unitary_mat.clear ();
      return 0;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile complex_selector selector = 0;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana<Complex>;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig<Complex>;

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 8 * n;
  octave_idx_type info;
  octave_idx_type sdim;
  double rconde;
  double rcondv;

  schur_mat = a;
  if (calc_unitary)
    unitary_mat.clear (n, n);

  Complex *s = schur_mat.fortran_vec ();
  Complex *q = unitary_mat.fortran_vec ();

  Array<double> rwork (dim_vector (n, 1));
  double *prwork = rwork.fortran_vec ();

  Array<Complex> w (dim_vector (n, 1));
  Complex *pw = w.fortran_vec ();

  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  octave_idx_type ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<octave_idx_type> bwork (dim_vector (ntmp, 1));
  octave_idx_type *pbwork = bwork.fortran_vec ();

  F77_XFCN (zgeesx, ZGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
                             F77_CONST_CHAR_ARG2 (&sort, 1),
                             selector,
                             F77_CONST_CHAR_ARG2 (&sense, 1),
                             n, F77_DBLE_CMPLX_ARG (s), n, sdim, F77_DBLE_CMPLX_ARG (pw), F77_DBLE_CMPLX_ARG (q), n, rconde, rcondv,
                             F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, pbwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
schur<ComplexMatrix>
rsf2csf<ComplexMatrix, Matrix> (const Matrix& s_arg, const Matrix& u_arg)
{
  ComplexMatrix s (s_arg);
  ComplexMatrix u (u_arg);

  octave_idx_type n = s.rows ();

  if (s.columns () != n || u.rows () != n || u.columns () != n)
    (*current_liboctave_error_handler)
      ("rsf2csf: inconsistent matrix dimensions");

  if (n > 0)
    {
      OCTAVE_LOCAL_BUFFER (double, c, n-1);
      OCTAVE_LOCAL_BUFFER (double, sx, n-1);

      F77_XFCN (zrsf2csf, ZRSF2CSF, (n, F77_DBLE_CMPLX_ARG (s.fortran_vec ()),
                                     F77_DBLE_CMPLX_ARG (u.fortran_vec ()), c, sx));
    }

  return schur<ComplexMatrix> (s, u);
}

template <>
octave_idx_type
schur<FloatComplexMatrix>::init (const FloatComplexMatrix& a,
                                 const std::string& ord, bool calc_unitary)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      schur_mat.clear ();
      unitary_mat.clear ();
      return 0;
    }

  // Workspace requirements may need to be fixed if any of the
  // following change.

  char jobvs;
  char sense = 'N';
  char sort = 'N';

  if (calc_unitary)
    jobvs = 'V';
  else
    jobvs = 'N';

  char ord_char = ord.empty () ? 'U' : ord[0];

  if (ord_char == 'A' || ord_char == 'D' || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile float_complex_selector selector = 0;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana<FloatComplex>;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig<FloatComplex>;

  octave_idx_type n = a_nc;
  octave_idx_type lwork = 8 * n;
  octave_idx_type info;
  octave_idx_type sdim;
  float rconde;
  float rcondv;

  schur_mat = a;
  if (calc_unitary)
    unitary_mat.clear (n, n);

  FloatComplex *s = schur_mat.fortran_vec ();
  FloatComplex *q = unitary_mat.fortran_vec ();

  Array<float> rwork (dim_vector (n, 1));
  float *prwork = rwork.fortran_vec ();

  Array<FloatComplex> w (dim_vector (n, 1));
  FloatComplex *pw = w.fortran_vec ();

  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  octave_idx_type ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<octave_idx_type> bwork (dim_vector (ntmp, 1));
  octave_idx_type *pbwork = bwork.fortran_vec ();

  F77_XFCN (cgeesx, CGEESX, (F77_CONST_CHAR_ARG2 (&jobvs, 1),
                             F77_CONST_CHAR_ARG2 (&sort, 1),
                             selector,
                             F77_CONST_CHAR_ARG2 (&sense, 1),
                             n, F77_CMPLX_ARG (s), n, sdim, F77_CMPLX_ARG (pw), F77_CMPLX_ARG (q), n, rconde, rcondv,
                             F77_CMPLX_ARG (pwork), lwork, prwork, pbwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
schur<FloatComplexMatrix>
rsf2csf<FloatComplexMatrix, FloatMatrix> (const FloatMatrix& s_arg, const FloatMatrix& u_arg)
{
  FloatComplexMatrix s (s_arg);
  FloatComplexMatrix u (u_arg);

  octave_idx_type n = s.rows ();

  if (s.columns () != n || u.rows () != n || u.columns () != n)
    (*current_liboctave_error_handler)
      ("rsf2csf: inconsistent matrix dimensions");

  if (n > 0)
    {
      OCTAVE_LOCAL_BUFFER (float, c, n-1);
      OCTAVE_LOCAL_BUFFER (float, sx, n-1);

      F77_XFCN (crsf2csf, CRSF2CSF, (n, F77_CMPLX_ARG (s.fortran_vec ()),
                                     F77_CMPLX_ARG (u.fortran_vec ()), c, sx));
    }

  return schur<FloatComplexMatrix> (s, u);
}

// Instantiations we need.

template class schur<ComplexMatrix>;

template class schur<FloatComplexMatrix>;

template class schur<FloatMatrix>;

template class schur<Matrix>;
