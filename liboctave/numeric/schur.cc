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

#include "Array.h"
#include "CMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"
#include "schur.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

// For real types.

static F77_INT
select_ana (const double& a, const double&)
{
  return (a < 0.0);
}

static F77_INT
select_dig (const double& a, const double& b)
{
  return (hypot (a, b) < 1.0);
}

static F77_INT
select_ana (const float& a, const float&)
{
  return (a < 0.0);
}

static F77_INT
select_dig (const float& a, const float& b)
{
  return (hypot (a, b) < 1.0);
}

// For complex types.

static F77_INT
select_ana (const F77_DBLE_CMPLX& a_arg)
{
  const Complex a = reinterpret_cast<const Complex&> (a_arg);
  return a.real () < 0.0;
}

static F77_INT
select_dig (const F77_DBLE_CMPLX& a_arg)
{
  const Complex& a = reinterpret_cast<const Complex&> (a_arg);
  return (abs (a) < 1.0);
}

static F77_INT
select_ana (const F77_CMPLX& a_arg)
{
  const FloatComplex& a = reinterpret_cast<const FloatComplex&> (a_arg);
  return a.real () < 0.0;
}

static F77_INT
select_dig (const F77_CMPLX& a_arg)
{
  const FloatComplex& a = reinterpret_cast<const FloatComplex&> (a_arg);
  return (abs (a) < 1.0);
}

template <>
OCTAVE_API F77_INT
schur<Matrix>::init (const Matrix& a, const std::string& ord,
                     bool calc_unitary)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("schur: requires square matrix");

  if (a_nr == 0)
    {
      m_schur_mat.clear ();
      m_unitary_schur_mat.clear ();
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

  char ord_char = (ord.empty () ? 'U' : ord[0]);

  if (ord_char == 'A' || ord_char == 'D'
      || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile double_selector selector = nullptr;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;

  F77_INT n = a_nc;
  F77_INT lwork = 8 * n;
  F77_INT liwork = 1;
  F77_INT info;
  F77_INT sdim;
  double rconde;
  double rcondv;

  m_schur_mat = a;

  if (calc_unitary)
    m_unitary_schur_mat.clear (n, n);

  double *s = m_schur_mat.fortran_vec ();
  double *q = m_unitary_schur_mat.fortran_vec ();

  Array<double> wr (dim_vector (n, 1));
  double *pwr = wr.fortran_vec ();

  Array<double> wi (dim_vector (n, 1));
  double *pwi = wi.fortran_vec ();

  Array<double> work (dim_vector (lwork, 1));
  double *pwork = work.fortran_vec ();

  // BWORK is not referenced for the non-ordered Schur routine.
  F77_INT ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<F77_INT> bwork (dim_vector (ntmp, 1));
  F77_INT *pbwork = bwork.fortran_vec ();

  Array<F77_INT> iwork (dim_vector (liwork, 1));
  F77_INT *piwork = iwork.fortran_vec ();

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
OCTAVE_API F77_INT
schur<FloatMatrix>::init (const FloatMatrix& a, const std::string& ord,
                          bool calc_unitary)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      m_schur_mat.clear ();
      m_unitary_schur_mat.clear ();
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

  char ord_char = (ord.empty () ? 'U' : ord[0]);

  if (ord_char == 'A' || ord_char == 'D'
      || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile float_selector selector = nullptr;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;

  F77_INT n = a_nc;
  F77_INT lwork = 8 * n;
  F77_INT liwork = 1;
  F77_INT info;
  F77_INT sdim;
  float rconde;
  float rcondv;

  m_schur_mat = a;

  if (calc_unitary)
    m_unitary_schur_mat.clear (n, n);

  float *s = m_schur_mat.fortran_vec ();
  float *q = m_unitary_schur_mat.fortran_vec ();

  Array<float> wr (dim_vector (n, 1));
  float *pwr = wr.fortran_vec ();

  Array<float> wi (dim_vector (n, 1));
  float *pwi = wi.fortran_vec ();

  Array<float> work (dim_vector (lwork, 1));
  float *pwork = work.fortran_vec ();

  // BWORK is not referenced for the non-ordered Schur routine.
  F77_INT ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<F77_INT> bwork (dim_vector (ntmp, 1));
  F77_INT *pbwork = bwork.fortran_vec ();

  Array<F77_INT> iwork (dim_vector (liwork, 1));
  F77_INT *piwork = iwork.fortran_vec ();

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
OCTAVE_API F77_INT
schur<ComplexMatrix>::init (const ComplexMatrix& a, const std::string& ord,
                            bool calc_unitary)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      m_schur_mat.clear ();
      m_unitary_schur_mat.clear ();
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

  char ord_char = (ord.empty () ? 'U' : ord[0]);

  if (ord_char == 'A' || ord_char == 'D'
      || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile complex_selector selector = nullptr;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;

  F77_INT n = a_nc;
  F77_INT lwork = 8 * n;
  F77_INT info;
  F77_INT sdim;
  double rconde;
  double rcondv;

  m_schur_mat = a;
  if (calc_unitary)
    m_unitary_schur_mat.clear (n, n);

  Complex *s = m_schur_mat.fortran_vec ();
  Complex *q = m_unitary_schur_mat.fortran_vec ();

  Array<double> rwork (dim_vector (n, 1));
  double *prwork = rwork.fortran_vec ();

  Array<Complex> w (dim_vector (n, 1));
  Complex *pw = w.fortran_vec ();

  Array<Complex> work (dim_vector (lwork, 1));
  Complex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  F77_INT ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<F77_INT> bwork (dim_vector (ntmp, 1));
  F77_INT *pbwork = bwork.fortran_vec ();

  F77_XFCN (zgeesx, ZGEESX,
            (F77_CONST_CHAR_ARG2 (&jobvs, 1),
             F77_CONST_CHAR_ARG2 (&sort, 1),
             selector,
             F77_CONST_CHAR_ARG2 (&sense, 1),
             n, F77_DBLE_CMPLX_ARG (s), n, sdim, F77_DBLE_CMPLX_ARG (pw),
             F77_DBLE_CMPLX_ARG (q), n, rconde, rcondv,
             F77_DBLE_CMPLX_ARG (pwork), lwork, prwork, pbwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
OCTAVE_API schur<ComplexMatrix>
rsf2csf<ComplexMatrix, Matrix> (const Matrix& s_arg, const Matrix& u_arg)
{
  ComplexMatrix s (s_arg);
  ComplexMatrix u (u_arg);

  F77_INT n = to_f77_int (s.rows ());

  if (s.columns () != n || u.rows () != n || u.columns () != n)
    (*current_liboctave_error_handler)
      ("rsf2csf: inconsistent matrix dimensions");

  if (n > 0)
    {
      OCTAVE_LOCAL_BUFFER (double, c, n-1);
      OCTAVE_LOCAL_BUFFER (double, sx, n-1);

      F77_XFCN (zrsf2csf, ZRSF2CSF,
                (n, F77_DBLE_CMPLX_ARG (s.fortran_vec ()),
                 F77_DBLE_CMPLX_ARG (u.fortran_vec ()), c, sx));
    }

  return schur<ComplexMatrix> (s, u);
}

template <>
OCTAVE_API F77_INT
schur<FloatComplexMatrix>::init (const FloatComplexMatrix& a,
                                 const std::string& ord, bool calc_unitary)
{
  F77_INT a_nr = to_f77_int (a.rows ());
  F77_INT a_nc = to_f77_int (a.cols ());

  if (a_nr != a_nc)
    (*current_liboctave_error_handler) ("SCHUR requires square matrix");

  if (a_nr == 0)
    {
      m_schur_mat.clear ();
      m_unitary_schur_mat.clear ();
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

  char ord_char = (ord.empty () ? 'U' : ord[0]);

  if (ord_char == 'A' || ord_char == 'D'
      || ord_char == 'a' || ord_char == 'd')
    sort = 'S';

  volatile float_complex_selector selector = nullptr;
  if (ord_char == 'A' || ord_char == 'a')
    selector = select_ana;
  else if (ord_char == 'D' || ord_char == 'd')
    selector = select_dig;

  F77_INT n = a_nc;
  F77_INT lwork = 8 * n;
  F77_INT info;
  F77_INT sdim;
  float rconde;
  float rcondv;

  m_schur_mat = a;
  if (calc_unitary)
    m_unitary_schur_mat.clear (n, n);

  FloatComplex *s = m_schur_mat.fortran_vec ();
  FloatComplex *q = m_unitary_schur_mat.fortran_vec ();

  Array<float> rwork (dim_vector (n, 1));
  float *prwork = rwork.fortran_vec ();

  Array<FloatComplex> w (dim_vector (n, 1));
  FloatComplex *pw = w.fortran_vec ();

  Array<FloatComplex> work (dim_vector (lwork, 1));
  FloatComplex *pwork = work.fortran_vec ();

  // BWORK is not referenced for non-ordered Schur.
  F77_INT ntmp = (ord_char == 'N' || ord_char == 'n') ? 0 : n;
  Array<F77_INT> bwork (dim_vector (ntmp, 1));
  F77_INT *pbwork = bwork.fortran_vec ();

  F77_XFCN (cgeesx, CGEESX,
            (F77_CONST_CHAR_ARG2 (&jobvs, 1),
             F77_CONST_CHAR_ARG2 (&sort, 1),
             selector,
             F77_CONST_CHAR_ARG2 (&sense, 1),
             n, F77_CMPLX_ARG (s), n, sdim, F77_CMPLX_ARG (pw),
             F77_CMPLX_ARG (q), n,
             rconde, rcondv,
             F77_CMPLX_ARG (pwork), lwork, prwork, pbwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)));

  return info;
}

template <>
OCTAVE_API schur<FloatComplexMatrix>
rsf2csf<FloatComplexMatrix, FloatMatrix> (const FloatMatrix& s_arg,
    const FloatMatrix& u_arg)
{
  FloatComplexMatrix s (s_arg);
  FloatComplexMatrix u (u_arg);

  F77_INT n = to_f77_int (s.rows ());

  if (s.columns () != n || u.rows () != n || u.columns () != n)
    (*current_liboctave_error_handler)
      ("rsf2csf: inconsistent matrix dimensions");

  if (n > 0)
    {
      OCTAVE_LOCAL_BUFFER (float, c, n-1);
      OCTAVE_LOCAL_BUFFER (float, sx, n-1);

      F77_XFCN (crsf2csf, CRSF2CSF,
                (n, F77_CMPLX_ARG (s.fortran_vec ()),
                 F77_CMPLX_ARG (u.fortran_vec ()), c, sx));
    }

  return schur<FloatComplexMatrix> (s, u);
}

// Instantiations we need.

template class schur<ComplexMatrix>;

template class schur<FloatComplexMatrix>;

template class schur<FloatMatrix>;

template class schur<Matrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
