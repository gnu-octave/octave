////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1997-2023 The Octave Project Developers
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <algorithm>
#include <unordered_map>

#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "gsvd.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"
#include "oct-shlib.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static std::unordered_map<std::string, void *> gsvd_fcn;

static bool have_DGGSVD3 = false;
static bool gsvd_initialized = false;

/* Hack to stringize results of F77_FUNC macro. */
#define xSTRINGIZE(x) #x
#define STRINGIZE(x) xSTRINGIZE(x)

static void initialize_gsvd (void)
{
  if (gsvd_initialized)
    return;

  dynamic_library libs ("");
  if (! libs)
    (*current_liboctave_error_handler)
      ("gsvd: unable to query LAPACK library");

  have_DGGSVD3 = (libs.search (STRINGIZE (F77_FUNC (dggsvd3, DGGSVD3)))
                  != nullptr);

  if (have_DGGSVD3)
    {
      gsvd_fcn["dg"] = libs.search (STRINGIZE (F77_FUNC (dggsvd3, DGGSVD3)));
      gsvd_fcn["sg"] = libs.search (STRINGIZE (F77_FUNC (sggsvd3, SGGSVD3)));
      gsvd_fcn["zg"] = libs.search (STRINGIZE (F77_FUNC (zggsvd3, ZGGSVD3)));
      gsvd_fcn["cg"] = libs.search (STRINGIZE (F77_FUNC (cggsvd3, CGGSVD3)));
    }
  else
    {
      gsvd_fcn["dg"] = libs.search (STRINGIZE (F77_FUNC (dggsvd, DGGSVD)));
      gsvd_fcn["sg"] = libs.search (STRINGIZE (F77_FUNC (sggsvd, SGGSVD)));
      gsvd_fcn["zg"] = libs.search (STRINGIZE (F77_FUNC (zggsvd, ZGGSVD)));
      gsvd_fcn["cg"] = libs.search (STRINGIZE (F77_FUNC (cggsvd, CGGSVD)));
    }

  gsvd_initialized = true;
}

/* Clean up macro namespace as soon as we are done using them */
#undef xSTRINGIZE
#undef STRINGIZE

template<class T1>
struct real_ggsvd_ptr
{
  typedef F77_RET_T (*type)
    (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   T1 *,                      // A(LDA,N)
   const F77_INT&,            // LDA
   T1 *,                      // B(LDB,N)
   const F77_INT&,            // LDB
   T1 *,                      // ALPHA(N)
   T1 *,                      // BETA(N)
   T1 *,                      // U(LDU,M)
   const F77_INT&,            // LDU
   T1 *,                      // V(LDV,P)
   const F77_INT&,            // LDV
   T1 *,                      // Q(LDQ,N)
   const F77_INT&,            // LDQ
   T1 *,                      // WORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);
};

template<class T1>
struct real_ggsvd3_ptr
{
  typedef F77_RET_T (*type)
    (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   T1 *,                      // A(LDA,N)
   const F77_INT&,            // LDA
   T1 *,                      // B(LDB,N)
   const F77_INT&,            // LDB
   T1 *,                      // ALPHA(N)
   T1 *,                      // BETA(N)
   T1 *,                      // U(LDU,M)
   const F77_INT&,            // LDU
   T1 *,                      // V(LDV,P)
   const F77_INT&,            // LDV
   T1 *,                      // Q(LDQ,N)
   const F77_INT&,            // LDQ
   T1 *,                      // WORK
   const F77_INT&,            // LWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);
};

template<class T1, class T2>
struct comp_ggsvd_ptr
{
  typedef F77_RET_T (*type)
    (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   T1 *,                      // A(LDA,N)
   const F77_INT&,            // LDA
   T1 *,                      // B(LDB,N)
   const F77_INT&,            // LDB
   T2 *,                      // ALPHA(N)
   T2 *,                      // BETA(N)
   T1 *,                      // U(LDU,M)
   const F77_INT&,            // LDU
   T1 *,                      // V(LDV,P)
   const F77_INT&,            // LDV
   T1 *,                      // Q(LDQ,N)
   const F77_INT&,            // LDQ
   T1 *,                      // WORK
   T2 *,                      // RWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);
};

template<class T1, class T2>
struct comp_ggsvd3_ptr
{
  typedef F77_RET_T (*type)
    (F77_CONST_CHAR_ARG_DECL,   // JOBU
   F77_CONST_CHAR_ARG_DECL,   // JOBV
   F77_CONST_CHAR_ARG_DECL,   // JOBQ
   const F77_INT&,            // M
   const F77_INT&,            // N
   const F77_INT&,            // P
   F77_INT&,                  // K
   F77_INT&,                  // L
   T1 *,                      // A(LDA,N)
   const F77_INT&,            // LDA
   T1 *,                      // B(LDB,N)
   const F77_INT&,            // LDB
   T2 *,                      // ALPHA(N)
   T2 *,                      // BETA(N)
   T1 *,                      // U(LDU,M)
   const F77_INT&,            // LDU
   T1 *,                      // V(LDV,P)
   const F77_INT&,            // LDV
   T1 *,                      // Q(LDQ,N)
   const F77_INT&,            // LDQ
   T1 *,                      // WORK
   const F77_INT&,            // LWORK
   T2 *,                      // RWORK
   F77_INT *,                 // IWORK(N)
   F77_INT&                   // INFO
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL
   F77_CHAR_ARG_LEN_DECL);
};

// template specializations
typedef real_ggsvd_ptr<F77_DBLE>::type dggsvd_type;
typedef real_ggsvd3_ptr<F77_DBLE>::type dggsvd3_type;
typedef real_ggsvd_ptr<F77_REAL>::type sggsvd_type;
typedef real_ggsvd3_ptr<F77_REAL>::type sggsvd3_type;
typedef comp_ggsvd_ptr<F77_DBLE_CMPLX, F77_DBLE>::type zggsvd_type;
typedef comp_ggsvd3_ptr<F77_DBLE_CMPLX, F77_DBLE>::type zggsvd3_type;
typedef comp_ggsvd_ptr<F77_CMPLX, F77_REAL>::type cggsvd_type;
typedef comp_ggsvd3_ptr<F77_CMPLX, F77_REAL>::type cggsvd3_type;

OCTAVE_BEGIN_NAMESPACE(math)

template <>
void
gsvd<Matrix>::ggsvd (char& jobu, char& jobv, char& jobq, F77_INT m,
                     F77_INT n, F77_INT p, F77_INT& k, F77_INT& l,
                     double *tmp_dataA, F77_INT m1, double *tmp_dataB,
                     F77_INT p1, Matrix& alpha, Matrix& beta, double *u,
                     F77_INT nrow_u, double *v, F77_INT nrow_v, double *q,
                     F77_INT nrow_q, double *work, F77_INT lwork,
                     F77_INT *iwork, F77_INT& info)
{
  if (! gsvd_initialized)
    initialize_gsvd ();

  if (have_DGGSVD3)
    {
      dggsvd3_type f_ptr = reinterpret_cast<dggsvd3_type> (gsvd_fcn["dg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l, tmp_dataA, m1, tmp_dataB, p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             u, nrow_u, v, nrow_v, q, nrow_q,
             work, lwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
  else
    {
      dggsvd_type f_ptr = reinterpret_cast<dggsvd_type> (gsvd_fcn["dg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l, tmp_dataA, m1, tmp_dataB, p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             u, nrow_u, v, nrow_v, q, nrow_q,
             work, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
}

template <>
void
gsvd<FloatMatrix>::ggsvd (char& jobu, char& jobv, char& jobq, F77_INT m,
                          F77_INT n, F77_INT p, F77_INT& k, F77_INT& l,
                          float *tmp_dataA, F77_INT m1, float *tmp_dataB,
                          F77_INT p1, FloatMatrix& alpha,
                          FloatMatrix& beta, float *u, F77_INT nrow_u,
                          float *v, F77_INT nrow_v, float *q,
                          F77_INT nrow_q, float *work,
                          F77_INT lwork, F77_INT *iwork, F77_INT& info)
{
  if (! gsvd_initialized)
    initialize_gsvd ();

  if (have_DGGSVD3)
    {
      sggsvd3_type f_ptr = reinterpret_cast<sggsvd3_type> (gsvd_fcn["sg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l, tmp_dataA, m1, tmp_dataB, p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             u, nrow_u, v, nrow_v, q, nrow_q,
             work, lwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
  else
    {
      sggsvd_type f_ptr = reinterpret_cast<sggsvd_type> (gsvd_fcn["sg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l, tmp_dataA, m1, tmp_dataB, p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             u, nrow_u, v, nrow_v, q, nrow_q,
             work, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
}

template <>
void
gsvd<ComplexMatrix>::ggsvd (char& jobu, char& jobv, char& jobq,
                            F77_INT m, F77_INT n, F77_INT p, F77_INT& k,
                            F77_INT& l, Complex *tmp_dataA, F77_INT m1,
                            Complex *tmp_dataB, F77_INT p1, Matrix& alpha,
                            Matrix& beta, Complex *u, F77_INT nrow_u,
                            Complex *v, F77_INT nrow_v, Complex *q,
                            F77_INT nrow_q, Complex *work,
                            F77_INT lwork, F77_INT *iwork, F77_INT& info)
{
  if (! gsvd_initialized)
    initialize_gsvd ();

  OCTAVE_LOCAL_BUFFER(double, rwork, 2*n);

  if (have_DGGSVD3)
    {
      zggsvd3_type f_ptr = reinterpret_cast<zggsvd3_type> (gsvd_fcn["zg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l,
             F77_DBLE_CMPLX_ARG (tmp_dataA), m1,
             F77_DBLE_CMPLX_ARG (tmp_dataB), p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             F77_DBLE_CMPLX_ARG (u), nrow_u,
             F77_DBLE_CMPLX_ARG (v), nrow_v,
             F77_DBLE_CMPLX_ARG (q), nrow_q,
             F77_DBLE_CMPLX_ARG (work),
             lwork, rwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
  else
    {
      zggsvd_type f_ptr = reinterpret_cast<zggsvd_type> (gsvd_fcn["zg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l,
             F77_DBLE_CMPLX_ARG (tmp_dataA), m1,
             F77_DBLE_CMPLX_ARG (tmp_dataB), p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             F77_DBLE_CMPLX_ARG (u), nrow_u,
             F77_DBLE_CMPLX_ARG (v), nrow_v,
             F77_DBLE_CMPLX_ARG (q), nrow_q,
             F77_DBLE_CMPLX_ARG (work),
             rwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
}

template <>
void
gsvd<FloatComplexMatrix>::ggsvd (char& jobu, char& jobv, char& jobq,
                                 F77_INT m, F77_INT n, F77_INT p,
                                 F77_INT& k, F77_INT& l,
                                 FloatComplex *tmp_dataA, F77_INT m1,
                                 FloatComplex *tmp_dataB, F77_INT p1,
                                 FloatMatrix& alpha, FloatMatrix& beta,
                                 FloatComplex *u, F77_INT nrow_u,
                                 FloatComplex *v, F77_INT nrow_v,
                                 FloatComplex *q, F77_INT nrow_q,
                                 FloatComplex *work, F77_INT lwork,
                                 F77_INT *iwork, F77_INT& info)
{
  if (! gsvd_initialized)
    initialize_gsvd ();

  OCTAVE_LOCAL_BUFFER(float, rwork, 2*n);

  if (have_DGGSVD3)
    {
      cggsvd3_type f_ptr = reinterpret_cast<cggsvd3_type> (gsvd_fcn["cg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l,
             F77_CMPLX_ARG (tmp_dataA), m1,
             F77_CMPLX_ARG (tmp_dataB), p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             F77_CMPLX_ARG (u), nrow_u,
             F77_CMPLX_ARG (v), nrow_v,
             F77_CMPLX_ARG (q), nrow_q,
             F77_CMPLX_ARG (work), lwork,
             rwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
  else
    {
      cggsvd_type f_ptr = reinterpret_cast<cggsvd_type> (gsvd_fcn["cg"]);
      f_ptr (F77_CONST_CHAR_ARG2 (&jobu, 1),
             F77_CONST_CHAR_ARG2 (&jobv, 1),
             F77_CONST_CHAR_ARG2 (&jobq, 1),
             m, n, p, k, l,
             F77_CMPLX_ARG (tmp_dataA), m1,
             F77_CMPLX_ARG (tmp_dataB), p1,
             alpha.fortran_vec (), beta.fortran_vec (),
             F77_CMPLX_ARG (u), nrow_u,
             F77_CMPLX_ARG (v), nrow_v,
             F77_CMPLX_ARG (q), nrow_q,
             F77_CMPLX_ARG (work),
             rwork, iwork, info
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1)
             F77_CHAR_ARG_LEN (1));
    }
}

template <typename T>
T
gsvd<T>::left_singular_matrix_A (void) const
{
  if (m_type == gsvd::Type::sigma_only)
    (*current_liboctave_error_handler)
      ("gsvd: U not computed because type == gsvd::sigma_only");

  return m_left_smA;
}

template <typename T>
T
gsvd<T>::left_singular_matrix_B (void) const
{
  if (m_type == gsvd::Type::sigma_only)
    (*current_liboctave_error_handler)
      ("gsvd: V not computed because type == gsvd::sigma_only");

  return m_left_smB;
}

template <typename T>
T
gsvd<T>::right_singular_matrix (void) const
{
  if (m_type == gsvd::Type::sigma_only)
    (*current_liboctave_error_handler)
      ("gsvd: X not computed because type == gsvd::sigma_only");

  return m_right_sm;
}

template <typename T>
gsvd<T>::gsvd (const T& a, const T& b, gsvd::Type gsvd_type)
{
  if (a.isempty () || b.isempty ())
    (*current_liboctave_error_handler)
      ("gsvd: A and B cannot be empty matrices");

  F77_INT info;

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());
  F77_INT p = to_f77_int (b.rows ());

  T atmp = a;
  P *tmp_dataA = atmp.fortran_vec ();

  T btmp = b;
  P *tmp_dataB = btmp.fortran_vec ();

  char jobu = 'U';
  char jobv = 'V';
  char jobq = 'Q';

  F77_INT nrow_u = m;
  F77_INT nrow_v = p;
  F77_INT nrow_q = n;

  F77_INT k, l;

  switch (gsvd_type)
    {
    case gsvd<T>::Type::sigma_only:

      // FIXME: In LAPACK 3.0, problem below seems to be fixed,
      //        so we now set jobX = 'N'.
      //
      // For calculating sigma_only, both jobu and jobv should be 'N', but
      // there seems to be a bug in dgesvd from LAPACK V2.0.  To
      // demonstrate the bug, set both jobu and jobv to 'N' and find the
      // singular values of [eye(3), eye(3)].  The result is
      // [-sqrt(2), -sqrt(2), -sqrt(2)].

      jobu = jobv = jobq = 'N';
      nrow_u = nrow_v = nrow_q = 1;
      break;

    default:
      break;
    }

  m_type = gsvd_type;

  if (jobu != 'N')
    m_left_smA.resize (nrow_u, m);

  P *u = m_left_smA.fortran_vec ();

  if (jobv != 'N')
    m_left_smB.resize (nrow_v, p);

  P *v = m_left_smB.fortran_vec ();

  if (jobq != 'N')
    m_right_sm.resize (nrow_q, n);

  P *q = m_right_sm.fortran_vec ();

  real_matrix alpha (n, 1);
  real_matrix beta (n, 1);

  OCTAVE_LOCAL_BUFFER(F77_INT, iwork, n);

  if (! gsvd_initialized)
    initialize_gsvd ();

  F77_INT lwork;
  if (have_DGGSVD3)
    {
      lwork = -1;
      P work_tmp;

      gsvd<T>::ggsvd (jobu, jobv, jobq, m, n, p, k, l,
                      tmp_dataA, m, tmp_dataB, p,
                      alpha, beta, u, nrow_u, v, nrow_v, q, nrow_q,
                      &work_tmp, lwork, iwork, info);

      lwork = static_cast<F77_INT> (std::abs (work_tmp));
    }
  else
    {
      lwork = std::max ({3*n, m, p}) + n;
    }
  info = 0;

  OCTAVE_LOCAL_BUFFER(P, work, lwork);

  gsvd<T>::ggsvd (jobu, jobv, jobq, m, n, p, k, l,
                  tmp_dataA, m, tmp_dataB, p,
                  alpha, beta, u, nrow_u, v, nrow_v, q, nrow_q,
                  work, lwork, iwork, info);

  if (info < 0)
    (*current_liboctave_error_handler)
      ("*ggsvd.f: argument %" OCTAVE_F77_INT_TYPE_FORMAT " illegal",
       -info);

  if (info > 0)
    (*current_liboctave_error_handler)
      ("*ggsvd.f: Jacobi-type procedure failed to converge");

  F77_INT i, j;

  if (gsvd_type != gsvd<T>::Type::sigma_only)
    {
      // Size according to LAPACK is k+l,k+l, but this needs
      // to be nxn for Matlab compatibility.
      T R (n, n, 0.0);
      int astart = n-k-l;
      if (m - k - l >= 0)
        {
          // R is stored in A(1:K+L,N-K-L+1:N)
          for (i = 0; i < k+l; i++)
            for (j = 0; j < k+l; j++)
              R.xelem (i, j) = atmp.xelem (i, astart + j);
        }
      else
        {
          // (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N)
          // ( 0  R22 R23 )
          for (i = 0; i < m; i++)
            for (j = 0; j < k+l; j++)
              R.xelem (i, j) = atmp.xelem (i, astart + j);
          // and R33 is stored in B(M-K+1:L,N+M-K-L+1:N)
          for (i = m; i < k + l; i++)
            for (j = n - l - k + m; j < n; j++)
              R.xelem (i, j) = btmp.xelem (i - k, j);
        }

      // Output X = Q*R'
      // FIXME: Is there a way to call BLAS multiply directly
      //        with flags so that R is transposed?
      m_right_sm = m_right_sm * R.hermitian ();
    }

  // Fill in C and S
  F77_INT rank;
  bool fill_ptn;
  if (m-k-l >= 0)
    {
      rank = l;
      fill_ptn = true;
    }
  else
    {
      rank = m-k;
      fill_ptn = false;
    }

  if (gsvd_type == gsvd<T>::Type::sigma_only)
    {
      // Return column vector with results
      m_sigmaA.resize (k+l, 1);
      m_sigmaB.resize (k+l, 1);

      if (fill_ptn)
        {
          for (i = 0; i < k; i++)
            {
              m_sigmaA.xelem (i) = 1.0;
              m_sigmaB.xelem (i) = 0.0;
            }
          for (i = k, j = k+l-1; i < k+l; i++, j--)
            {
              m_sigmaA.xelem (i) = alpha.xelem (i);
              m_sigmaB.xelem (i) = beta.xelem (i);
            }
        }
      else
        {
          for (i = 0; i < k; i++)
            {
              m_sigmaA.xelem (i) = 1.0;
              m_sigmaB.xelem (i) = 0.0;
            }
          for (i = k; i < m; i++)
            {
              m_sigmaA.xelem (i) = alpha.xelem (i);
              m_sigmaB.xelem (i) = beta.xelem (i);
            }
          for (i = m; i < k+l; i++)
            {
              m_sigmaA.xelem (i) = 0.0;
              m_sigmaB.xelem (i) = 1.0;
            }
        }
    }
  else  // returning all matrices
    {
      // Number of columns according to LAPACK is k+l, but this needs
      // to be n for Matlab compatibility.
      m_sigmaA.resize (m, n);
      m_sigmaB.resize (p, n);

      for (i = 0; i < k; i++)
        m_sigmaA.xelem (i, i) = 1.0;

      for (i = 0; i < rank; i++)
        {
          m_sigmaA.xelem (k+i, k+i) = alpha.xelem (k+i);
          m_sigmaB.xelem (i, k+i) = beta.xelem (k+i);
        }

      if (! fill_ptn)
        {
          for (i = m; i < n; i++)
            m_sigmaB.xelem (i-k, i) = 1.0;
        }

    }
}

// Instantiations needed in octave::math namespace.
template class gsvd<Matrix>;
template class gsvd<FloatMatrix>;
template class gsvd<ComplexMatrix>;
template class gsvd<FloatComplexMatrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
