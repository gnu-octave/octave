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

#include <cassert>

#include <algorithm>
#include <unordered_map>

#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "svd.h"

// class to compute optimal work space size (lwork) for DGEJSV and SGEJSV
template<typename T>
class
gejsv_lwork
{
public:
  gejsv_lwork () = delete;

  // Unfortunately, dgejsv and sgejsv do not provide estimation of 'lwork'.
  // Thus, we have to estimate it according to corresponding LAPACK
  // documentation and related source codes (e.g. cgejsv).
  // In LAPACKE (C interface to LAPACK), the memory handling code in
  // LAPACKE_dgejsv() (lapacke_dgejsv.c, last visit 2019-02-17) uses
  // the minimum required working space.  In contrast, here the optimal
  // working space size is computed, at the cost of much longer code.

  static F77_INT optimal (char& joba, char& jobu, char& jobv,
                          F77_INT m, F77_INT n);

private:
  typedef typename T::element_type P;

  // functions could be called from GEJSV
  static F77_INT geqp3_lwork (F77_INT m, F77_INT n,
                              P *a, F77_INT lda,
                              F77_INT *jpvt, P *tau, P *work,
                              F77_INT lwork, F77_INT& info);

  static F77_INT geqrf_lwork (F77_INT m, F77_INT n,
                              P *a, F77_INT lda,
                              P *tau, P *work,
                              F77_INT lwork, F77_INT& info);

  static F77_INT gelqf_lwork (F77_INT m, F77_INT n,
                              P *a, F77_INT lda,
                              P *tau, P *work,
                              F77_INT lwork, F77_INT& info);

  static F77_INT ormlq_lwork (char& side, char& trans,
                              F77_INT m, F77_INT n, F77_INT k,
                              P *a, F77_INT lda,
                              P *tau, P *c, F77_INT ldc,
                              P *work, F77_INT lwork, F77_INT& info);

  static F77_INT ormqr_lwork (char& side, char& trans,
                              F77_INT m, F77_INT n, F77_INT k,
                              P *a, F77_INT lda,
                              P *tau, P *c, F77_INT ldc,
                              P *work, F77_INT lwork, F77_INT& info);
};

#define GEJSV_REAL_QP3_LWORK(f, F)                              \
  F77_XFCN (f, F, (m, n, a, lda, jpvt, tau, work, lwork, info))

#define GEJSV_REAL_QR_LWORK(f, F)                               \
  F77_XFCN (f, F, (m, n, a, lda, tau, work, lwork, info))

#define GEJSV_REAL_ORM_LWORK(f, F)                              \
  F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&side,  1),             \
                   F77_CONST_CHAR_ARG2 (&trans, 1),             \
                   m, n, k, a, lda, tau,                        \
                   c, ldc, work, lwork, info                    \
                   F77_CHAR_ARG_LEN (1)                         \
                   F77_CHAR_ARG_LEN (1)))

// For Matrix
template<>
F77_INT
gejsv_lwork<Matrix>::geqp3_lwork (F77_INT m, F77_INT n,
                                  P *a, F77_INT lda,
                                  F77_INT *jpvt, P *tau, P *work,
                                  F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QP3_LWORK (dgeqp3, DGEQP3);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<Matrix>::geqrf_lwork (F77_INT m, F77_INT n,
                                  P *a, F77_INT lda,
                                  P *tau, P *work,
                                  F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QR_LWORK (dgeqrf, DGEQRF);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<Matrix>::gelqf_lwork (F77_INT m, F77_INT n,
                                  P *a, F77_INT lda,
                                  P *tau, P *work,
                                  F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QR_LWORK (dgelqf, DGELQF);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<Matrix>::ormlq_lwork (char& side, char& trans,
                                  F77_INT m, F77_INT n, F77_INT k,
                                  P *a, F77_INT lda,
                                  P *tau, P *c, F77_INT ldc,
                                  P *work, F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_ORM_LWORK (dormlq, DORMLQ);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<Matrix>::ormqr_lwork (char& side, char& trans,
                                  F77_INT m, F77_INT n, F77_INT k,
                                  P *a, F77_INT lda,
                                  P *tau, P *c, F77_INT ldc,
                                  P *work, F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_ORM_LWORK (dormqr, DORMQR);
  return static_cast<F77_INT> (work[0]);
}

// For FloatMatrix
template<>
F77_INT
gejsv_lwork<FloatMatrix>::geqp3_lwork (F77_INT m, F77_INT n,
                                       P *a, F77_INT lda,
                                       F77_INT *jpvt, P *tau, P *work,
                                       F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QP3_LWORK (sgeqp3, SGEQP3);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<FloatMatrix>::geqrf_lwork (F77_INT m, F77_INT n,
                                       P *a, F77_INT lda,
                                       P *tau, P *work,
                                       F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QR_LWORK (sgeqrf, SGEQRF);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<FloatMatrix>::gelqf_lwork (F77_INT m, F77_INT n,
                                       P *a, F77_INT lda,
                                       P *tau, P *work,
                                       F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_QR_LWORK (sgelqf, SGELQF);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<FloatMatrix>::ormlq_lwork (char& side, char& trans,
                                       F77_INT m, F77_INT n, F77_INT k,
                                       P *a, F77_INT lda,
                                       P *tau, P *c, F77_INT ldc,
                                       P *work, F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_ORM_LWORK (sormlq, SORMLQ);
  return static_cast<F77_INT> (work[0]);
}

template<>
F77_INT
gejsv_lwork<FloatMatrix>::ormqr_lwork (char& side, char& trans,
                                       F77_INT m, F77_INT n, F77_INT k,
                                       P *a, F77_INT lda,
                                       P *tau, P *c, F77_INT ldc,
                                       P *work, F77_INT lwork, F77_INT& info)
{
  GEJSV_REAL_ORM_LWORK (sormqr, SORMQR);
  return static_cast<F77_INT> (work[0]);
}

#undef GEJSV_REAL_QP3_LWORK
#undef GEJSV_REAL_QR_LWORK
#undef GEJSV_REAL_ORM_LWORK

template<typename T>
F77_INT
gejsv_lwork<T>::optimal (char& joba, char& jobu, char& jobv,
                         F77_INT m, F77_INT n)
{
  F77_INT lwork = -1;
  std::vector<P> work (2);  // dummy work space

  // variables that mimic running environment of gejsv
  F77_INT lda  = std::max<F77_INT> (m, 1);
  F77_INT ierr = 0;
  char side  = 'L';
  char trans = 'N';
  std::vector<P> mat_a (1);
  P *a = mat_a.data ();    // dummy input matrix
  std::vector<F77_INT> vec_jpvt = {0};
  P *tau = work.data ();
  P *u   = work.data ();
  P *v   = work.data ();

  bool need_lsvec = jobu == 'U' || jobu == 'F';
  bool need_rsvec = jobv == 'V' || jobv == 'J';

  F77_INT lw_pocon = 3 * n;  // for [s,d]pocon
  F77_INT lw_geqp3 = geqp3_lwork (m, n, a, lda, vec_jpvt.data (),
                                  tau, work.data (), -1, ierr);
  F77_INT lw_geqrf = geqrf_lwork (m, n, a, lda,
                                  tau, work.data (), -1, ierr);

  if (! (need_lsvec || need_rsvec) )
    {
      // only SIGMA is needed
      if (! (joba == 'E' || joba == 'G') )
        lwork = std::max<F77_INT> ({2*m + n, n + lw_geqp3, n + lw_geqrf, 7});
      else
        lwork = std::max<F77_INT> ({2*m + n, n + lw_geqp3, n + lw_geqrf,
                                    n + n*n + lw_pocon, 7});
    }
  else if (need_rsvec && ! need_lsvec)
    {
      // SIGMA and the right singular vectors are needed
      F77_INT lw_gelqf = gelqf_lwork (n, n, a, lda,
                                      tau, work.data (), -1, ierr);
      trans = 'T';
      F77_INT lw_ormlq = ormlq_lwork (side, trans, n, n, n, a, lda,
                                      tau, v, n, work.data (), -1, ierr);
      lwork = std::max<F77_INT> ({2*m + n, n + lw_geqp3, n + lw_pocon,
                                  n + lw_gelqf, 2*n + lw_geqrf, n + lw_ormlq});
    }
  else if (need_lsvec && ! need_rsvec)
    {
      // SIGMA and the left singular vectors are needed
      F77_INT n1 = (jobu == 'U') ? n : m;  // size of U is m x n1
      F77_INT lw_ormqr = ormqr_lwork (side, trans, m, n1, n, a, lda,
                                      tau, u, m, work.data (), -1, ierr);
      lwork = std::max<F77_INT> ({2*m + n, n + lw_geqp3, n + lw_pocon,
                                  2*n + lw_geqrf, n + lw_ormqr});
    }
  else  // full SVD is needed
    {
      if (jobv == 'V')
        lwork = std::max (2*m + n, 6*n + 2*n*n);
      else if (jobv == 'J')
        lwork = std::max<F77_INT> ({2*m + n, 4*n + n*n, 2*n + n*n + 6});

      F77_INT n1 = (jobu == 'U') ? n : m;  // size of U is m x n1
      F77_INT lw_ormqr = ormqr_lwork (side, trans, m, n1, n, a, lda,
                                      tau, u, m, work.data (), -1, ierr);
      lwork = std::max (lwork, n + lw_ormqr);
    }

  return lwork;
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename T>
T
svd<T>::left_singular_matrix (void) const
{
  if (m_type == svd::Type::sigma_only)
    (*current_liboctave_error_handler)
      ("svd: U not computed because type == svd::sigma_only");

  return m_left_sm;
}

template <typename T>
T
svd<T>::right_singular_matrix (void) const
{
  if (m_type == svd::Type::sigma_only)
    (*current_liboctave_error_handler)
      ("svd: V not computed because type == svd::sigma_only");

  return m_right_sm;
}

// GESVD specializations

#define GESVD_REAL_STEP(f, F)                                   \
  F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobu, 1),              \
                   F77_CONST_CHAR_ARG2 (&jobv, 1),              \
                   m, n, tmp_data, m1, s_vec, u, m1, vt,        \
                   nrow_vt1, work.data (), lwork, info          \
                   F77_CHAR_ARG_LEN (1)                         \
                   F77_CHAR_ARG_LEN (1)))

#define GESVD_COMPLEX_STEP(f, F, CMPLX_ARG)             \
  F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobu, 1),      \
                   F77_CONST_CHAR_ARG2 (&jobv, 1),      \
                   m, n, CMPLX_ARG (tmp_data),          \
                   m1, s_vec, CMPLX_ARG (u), m1,        \
                   CMPLX_ARG (vt), nrow_vt1,            \
                   CMPLX_ARG (work.data ()),            \
                   lwork, rwork.data (), info           \
                   F77_CHAR_ARG_LEN (1)                 \
                   F77_CHAR_ARG_LEN (1)))

// DGESVD
template<>
OCTAVE_API void
svd<Matrix>::gesvd (char& jobu, char& jobv, F77_INT m, F77_INT n,
                    double *tmp_data, F77_INT m1, double *s_vec,
                    double *u, double *vt, F77_INT nrow_vt1,
                    std::vector<double>& work, F77_INT& lwork,
                    F77_INT& info)
{
  GESVD_REAL_STEP (dgesvd, DGESVD);

  lwork = static_cast<F77_INT> (work[0]);
  work.reserve (lwork);

  GESVD_REAL_STEP (dgesvd, DGESVD);
}

// SGESVD
template<>
OCTAVE_API void
svd<FloatMatrix>::gesvd (char& jobu, char& jobv, F77_INT m, F77_INT n,
                         float *tmp_data, F77_INT m1, float *s_vec,
                         float *u, float *vt, F77_INT nrow_vt1,
                         std::vector<float>& work, F77_INT& lwork,
                         F77_INT& info)
{
  GESVD_REAL_STEP (sgesvd, SGESVD);

  lwork = static_cast<F77_INT> (work[0]);
  work.reserve (lwork);

  GESVD_REAL_STEP (sgesvd, SGESVD);
}

// ZGESVD
template<>
OCTAVE_API void
svd<ComplexMatrix>::gesvd (char& jobu, char& jobv, F77_INT m, F77_INT n,
                           Complex *tmp_data, F77_INT m1, double *s_vec,
                           Complex *u, Complex *vt, F77_INT nrow_vt1,
                           std::vector<Complex>& work, F77_INT& lwork,
                           F77_INT& info)
{
  std::vector<double> rwork (5 * std::max (m, n));

  GESVD_COMPLEX_STEP (zgesvd, ZGESVD, F77_DBLE_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  GESVD_COMPLEX_STEP (zgesvd, ZGESVD, F77_DBLE_CMPLX_ARG);
}

// CGESVD
template<>
OCTAVE_API void
svd<FloatComplexMatrix>::gesvd (char& jobu, char& jobv, F77_INT m,
                                F77_INT n, FloatComplex *tmp_data,
                                F77_INT m1, float *s_vec, FloatComplex *u,
                                FloatComplex *vt, F77_INT nrow_vt1,
                                std::vector<FloatComplex>& work,
                                F77_INT& lwork, F77_INT& info)
{
  std::vector<float> rwork (5 * std::max (m, n));

  GESVD_COMPLEX_STEP (cgesvd, CGESVD, F77_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  GESVD_COMPLEX_STEP (cgesvd, CGESVD, F77_CMPLX_ARG);
}

#undef GESVD_REAL_STEP
#undef GESVD_COMPLEX_STEP

// GESDD specializations

#define GESDD_REAL_STEP(f, F)                                           \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobz, 1),                    \
                     m, n, tmp_data, m1, s_vec, u, m1, vt, nrow_vt1,    \
                     work.data (), lwork, iwork, info                   \
                     F77_CHAR_ARG_LEN (1)))

#define GESDD_COMPLEX_STEP(f, F, CMPLX_ARG)                     \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobz, 1), m, n,      \
                     CMPLX_ARG (tmp_data), m1,                  \
                     s_vec, CMPLX_ARG (u), m1,                  \
                     CMPLX_ARG (vt), nrow_vt1,                  \
                     CMPLX_ARG (work.data ()), lwork,           \
                     rwork.data (), iwork, info                 \
                     F77_CHAR_ARG_LEN (1)))

// DGESDD
template<>
OCTAVE_API void
svd<Matrix>::gesdd (char& jobz, F77_INT m, F77_INT n, double *tmp_data,
                    F77_INT m1, double *s_vec, double *u, double *vt,
                    F77_INT nrow_vt1, std::vector<double>& work,
                    F77_INT& lwork, F77_INT *iwork, F77_INT& info)
{
  GESDD_REAL_STEP (dgesdd, DGESDD);

  lwork = static_cast<F77_INT> (work[0]);
  work.reserve (lwork);

  GESDD_REAL_STEP (dgesdd, DGESDD);
}

// SGESDD
template<>
OCTAVE_API void
svd<FloatMatrix>::gesdd (char& jobz, F77_INT m, F77_INT n, float *tmp_data,
                         F77_INT m1, float *s_vec, float *u, float *vt,
                         F77_INT nrow_vt1, std::vector<float>& work,
                         F77_INT& lwork, F77_INT *iwork, F77_INT& info)
{
  GESDD_REAL_STEP (sgesdd, SGESDD);

  lwork = static_cast<F77_INT> (work[0]);
  work.reserve (lwork);

  GESDD_REAL_STEP (sgesdd, SGESDD);
}

// ZGESDD
template<>
OCTAVE_API void
svd<ComplexMatrix>::gesdd (char& jobz, F77_INT m, F77_INT n,
                           Complex *tmp_data, F77_INT m1, double *s_vec,
                           Complex *u, Complex *vt, F77_INT nrow_vt1,
                           std::vector<Complex>& work, F77_INT& lwork,
                           F77_INT *iwork, F77_INT& info)
{

  F77_INT min_mn = std::min (m, n);
  F77_INT max_mn = std::max (m, n);

  F77_INT lrwork;
  if (jobz == 'N')
    lrwork = 7*min_mn;
  else
    lrwork = min_mn * std::max (5*min_mn+5, 2*max_mn+2*min_mn+1);

  std::vector<double> rwork (lrwork);

  GESDD_COMPLEX_STEP (zgesdd, ZGESDD, F77_DBLE_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  GESDD_COMPLEX_STEP (zgesdd, ZGESDD, F77_DBLE_CMPLX_ARG);
}

// CGESDD
template<>
OCTAVE_API void
svd<FloatComplexMatrix>::gesdd (char& jobz, F77_INT m, F77_INT n,
                                FloatComplex *tmp_data, F77_INT m1,
                                float *s_vec, FloatComplex *u,
                                FloatComplex *vt, F77_INT nrow_vt1,
                                std::vector<FloatComplex>& work,
                                F77_INT& lwork, F77_INT *iwork,
                                F77_INT& info)
{
  F77_INT min_mn = std::min (m, n);
  F77_INT max_mn = std::max (m, n);

  F77_INT lrwork;
  if (jobz == 'N')
    lrwork = 7*min_mn;
  else
    lrwork = min_mn * std::max (5*min_mn+5, 2*max_mn+2*min_mn+1);
  std::vector<float> rwork (lrwork);

  GESDD_COMPLEX_STEP (cgesdd, CGESDD, F77_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  GESDD_COMPLEX_STEP (cgesdd, CGESDD, F77_CMPLX_ARG);
}

#undef GESDD_REAL_STEP
#undef GESDD_COMPLEX_STEP

// GEJSV specializations

#define GEJSV_REAL_STEP(f, F)                                         \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&joba, 1),                  \
                     F77_CONST_CHAR_ARG2 (&jobu, 1),                  \
                     F77_CONST_CHAR_ARG2 (&jobv, 1),                  \
                     F77_CONST_CHAR_ARG2 (&jobr, 1),                  \
                     F77_CONST_CHAR_ARG2 (&jobt, 1),                  \
                     F77_CONST_CHAR_ARG2 (&jobp, 1),                  \
                     m, n, tmp_data, m1, s_vec, u, m1, v, nrow_v1,    \
                     work.data (), lwork, iwork.data (), info         \
                     F77_CHAR_ARG_LEN (1)                             \
                     F77_CHAR_ARG_LEN (1)                             \
                     F77_CHAR_ARG_LEN (1)                             \
                     F77_CHAR_ARG_LEN (1)                             \
                     F77_CHAR_ARG_LEN (1)                             \
                     F77_CHAR_ARG_LEN (1)))

#define GEJSV_COMPLEX_STEP(f, F, CMPLX_ARG)                       \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&joba, 1),              \
                     F77_CONST_CHAR_ARG2 (&jobu, 1),              \
                     F77_CONST_CHAR_ARG2 (&jobv, 1),              \
                     F77_CONST_CHAR_ARG2 (&jobr, 1),              \
                     F77_CONST_CHAR_ARG2 (&jobt, 1),              \
                     F77_CONST_CHAR_ARG2 (&jobp, 1),              \
                     m, n, CMPLX_ARG (tmp_data), m1,              \
                     s_vec, CMPLX_ARG (u), m1,                    \
                     CMPLX_ARG (v), nrow_v1,                      \
                     CMPLX_ARG (work.data ()), lwork,             \
                     rwork.data (), lrwork, iwork.data (), info   \
                     F77_CHAR_ARG_LEN (1)                         \
                     F77_CHAR_ARG_LEN (1)                         \
                     F77_CHAR_ARG_LEN (1)                         \
                     F77_CHAR_ARG_LEN (1)                         \
                     F77_CHAR_ARG_LEN (1)                         \
                     F77_CHAR_ARG_LEN (1)))

// DGEJSV
template<>
void
svd<Matrix>::gejsv (char& joba, char& jobu, char& jobv,
                    char& jobr, char& jobt, char& jobp,
                    F77_INT m, F77_INT n,
                    P *tmp_data, F77_INT m1, DM_P *s_vec, P *u,
                    P *v, F77_INT nrow_v1, std::vector<P>& work,
                    F77_INT& lwork, std::vector<F77_INT>& iwork,
                    F77_INT& info)
{
  lwork = gejsv_lwork<Matrix>::optimal (joba, jobu, jobv, m, n);
  work.reserve (lwork);

  GEJSV_REAL_STEP (dgejsv, DGEJSV);
}

// SGEJSV
template<>
void
svd<FloatMatrix>::gejsv (char& joba, char& jobu, char& jobv,
                         char& jobr, char& jobt, char& jobp,
                         F77_INT m, F77_INT n,
                         P *tmp_data, F77_INT m1, DM_P *s_vec, P *u,
                         P *v, F77_INT nrow_v1, std::vector<P>& work,
                         F77_INT& lwork, std::vector<F77_INT>& iwork,
                         F77_INT& info)
{
  lwork = gejsv_lwork<FloatMatrix>::optimal (joba, jobu, jobv, m, n);
  work.reserve (lwork);

  GEJSV_REAL_STEP (sgejsv, SGEJSV);
}

// ZGEJSV
template<>
void
svd<ComplexMatrix>::gejsv (char& joba, char& jobu, char& jobv,
                           char& jobr, char& jobt, char& jobp,
                           F77_INT m, F77_INT n,
                           P *tmp_data, F77_INT m1, DM_P *s_vec, P *u,
                           P *v, F77_INT nrow_v1, std::vector<P>& work,
                           F77_INT& lwork, std::vector<F77_INT>& iwork,
                           F77_INT& info)
{
  F77_INT lrwork = -1;          // work space size query
  std::vector<double> rwork (1);
  work.reserve (2);

  GEJSV_COMPLEX_STEP (zgejsv, ZGEJSV, F77_DBLE_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  lrwork = static_cast<F77_INT> (rwork[0]);
  rwork.reserve (lrwork);

  F77_INT liwork = static_cast<F77_INT> (iwork[0]);
  iwork.reserve (liwork);

  GEJSV_COMPLEX_STEP (zgejsv, ZGEJSV, F77_DBLE_CMPLX_ARG);
}

// CGEJSV
template<>
void
svd<FloatComplexMatrix>::gejsv (char& joba, char& jobu, char& jobv,
                                char& jobr, char& jobt, char& jobp,
                                F77_INT m, F77_INT n, P *tmp_data,
                                F77_INT m1, DM_P *s_vec, P *u, P *v,
                                F77_INT nrow_v1, std::vector<P>& work,
                                F77_INT& lwork,
                                std::vector<F77_INT>& iwork, F77_INT& info)
{
  F77_INT lrwork = -1;          // work space size query
  std::vector<float> rwork (1);
  work.reserve (2);

  GEJSV_COMPLEX_STEP (cgejsv, CGEJSV, F77_CMPLX_ARG);

  lwork = static_cast<F77_INT> (work[0].real ());
  work.reserve (lwork);

  lrwork = static_cast<F77_INT> (rwork[0]);
  rwork.reserve (lrwork);

  F77_INT liwork = static_cast<F77_INT> (iwork[0]);
  iwork.reserve (liwork);

  GEJSV_COMPLEX_STEP (cgejsv, CGEJSV, F77_CMPLX_ARG);
}

#undef GEJSV_REAL_STEP
#undef GEJSV_COMPLEX_STEP

template<typename T>
svd<T>::svd (const T& a, svd::Type type, svd::Driver driver)
  : m_type (type), m_driver (driver), m_left_sm (), m_sigma (),
    m_right_sm ()
{
  F77_INT info;

  F77_INT m = to_f77_int (a.rows ());
  F77_INT n = to_f77_int (a.cols ());

  if (m == 0 || n == 0)
    {
      switch (m_type)
        {
        case svd::Type::std:
          m_left_sm = T (m, m, 0);
          for (F77_INT i = 0; i < m; i++)
            m_left_sm.xelem (i, i) = 1;
          m_sigma = DM_T (m, n);
          m_right_sm = T (n, n, 0);
          for (F77_INT i = 0; i < n; i++)
            m_right_sm.xelem (i, i) = 1;
          break;

        case svd::Type::economy:
          m_left_sm = T (m, 0, 0);
          m_sigma = DM_T (0, 0);
          m_right_sm = T (n, 0, 0);
          break;

        case svd::Type::sigma_only:
        default:
          m_sigma = DM_T (0, 1);
          break;
        }
      return;
    }

  T atmp = a;
  P *tmp_data = atmp.fortran_vec ();

  F77_INT min_mn = (m < n ? m : n);

  char jobu = 'A';
  char jobv = 'A';

  F77_INT ncol_u = m;
  F77_INT nrow_vt = n;
  F77_INT nrow_s = m;
  F77_INT ncol_s = n;

  switch (m_type)
    {
    case svd::Type::economy:
      jobu = jobv = 'S';
      ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
      break;

    case svd::Type::sigma_only:

      // Note:  for this case, both jobu and jobv should be 'N', but there
      // seems to be a bug in dgesvd from Lapack V2.0.  To demonstrate the
      // bug, set both jobu and jobv to 'N' and find the singular values of
      // [eye(3), eye(3)].  The result is [-sqrt(2), -sqrt(2), -sqrt(2)].
      //
      // For Lapack 3.0, this problem seems to be fixed.

      jobu = jobv = 'N';
      ncol_u = nrow_vt = 1;
      break;

    default:
      break;
    }

  if (! (jobu == 'N' || jobu == 'O'))
    m_left_sm.resize (m, ncol_u);

  P *u = m_left_sm.fortran_vec ();

  m_sigma.resize (nrow_s, ncol_s);
  DM_P *s_vec = m_sigma.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O'))
    {
      if (m_driver == svd::Driver::GEJSV)
        m_right_sm.resize (n, nrow_vt);
      else
        m_right_sm.resize (nrow_vt, n);
    }

  P *vt = m_right_sm.fortran_vec ();

  // Query _GESVD for the correct dimension of WORK.

  F77_INT lwork = -1;

  std::vector<P> work (1);

  const F77_INT f77_int_one = static_cast<F77_INT> (1);
  F77_INT m1 = std::max (m, f77_int_one);
  F77_INT nrow_vt1 = std::max (nrow_vt, f77_int_one);

  if (m_driver == svd::Driver::GESVD)
    gesvd (jobu, jobv, m, n, tmp_data, m1, s_vec, u, vt, nrow_vt1,
           work, lwork, info);
  else if (m_driver == svd::Driver::GESDD)
    {
      assert (jobu == jobv);
      char jobz = jobu;

      std::vector<F77_INT> iwork (8 * std::min (m, n));

      gesdd (jobz, m, n, tmp_data, m1, s_vec, u, vt, nrow_vt1,
             work, lwork, iwork.data (), info);
    }
  else if (m_driver == svd::Driver::GEJSV)
    {
      bool transposed = false;
      if (n > m)
        {
          // GEJSV only accepts m >= n, thus we need to transpose here
          transposed = true;

          std::swap (m, n);
          m1 = std::max (m, f77_int_one);
          nrow_vt1 = std::max (n, f77_int_one);  // we have m > n
          if (m_type == svd::Type::sigma_only)
            nrow_vt1 = 1;
          std::swap (jobu, jobv);

          atmp = atmp.hermitian ();
          tmp_data = atmp.fortran_vec ();

          // Swap pointers of U and V.
          u  = m_right_sm.fortran_vec ();
          vt = m_left_sm.fortran_vec ();
        }

      // translate jobu and jobv from gesvd to gejsv.
      std::unordered_map<char, std::string> job_svd2jsv;
      job_svd2jsv['A'] = "FJ";
      job_svd2jsv['S'] = "UV";
      job_svd2jsv['O'] = "WW";
      job_svd2jsv['N'] = "NN";
      jobu = job_svd2jsv[jobu][0];
      jobv = job_svd2jsv[jobv][1];

      char joba = 'F';  // 'F': most conservative
      char jobr = 'R';  // 'R' is recommended.
      char jobt = 'N';  // or 'T', but that requires U and V appear together
      char jobp = 'N';  // use 'P' if denormal is poorly implemented.

      std::vector<F77_INT> iwork (std::max<F77_INT> (m + 3*n, 1));

      gejsv (joba, jobu, jobv, jobr, jobt, jobp, m, n, tmp_data, m1,
             s_vec, u, vt, nrow_vt1, work, lwork, iwork, info);

      if (iwork[2] == 1)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:convergence", "svd: (driver: GEJSV) "
           "Denormal occurred, possible loss of accuracy.");

      if (info < 0)
        (*current_liboctave_error_handler)
          ("svd: (driver: GEJSV) Illegal argument at #%d",
           static_cast<int> (-info));
      else if (info > 0)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:convergence", "svd: (driver: GEJSV) "
           "Fail to converge within max sweeps, "
           "possible inaccurate result.");

      if (transposed)  // put things that need to transpose back here
        std::swap (m, n);
    }
  else
    (*current_liboctave_error_handler) ("svd: unknown driver");

  // LAPACK can return -0 which is a small problem (bug #55710).
  for (octave_idx_type i = 0; i < m_sigma.diag_length (); i++)
    {
      if (! m_sigma.dgxelem (i))
        m_sigma.dgxelem (i) = DM_P (0);
    }

  // GESVD and GESDD return VT instead of V, GEJSV return V.
  if (! (jobv == 'N' || jobv == 'O') && (m_driver != svd::Driver::GEJSV))
    m_right_sm = m_right_sm.hermitian ();
}

// Instantiations we need.

template class svd<Matrix>;

template class svd<FloatMatrix>;

template class svd<ComplexMatrix>;

template class svd<FloatComplexMatrix>;

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
