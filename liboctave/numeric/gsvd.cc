////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1997-2021 The Octave Project Developers
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

#include <vector>

#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "gsvd.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-shlib.h"

namespace octave
{
  static std::map<std::string, void *> gsvd_fcn;

  static bool have_DGGSVD3 = false;
  static bool gsvd_initialized = false;

  /* Hack to stringize macro results. */
#define xSTRINGIZE(x) #x
#define STRINGIZE(x) xSTRINGIZE(x)

  static void initialize_gsvd (void)
  {
    if (gsvd_initialized)
      return;

    dynamic_library libs ("");
    if (! libs)
      {
        // FIXME: Should we throw an error if we cannot check the libraries?
        have_DGGSVD3 = false;
        return;
      }

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
     F77_INT &,                 // K
     F77_INT &,                 // L
     T1*,                       // A(LDA,N)
     const F77_INT&,            // LDA
     T1*,                       // B(LDB,N)
     const F77_INT&,            // LDB
     T1*,                       // ALPHA(N)
     T1*,                       // BETA(N)
     T1*,                       // U(LDU,M)
     const F77_INT&,            // LDU
     T1*,                       // V(LDV,P)
     const F77_INT&,            // LDV
     T1*,                       // Q(LDQ,N)
     const F77_INT&,            // LDQ
     T1*,                       // WORK
     F77_INT*,                  // IWORK(N)
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
     F77_INT &,                 // K
     F77_INT &,                 // L
     T1*,                       // A(LDA,N)
     const F77_INT&,            // LDA
     T1*,                       // B(LDB,N)
     const F77_INT&,            // LDB
     T1*,                       // ALPHA(N)
     T1*,                       // BETA(N)
     T1*,                       // U(LDU,M)
     const F77_INT&,            // LDU
     T1*,                       // V(LDV,P)
     const F77_INT&,            // LDV
     T1*,                       // Q(LDQ,N)
     const F77_INT&,            // LDQ
     T1*,                       // WORK
     const F77_INT&,            // LWORK
     F77_INT*,                  // IWORK(N)
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
     F77_INT &,                 // K
     F77_INT &,                 // L
     T1*,                       // A(LDA,N)
     const F77_INT&,            // LDA
     T1*,                       // B(LDB,N)
     const F77_INT&,            // LDB
     T2*,                       // ALPHA(N)
     T2*,                       // BETA(N)
     T1*,                       // U(LDU,M)
     const F77_INT&,            // LDU
     T1*,                       // V(LDV,P)
     const F77_INT&,            // LDV
     T1*,                       // Q(LDQ,N)
     const F77_INT&,            // LDQ
     T1*,                       // WORK
     T2*,                       // RWORK
     F77_INT*,                  // IWORK(N)
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
     F77_INT &,                 // K
     F77_INT &,                 // L
     T1*,                       // A(LDA,N)
     const F77_INT&,            // LDA
     T1*,                       // B(LDB,N)
     const F77_INT&,            // LDB
     T2*,                       // ALPHA(N)
     T2*,                       // BETA(N)
     T1*,                       // U(LDU,M)
     const F77_INT&,            // LDU
     T1*,                       // V(LDV,P)
     const F77_INT&,            // LDV
     T1*,                       // Q(LDQ,N)
     const F77_INT&,            // LDQ
     T1*,                       // WORK
     const F77_INT&,            // LWORK
     T2*,                       // RWORK
     F77_INT*,                  // IWORK(N)
     F77_INT&                   // INFO
     F77_CHAR_ARG_LEN_DECL
     F77_CHAR_ARG_LEN_DECL
     F77_CHAR_ARG_LEN_DECL);
  };

  // template specializations
  typedef real_ggsvd3_ptr<F77_DBLE>::type dggsvd3_type;
  typedef real_ggsvd_ptr<F77_DBLE>::type dggsvd_type;
  typedef real_ggsvd3_ptr<F77_REAL>::type sggsvd3_type;
  typedef real_ggsvd_ptr<F77_REAL>::type sggsvd_type;
  typedef comp_ggsvd3_ptr<F77_DBLE_CMPLX, F77_DBLE>::type zggsvd3_type;
  typedef comp_ggsvd_ptr<F77_DBLE_CMPLX, F77_DBLE>::type zggsvd_type;
  typedef comp_ggsvd3_ptr<F77_CMPLX, F77_REAL>::type cggsvd3_type;
  typedef comp_ggsvd_ptr<F77_CMPLX, F77_REAL>::type cggsvd_type;

  namespace math
  {
    template <>
    void
    gsvd<Matrix>::ggsvd (char& jobu, char& jobv, char& jobq, F77_INT m,
                         F77_INT n, F77_INT p, F77_INT& k, F77_INT& l,
                         double *tmp_dataA, F77_INT m1, double *tmp_dataB,
                         F77_INT p1, Matrix& alpha, Matrix& beta, double *u,
                         F77_INT nrow_u, double *v, F77_INT nrow_v, double *q,
                         F77_INT nrow_q, Matrix& work, F77_INT lwork,
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
                 work.fortran_vec (), lwork, iwork, info
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
                 work.fortran_vec (), iwork, info
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
                              F77_INT nrow_q, FloatMatrix& work,
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
                 work.fortran_vec (), lwork, iwork, info
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
                 work.fortran_vec (), iwork, info
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
                                F77_INT nrow_q, ComplexMatrix& work,
                                F77_INT lwork, F77_INT *iwork, F77_INT& info)
    {
      if (! gsvd_initialized)
        initialize_gsvd ();

      Matrix rwork(2*n, 1);
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
                 F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                 lwork, rwork.fortran_vec (), iwork, info
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
                 F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                 rwork.fortran_vec (), iwork, info
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
                                     FloatComplexMatrix& work, F77_INT lwork,
                                     F77_INT *iwork, F77_INT& info)
    {
      if (! gsvd_initialized)
        initialize_gsvd ();

      FloatMatrix rwork(2*n, 1);
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
                 F77_CMPLX_ARG (work.fortran_vec ()), lwork,
                 rwork.fortran_vec (), iwork, info
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
                 F77_CMPLX_ARG (work.fortran_vec ()),
                 rwork.fortran_vec (), iwork, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1));
        }
    }

    template <typename T>
    T
    gsvd<T>::left_singular_matrix_A (void) const
    {
      if (type == gsvd::Type::sigma_only)
        {
          (*current_liboctave_error_handler)
            ("gsvd: U not computed because type == gsvd::sigma_only");
          return T ();
        }
      else
        return left_smA;
    }

    template <typename T>
    T
    gsvd<T>::left_singular_matrix_B (void) const
    {
      if (type == gsvd::Type::sigma_only)
        {
          (*current_liboctave_error_handler)
            ("gsvd: V not computed because type == gsvd::sigma_only");
          return T ();
        }
      else
        return left_smB;
    }

    template <typename T>
    T
    gsvd<T>::right_singular_matrix (void) const
    {
      if (type == gsvd::Type::sigma_only)
        {
          (*current_liboctave_error_handler)
            ("gsvd: X not computed because type == gsvd::sigma_only");
          return T ();
        }
      else
        return right_sm;
    }

    template <typename T>
    T
    gsvd<T>::R_matrix (void) const
    {
      if (type != gsvd::Type::std)
        {
          (*current_liboctave_error_handler)
            ("gsvd: R not computed because type != gsvd::std");
          return T ();
        }
      else
        return R;
    }

    template <typename T>
    gsvd<T>::gsvd (const T& a, const T& b, gsvd::Type gsvd_type)
    {
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

          // Note:  for this case, both jobu and jobv should be 'N', but
          // there seems to be a bug in dgesvd from Lapack V2.0.  To
          // demonstrate the bug, set both jobu and jobv to 'N' and find
          // the singular values of [eye(3), eye(3)].  The result is
          // [-sqrt(2), -sqrt(2), -sqrt(2)].
          //
          // For Lapack 3.0, this problem seems to be fixed.

          jobu = 'N';
          jobv = 'N';
          jobq = 'N';
          nrow_u = nrow_v = nrow_q = 1;
          break;

        default:
          break;
        }

      type = gsvd_type;

      if (! (jobu == 'N' || jobu == 'O'))
        left_smA.resize (nrow_u, m);

      P *u = left_smA.fortran_vec ();

      if (! (jobv == 'N' || jobv == 'O'))
        left_smB.resize (nrow_v, p);

      P *v = left_smB.fortran_vec ();

      if (! (jobq == 'N' || jobq == 'O'))
        right_sm.resize (nrow_q, n);

      P *q = right_sm.fortran_vec ();

      real_matrix alpha (n, 1);
      real_matrix beta (n, 1);

      std::vector<F77_INT> iwork (n);

      if (! gsvd_initialized)
        initialize_gsvd ();

      F77_INT lwork;
      if (have_DGGSVD3)
        {
          lwork = -1;
          T work_tmp (1, 1);

          gsvd<T>::ggsvd (jobu, jobv, jobq, m, n, p, k, l,
                          tmp_dataA, m, tmp_dataB, p, alpha, beta, u,
                          nrow_u, v, nrow_v, q, nrow_q, work_tmp, lwork,
                          iwork.data (), info);

          lwork = static_cast<F77_INT> (std::abs (work_tmp(0, 0)));
        }
      else
        {
          lwork = 3*n;
          lwork = (lwork > m ? lwork : m);
          lwork = (lwork > p ? lwork : p) + n;
        }
      info = 0;

      T work (lwork, 1);

      gsvd<T>::ggsvd (jobu, jobv, jobq, m, n, p, k, l,
                      tmp_dataA, m, tmp_dataB, p, alpha, beta, u,
                      nrow_u, v, nrow_v, q, nrow_q, work, lwork, iwork.data (),
                      info);

      if (info < 0)
        (*current_liboctave_error_handler)
          ("*ggsvd.f: argument %" OCTAVE_F77_INT_TYPE_FORMAT " illegal",
           -info);
      else
        {
          if (info > 0)
            (*current_liboctave_error_handler)
              ("*ggsvd.f: Jacobi-type procedure failed to converge.");
          else
            {
              F77_INT i, j;

              if (gsvd<T>::Type::std == gsvd_type)
                {
                  R.resize(k+l, k+l);
                  int astart = n-k-l;
                  if (m - k - l >=  0)
                    {
                      astart = n-k-l;
                      // R is stored in A(1:K+L,N-K-L+1:N)
                      for (i = 0; i < k+l; i++)
                        for (j = 0; j < k+l; j++)
                          R.xelem (i, j) = atmp.xelem (i, astart + j);
                    }
                  else
                    {
                      // (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N),
                      // ( 0  R22 R23 )

                      for (i = 0; i < m; i++)
                        for (j = 0; j < k+l; j++)
                          R.xelem (i, j) = atmp.xelem (i, astart + j);
                      // and R33 is stored in B(M-K+1:L,N+M-K-L+1:N)
                      for (i = k+l-1; i >=m; i--)
                        {
                          for (j = 0; j < m; j++)
                            R.xelem(i, j) = 0.0;
                          for (j = m; j < k+l; j++)
                            R.xelem (i, j) = btmp.xelem (i - k, astart + j);
                        }
                    }
                }

              if (m-k-l >= 0)
                {
                  // Fills in C and S
                  sigmaA.resize (l, l);
                  sigmaB.resize (l, l);
                  for (i = 0; i < l; i++)
                    {
                      sigmaA.dgxelem(i) = alpha.elem(k+i);
                      sigmaB.dgxelem(i) = beta.elem(k+i);
                    }
                }
              else
                {
                  // Fills in C and S
                  sigmaA.resize (m-k, m-k);
                  sigmaB.resize (m-k, m-k);
                  for (i = 0; i < m-k; i++)
                    {
                      sigmaA.dgxelem(i) = alpha.elem(k+i);
                      sigmaB.dgxelem(i) = beta.elem(k+i);
                    }
                }
            }
        }
    }

    // Instantiations we need.
    template class gsvd<Matrix>;
    template class gsvd<FloatMatrix>;
    template class gsvd<ComplexMatrix>;
    template class gsvd<FloatComplexMatrix>;
  }
}
