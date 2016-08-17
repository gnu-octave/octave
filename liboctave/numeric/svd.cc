/*

Copyright (C) 2016 Carnë Draug
Copyright (C) 1994-2016 John W. Eaton

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

#include "svd.h"

#include <cassert>
#include <algorithm>

#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "fDiagMatrix.h"
#include "dMatrix.h"
#include "fCMatrix.h"
#include "fMatrix.h"
#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "oct-locbuf.h"

namespace octave
{
  namespace math
  {
    template <typename T>
    T
    svd<T>::left_singular_matrix (void) const
    {
      if (m_type == svd::Type::sigma_only)
        (*current_liboctave_error_handler)
          ("svd: U not computed because type == svd::sigma_only");

      return left_sm;
    }

    template <typename T>
    T
    svd<T>::right_singular_matrix (void) const
    {
      if (m_type == svd::Type::sigma_only)
        (*current_liboctave_error_handler)
          ("svd: V not computed because type == svd::sigma_only");

      return right_sm;
    }


    // GESVD specializations

#define GESVD_REAL_STEP(f, F)                                   \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobu, 1),            \
                     F77_CONST_CHAR_ARG2 (&jobv, 1),            \
                     m, n, tmp_data, m1, s_vec, u, m1, vt,      \
                     nrow_vt1, work.data (), lwork, info        \
                     F77_CHAR_ARG_LEN (1)                       \
                     F77_CHAR_ARG_LEN (1)))

#define GESVD_COMPLEX_STEP(f, F, CMPLX_ARG)             \
    F77_XFCN (f, F, (F77_CONST_CHAR_ARG2 (&jobu, 1),    \
                     F77_CONST_CHAR_ARG2 (&jobv, 1),    \
                     m, n, CMPLX_ARG (tmp_data),        \
                     m1, s_vec, CMPLX_ARG (u), m1,      \
                     CMPLX_ARG (vt), nrow_vt1,          \
                     CMPLX_ARG (work.data ()),          \
                     lwork, rwork.data (), info         \
                     F77_CHAR_ARG_LEN (1)               \
                     F77_CHAR_ARG_LEN (1)))

    // DGESVD
    template<>
    void
    svd<Matrix>::gesvd (char& jobu, char& jobv, octave_idx_type m,
                        octave_idx_type n, double* tmp_data, octave_idx_type m1,
                        double* s_vec, double* u, double* vt,
                        octave_idx_type nrow_vt1, std::vector<double>& work,
                        octave_idx_type& lwork, octave_idx_type& info)
    {
      GESVD_REAL_STEP (dgesvd, DGESVD);

      lwork = work[0];
      work.reserve (lwork);

      GESVD_REAL_STEP (dgesvd, DGESVD);
    }

    // SGESVD
    template<>
    void
    svd<FloatMatrix>::gesvd (char& jobu, char& jobv, octave_idx_type m,
                             octave_idx_type n, float* tmp_data,
                             octave_idx_type m1, float* s_vec, float* u, float* vt,
                             octave_idx_type nrow_vt1, std::vector<float>& work,
                             octave_idx_type& lwork, octave_idx_type& info)
    {
      GESVD_REAL_STEP (sgesvd, SGESVD);

      lwork = work[0];
      work.reserve (lwork);

      GESVD_REAL_STEP (sgesvd, SGESVD);
    }

    // ZGESVD
    template<>
    void
    svd<ComplexMatrix>::gesvd (char& jobu, char& jobv, octave_idx_type m,
                               octave_idx_type n, Complex* tmp_data,
                               octave_idx_type m1, double* s_vec, Complex* u,
                               Complex* vt, octave_idx_type nrow_vt1,
                               std::vector<Complex>& work,
                               octave_idx_type& lwork, octave_idx_type& info)
    {
      std::vector<double> rwork (5 * std::max (m, n));

      GESVD_COMPLEX_STEP (zgesvd, ZGESVD, F77_DBLE_CMPLX_ARG);

      lwork = work[0].real ();
      work.reserve (lwork);

      GESVD_COMPLEX_STEP (zgesvd, ZGESVD, F77_DBLE_CMPLX_ARG);
    }

    // CGESVD
    template<>
    void
    svd<FloatComplexMatrix>::gesvd (char& jobu, char& jobv,
                                    octave_idx_type m, octave_idx_type n,
                                    FloatComplex* tmp_data, octave_idx_type m1,
                                    float* s_vec, FloatComplex* u,
                                    FloatComplex* vt, octave_idx_type nrow_vt1,
                                    std::vector<FloatComplex>& work,
                                    octave_idx_type& lwork, octave_idx_type& info)
    {
      std::vector<float> rwork (5 * std::max (m, n));

      GESVD_COMPLEX_STEP (cgesvd, CGESVD, F77_CMPLX_ARG);

      lwork = work[0].real ();
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
    void
    svd<Matrix>::gesdd (char& jobz, octave_idx_type m, octave_idx_type n,
                        double* tmp_data, octave_idx_type m1,
                        double* s_vec, double* u,
                        double* vt, octave_idx_type nrow_vt1,
                        std::vector<double>& work, octave_idx_type& lwork,
                        octave_idx_type* iwork, octave_idx_type& info)
    {
      GESDD_REAL_STEP (dgesdd, DGESDD);

      lwork = work[0];
      work.reserve (lwork);

      GESDD_REAL_STEP (dgesdd, DGESDD);
    }

    // SGESDD
    template<>
    void
    svd<FloatMatrix>::gesdd (char& jobz, octave_idx_type m, octave_idx_type n,
                             float* tmp_data, octave_idx_type m1,
                             float* s_vec, float* u,
                             float* vt, octave_idx_type nrow_vt1,
                             std::vector<float>& work, octave_idx_type& lwork,
                             octave_idx_type* iwork, octave_idx_type& info)
    {
      GESDD_REAL_STEP (sgesdd, SGESDD);

      lwork = work[0];
      work.reserve (lwork);

      GESDD_REAL_STEP (sgesdd, SGESDD);
    }

    // ZGESDD
    template<>
    void
    svd<ComplexMatrix>::gesdd (char& jobz, octave_idx_type m, octave_idx_type n,
                               Complex* tmp_data, octave_idx_type m1,
                               double* s_vec, Complex* u,
                               Complex* vt, octave_idx_type nrow_vt1,
                               std::vector<Complex>& work, octave_idx_type& lwork,
                               octave_idx_type* iwork, octave_idx_type& info)
    {

      octave_idx_type min_mn = std::min (m, n);

      octave_idx_type lrwork;
      if (jobz == 'N')
        lrwork = 7*min_mn;
      else
        lrwork = 5*min_mn*min_mn + 5*min_mn;

      std::vector<double> rwork (lrwork);

      GESDD_COMPLEX_STEP (zgesdd, ZGESDD, F77_DBLE_CMPLX_ARG);

      lwork = work[0].real ();
      work.reserve (lwork);

      GESDD_COMPLEX_STEP (zgesdd, ZGESDD, F77_DBLE_CMPLX_ARG);
    }

    // CGESDD
    template<>
    void
    svd<FloatComplexMatrix>::gesdd (char& jobz, octave_idx_type m,
                                    octave_idx_type n,
                                    FloatComplex* tmp_data, octave_idx_type m1,
                                    float* s_vec, FloatComplex* u,
                                    FloatComplex* vt, octave_idx_type nrow_vt1,
                                    std::vector<FloatComplex>& work,
                                    octave_idx_type& lwork, octave_idx_type* iwork,
                                    octave_idx_type& info)
    {
      octave_idx_type min_mn = std::min (m, n);
      octave_idx_type max_mn = std::max (m, n);

      octave_idx_type lrwork;
      if (jobz == 'N')
        lrwork = 5*min_mn;
      else
        lrwork = min_mn * std::max (5*min_mn+7, 2*max_mn+2*min_mn+1);
      std::vector<float> rwork (lrwork);

      GESDD_COMPLEX_STEP (cgesdd, CGESDD, F77_CMPLX_ARG);

      lwork = work[0].real ();
      work.reserve (lwork);

      GESDD_COMPLEX_STEP (cgesdd, CGESDD, F77_CMPLX_ARG);
    }

#undef GESDD_REAL_STEP
#undef GESDD_COMPLEX_STEP


    template<typename T>
    svd<T>::svd (const T& a, svd::Type type,
                 svd::Driver driver)
      : m_type (type), m_driver (driver), left_sm (), sigma (), right_sm ()
    {
      octave_idx_type info;

      octave_idx_type m = a.rows ();
      octave_idx_type n = a.cols ();

      if (m == 0 || n == 0)
        {
          switch (m_type)
            {
            case svd::Type::std:
              left_sm = T (m, m, 0);
              for (octave_idx_type i = 0; i < m; i++)
                left_sm.xelem (i, i) = 1;
              sigma = DM_T (m, n);
              right_sm = T (n, n, 0);
              for (octave_idx_type i = 0; i < n; i++)
                right_sm.xelem (i, i) = 1;
              break;

            case svd::Type::economy:
              left_sm = T (m, 0, 0);
              sigma = DM_T (0, 0);
              right_sm = T (0, n, 0);
              break;

            case svd::Type::sigma_only:
            default:
              sigma = DM_T (0, 1);
              break;
            }
          return;
        }

      T atmp = a;
      P* tmp_data = atmp.fortran_vec ();

      octave_idx_type min_mn = m < n ? m : n;

      char jobu = 'A';
      char jobv = 'A';

      octave_idx_type ncol_u = m;
      octave_idx_type nrow_vt = n;
      octave_idx_type nrow_s = m;
      octave_idx_type ncol_s = n;

      switch (m_type)
        {
        case svd::Type::economy:
          jobu = jobv = 'S';
          ncol_u = nrow_vt = nrow_s = ncol_s = min_mn;
          break;

        case svd::Type::sigma_only:

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

      if (! (jobu == 'N' || jobu == 'O'))
        left_sm.resize (m, ncol_u);

      P* u = left_sm.fortran_vec ();

      sigma.resize (nrow_s, ncol_s);
      DM_P* s_vec = sigma.fortran_vec ();

      if (! (jobv == 'N' || jobv == 'O'))
        right_sm.resize (nrow_vt, n);

      P* vt = right_sm.fortran_vec ();

      // Query _GESVD for the correct dimension of WORK.

      octave_idx_type lwork = -1;

      std::vector<P> work (1);

      octave_idx_type m1 = std::max (m, static_cast<octave_idx_type> (1));
      octave_idx_type nrow_vt1 = std::max (nrow_vt,
                                           static_cast<octave_idx_type> (1));

      if (m_driver == svd::Driver::GESVD)
        gesvd (jobu, jobv, m, n, tmp_data, m1, s_vec, u, vt, nrow_vt1,
               work, lwork, info);
      else if (m_driver == svd::Driver::GESDD)
        {
          assert (jobu == jobv);
          char jobz = jobu;

          std::vector<octave_idx_type> iwork (8 * std::min (m, n));

          gesdd (jobz, m, n, tmp_data, m1, s_vec, u, vt, nrow_vt1,
                 work, lwork, iwork.data (), info);
        }
      else
        abort ();

      if (! (jobv == 'N' || jobv == 'O'))
        right_sm = right_sm.transpose ();
    }

    // Instantiations we need.

    template class svd<Matrix>;

    template class svd<FloatMatrix>;

    template class svd<ComplexMatrix>;

    template class svd<FloatComplexMatrix>;
  }
}
