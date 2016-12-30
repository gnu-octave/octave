/*

Copyright (C) 2016 Barbara Lócsi
Copyright (C) 2006 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <vector>

#include "gsvd.h"

#include "lo-error.h"
#include "lo-lapack-proto.h"
#include "dMatrix.h"
#include "fMatrix.h"
#include "CMatrix.h"
#include "fCMatrix.h"
#include "dDiagMatrix.h"
#include "fDiagMatrix.h"

namespace octave
{
  namespace math
  {
    template <>
    void
    gsvd<Matrix>::ggsvd (char& jobu, char& jobv, char& jobq, F77_INT m,
                         F77_INT n, F77_INT p, F77_INT& k, F77_INT& l,
                         double *tmp_dataA, F77_INT m1, double *tmp_dataB,
                         F77_INT p1, Matrix& alpha, Matrix& beta, double *u,
                         F77_INT nrow_u, double *v, F77_INT nrow_v, double *q,
                         F77_INT nrow_q, Matrix& work, F77_INT* iwork,
                         F77_INT& info)
    {
      F77_XFCN (dggsvd, DGGSVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 F77_CONST_CHAR_ARG2 (&jobq, 1),
                                 m, n, p, k, l, tmp_dataA, m1,
                                 tmp_dataB, p1, alpha.fortran_vec (),
                                 beta.fortran_vec (), u, nrow_u,
                                 v, nrow_v, q, nrow_q, work.fortran_vec (),
                                 iwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
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
                              F77_INT* iwork, F77_INT& info)
    {
      F77_XFCN (sggsvd, SGGSVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 F77_CONST_CHAR_ARG2 (&jobq, 1),
                                 m, n, p, k, l, tmp_dataA, m1,
                                 tmp_dataB, p1, alpha.fortran_vec (),
                                 beta.fortran_vec (), u, nrow_u,
                                 v, nrow_v, q, nrow_q, work.fortran_vec (),
                                 iwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
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
                                F77_INT* iwork, F77_INT& info)
    {
      Matrix rwork(2*n, 1);
      F77_XFCN (zggsvd, ZGGSVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 F77_CONST_CHAR_ARG2 (&jobq, 1),
                                 m, n, p, k, l, F77_DBLE_CMPLX_ARG (tmp_dataA),
                                 m1, F77_DBLE_CMPLX_ARG (tmp_dataB), p1,
                                 alpha.fortran_vec (), beta.fortran_vec (),
                                 F77_DBLE_CMPLX_ARG (u), nrow_u,
                                 F77_DBLE_CMPLX_ARG (v), nrow_v,
                                 F77_DBLE_CMPLX_ARG (q), nrow_q,
                                 F77_DBLE_CMPLX_ARG (work.fortran_vec ()),
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
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
                                     FloatComplexMatrix& work,
                                     F77_INT* iwork, F77_INT& info)
    {
      FloatMatrix rwork(2*n, 1);
      F77_XFCN (cggsvd, CGGSVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                                 F77_CONST_CHAR_ARG2 (&jobv, 1),
                                 F77_CONST_CHAR_ARG2 (&jobq, 1),
                                 m, n, p, k, l, F77_CMPLX_ARG (tmp_dataA), m1,
                                 F77_CMPLX_ARG (tmp_dataB), p1,
                                 alpha.fortran_vec (), beta.fortran_vec (),
                                 F77_CMPLX_ARG (u), nrow_u,
                                 F77_CMPLX_ARG (v), nrow_v,
                                 F77_CMPLX_ARG (q), nrow_q,
                                 F77_CMPLX_ARG (work.fortran_vec ()),
                                 rwork.fortran_vec (), iwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));
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

      F77_INT m = octave::to_f77_int (a.rows ());
      F77_INT n = octave::to_f77_int (a.cols ());
      F77_INT p = octave::to_f77_int (b.rows ());

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

      F77_INT lwork = 3*n;
      lwork = lwork > m ? lwork : m;
      lwork = (lwork > p ? lwork : p) + n;

      T work (lwork, 1);
      real_matrix alpha (n, 1);
      real_matrix beta (n, 1);

      std::vector<F77_INT> iwork (n);

      gsvd<T>::ggsvd (jobu, jobv, jobq, m, n, p, k, l,
                      tmp_dataA, m, tmp_dataB, p, alpha, beta, u,
                      nrow_u, v, nrow_v, q, nrow_q, work, iwork.data (), info);

      if (f77_exception_encountered)
        (*current_liboctave_error_handler) ("unrecoverable error in *ggsvd");

      if (info < 0)
        (*current_liboctave_error_handler) ("*ggsvd.f: argument %d illegal",
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
