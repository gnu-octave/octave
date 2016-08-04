// Copyright (C) 1996, 1997 John W. Eaton
// Copyright (C) 2006 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, see <http://www.gnu.org/licenses/>.

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "dbleGSVD.h"
#include "f77-fcn.h"
#include "lo-error.h"

/*
   uncomment those lines to monitor k and l
   #include "oct-obj.h"
   #include "pager.h"
*/

extern "C"
{
  F77_RET_T
  F77_FUNC (dggsvd, DGGSVD)
   (
     F77_CONST_CHAR_ARG_DECL,   // JOBU    (input) CHARACTER*1
     F77_CONST_CHAR_ARG_DECL,   // JOBV    (input) CHARACTER*1
     F77_CONST_CHAR_ARG_DECL,   // JOBQ    (input) CHARACTER*1
     const octave_idx_type&,    // M       (input) INTEGER
     const octave_idx_type&,    // N       (input) INTEGER
     const octave_idx_type&,    // P       (input) INTEGER
     octave_idx_type &,         // K       (output) INTEGER
     octave_idx_type &,         // L       (output) INTEGER
     double*,                   // A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
     const octave_idx_type&,    // LDA     (input) INTEGER
     double*,                   // B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
     const octave_idx_type&,    // LDB     (input) INTEGER
     double*,                   // ALPHA   (output) DOUBLE PRECISION array, dimension (N)
     double*,                   // BETA    (output) DOUBLE PRECISION array, dimension (N)
     double*,                   // U       (output) DOUBLE PRECISION array, dimension (LDU,M)
     const octave_idx_type&,    // LDU     (input) INTEGER
     double*,                   // V       (output) DOUBLE PRECISION array, dimension (LDV,P)
     const octave_idx_type&,    // LDV     (input) INTEGER
     double*,                   // Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
     const octave_idx_type&,    // LDQ     (input) INTEGER
     double*,                   // WORK    (workspace) DOUBLE PRECISION array
     int*,                      // IWORK   (workspace/output) INTEGER array, dimension (N)
     octave_idx_type&           // INFO    (output)INTEGER
     F77_CHAR_ARG_LEN_DECL
     F77_CHAR_ARG_LEN_DECL
     F77_CHAR_ARG_LEN_DECL
     );
}

Matrix
GSVD::left_singular_matrix_A (void) const
{
  if (type_computed == GSVD::sigma_only)
    {
      (*current_liboctave_error_handler)
        ("dbleGSVD: U not computed because type == GSVD::sigma_only");
      return Matrix ();
    }
  else
    return left_smA;
}

Matrix
GSVD::left_singular_matrix_B (void) const
{
  if (type_computed == GSVD::sigma_only)
    {
      (*current_liboctave_error_handler)
        ("dbleGSVD: V not computed because type == GSVD::sigma_only");
      return Matrix ();
    }
  else
    return left_smB;
}

Matrix
GSVD::right_singular_matrix (void) const
{
  if (type_computed == GSVD::sigma_only)
    {
      (*current_liboctave_error_handler)
        ("dbleGSVD: X not computed because type == GSVD::sigma_only");
      return Matrix ();
    }
  else
    return right_sm;
}
Matrix
GSVD::R_matrix (void) const
{
  if (type_computed != GSVD::std)
    {
      (*current_liboctave_error_handler)
        ("dbleGSVD: R not computed because type != GSVD::std");
      return Matrix ();
    }
  else
    return R;
}

octave_idx_type
GSVD::init (const Matrix& a, const Matrix& b, GSVD::type gsvd_type)
{
  octave_idx_type info;

  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();
  octave_idx_type p = b.rows ();
  
  Matrix atmp = a;
  double *tmp_dataA = atmp.fortran_vec ();
  
  Matrix btmp = b;
  double *tmp_dataB = btmp.fortran_vec ();

  // octave_idx_type min_mn = m < n ? m : n;

  char jobu = 'U';
  char jobv = 'V';
  char jobq = 'Q';

  octave_idx_type nrow_u = m;
  octave_idx_type nrow_v = p;
  octave_idx_type nrow_q = n;

  octave_idx_type k, l;

  switch (gsvd_type)
    {

    case GSVD::sigma_only:

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

  type_computed = gsvd_type;

  if (! (jobu == 'N' || jobu == 'O')) {
    left_smA.resize (nrow_u, m);
  }
  
  double *u = left_smA.fortran_vec ();

  if (! (jobv == 'N' || jobv == 'O')) {
    left_smB.resize (nrow_v, p);
  }

  double *v = left_smB.fortran_vec ();

  if (! (jobq == 'N' || jobq == 'O')) {
    right_sm.resize (nrow_q, n);
  }
  double *q = right_sm.fortran_vec ();
  
  octave_idx_type lwork = 3*n;
  lwork = lwork > m ? lwork : m;
  lwork = (lwork > p ? lwork : p) + n;

  Array<double> work (dim_vector (lwork, 1));
  Array<double> alpha (dim_vector (n, 1));
  Array<double> beta (dim_vector (n, 1));
  Array<int>    iwork (dim_vector (n, 1));

  F77_XFCN (dggsvd, DGGSVD, (F77_CONST_CHAR_ARG2 (&jobu, 1),
                             F77_CONST_CHAR_ARG2 (&jobv, 1),
                             F77_CONST_CHAR_ARG2 (&jobq, 1),
                             m, n, p, k, l, tmp_dataA, m,
                             tmp_dataB, p, alpha.fortran_vec (),
                             beta.fortran_vec (), u, nrow_u,
                             v, nrow_v, q, nrow_q, work.fortran_vec (),
                             iwork.fortran_vec (), info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));
  
  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dggsvd");
 
  if (info < 0) {
    (*current_liboctave_error_handler) ("dggsvd.f: argument %d illegal", -info);
  } else {
    if (info > 0) {
      (*current_liboctave_error_handler) ("dggsvd.f: Jacobi-type procedure failed to converge.");
    } else {
      octave_idx_type i, j;
      
      if (GSVD::std == gsvd_type) {
        R.resize(k+l, k+l);
        int astart = n-k-l;
        if (m - k - l >=  0) {
          int astart = n-k-l;
          /*
           *  R is stored in A(1:K+L,N-K-L+1:N)
           */
          for (i = 0; i < k+l; i++)
            for (j = 0; j < k+l; j++)
              R.xelem(i, j) = atmp.xelem(i, astart + j);
        } else {
          /*
           *    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N),
           *    ( 0  R22 R23 )
           */

           for (i = 0; i < m; i++)
             for (j = 0; j < k+l; j++)
               R.xelem(i, j) = atmp.xelem(i, astart + j);
           /*
            * and R33 is stored in B(M-K+1:L,N+M-K-L+1:N)
            */
           for (i = k+l-1; i >=m; i--) {
             for (j = 0; j < m; j++)
               R.xelem(i, j) = 0.0;
             for (j = m; j < k+l; j++)
               R.xelem(i, j) = btmp.xelem(i - k, astart + j);
           }
        }
      }
      /*
        uncomment this to monitor k and l
        octave_value tmp;
        octave_stdout << "dbleGSVD k: ";
        tmp = k;
        tmp.print(octave_stdout);
        octave_stdout << "\n";
        octave_stdout << "dbleGSVD l: ";
        tmp = l;
        tmp.print(octave_stdout);
        octave_stdout << "\n";
      */

      if (m-k-l >= 0) {
        // Fills in C and S
        sigmaA.resize (l, l);
        sigmaB.resize (l, l);
        for (i = 0; i < l; i++) {
          sigmaA.dgxelem(i) = alpha.elem(k+i);
          sigmaB.dgxelem(i) = beta.elem(k+i);
        }
      } else {
        // Fills in C and S
        sigmaA.resize (m-k, m-k);
        sigmaB.resize (m-k, m-k);
        for (i = 0; i < m-k; i++) {
          sigmaA.dgxelem(i) = alpha.elem(k+i);
          sigmaB.dgxelem(i) = beta.elem(k+i);
        }
      }
    }
  }
  return info;
}

std::ostream&
operator << (std::ostream& os, const GSVD& a)
{
  os << a.left_singular_matrix_A () << "\n";
  os << a.left_singular_matrix_B () << "\n";
  os << a.singular_values_A () << "\n";
  os << a.singular_values_B () << "\n";
  os << a.right_singular_matrix () << "\n";

  return os;
}
