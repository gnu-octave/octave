////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#include <cmath>

#include <ostream>

#include "Array.h"
#include "CSparse.h"
#include "MatrixType.h"
#include "PermMatrix.h"
#include "chol.h"
#include "dSparse.h"
#include "eigs-base.h"
#include "lo-arpack-proto.h"
#include "lo-blas-proto.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lu.h"
#include "mx-ops.h"
#include "oct-locbuf.h"
#include "oct-rand.h"
#include "sparse-chol.h"
#include "sparse-lu.h"

#if defined (HAVE_ARPACK)

static void
warn_convergence (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:convergence",
     "eigs: 'A - sigma*B' is singular, indicating sigma is exactly "
     "an eigenvalue so convergence is not guaranteed");
}

// Conversion from error number to strings
std::string
arpack_errno2str (const octave_idx_type& errnum, const std::string& fcn_name)
{
  std::string msg;
  std::string bug_msg = "\nThis should not happen.  Please, see https://www.gnu.org/software/octave/bugs.html, and file a bug report";

  switch (errnum)
    {
    case -1:
      msg = "N must be positive";
      break;

    case -2:
      msg = "NEV must be positive";
      break;

    case -3:
      msg = "NCV-NEV >= 2 and less than or equal to N";
      break;

    case -4:
      msg = "The maximum number of Arnoldi update iterations allowed must be greater than zero";
      break;

    case -5:
      msg = "WHICH must be one of 'LM', 'SM', 'LR', 'SR', 'LI', 'SI'";
      break;

    case -6:
      msg = "BMAT must be one of 'I' or 'G'";
      break;

    case -7:
      msg = "Length of private work WORKL array is insufficient";
      break;

    case -8:
      msg = "Error return from LAPACK eigenvalue calculation";
      break;

    case -9:
      if (fcn_name.compare ("zneupd") == 0)
        msg = "Error return from calculation of eigenvectors.  Informational error from LAPACK routine ztrevc";
      else if (fcn_name.compare ("dneupd") == 0)
        msg = "Error return from calculation of eigenvectors.  Informational error from LAPACK routine dtrevc";
      else
        msg = "Starting vector is zero";

      break;

    case -10:
      if (fcn_name.compare ("dneupd") == 0
          || fcn_name.compare ("dnaupd") == 0)
        msg = "IPARAM(7) must be 1,2,3,4";
      else if (fcn_name.compare ("zneupd") == 0
               || fcn_name.compare ("znaupd") == 0)
        msg = "IPARAM(7) must be 1,2,3";
      else
        msg = "IPARAM(7) must be 1,2,3,4,5";

      break;

    case -11:
      msg = "IPARAM(7) = 1 and BMAT = 'G' are incompatible";
      break;

    case -12:
      if (fcn_name.compare ("dnaupd") == 0
          || fcn_name.compare ("znaupd") == 0
          || fcn_name.compare ("dsaupd") == 0)
        msg = std::string ("IPARAM(1) must be equal to 0 or 1");
      else if (fcn_name.compare ("dneupd") == 0
               || fcn_name.compare ("zneupd") == 0)
        msg = "HOWMNY = 'S' not yet implemented";
      else
        msg = "NEV and WHICH = 'BE' are incompatible";

      break;

    case -13:
      if (fcn_name.compare ("dneupd") == 0
          || fcn_name.compare ("zneupd") == 0)
        msg = "HOWMNY must be one of 'A' or 'P' if RVEC = .true.";
      else if (fcn_name.compare ("dsaupd") == 0)
        msg = "NEV and WHICH = 'BE' are incompatible";

      break;

    case -14:
      if (fcn_name.compare ("dneupd") == 0)
        msg = "DNAUPD did not find any eigenvalues to sufficient accuracy.";
      else if (fcn_name.compare ("zneupd") == 0)
        msg = "ZNAUPD did not find any eigenvalues to sufficient accuracy.";
      else if (fcn_name.compare ("dseupd") == 0)
        msg = "DSAUPD did not find any eigenvalues to sufficient accuracy.";

      msg += "  Consider changing tolerance (TOL), maximum iterations (MAXIT), number of Lanzcos basis vectors (P), or starting vector (V0) in OPTS structure.";

      break;

    case -15:
      if (fcn_name.compare ("dseupd") == 0)
        msg = "HOWMNY must be one of 'A' or 'S' if RVEC = .true.";

      break;

    case -16:
      if (fcn_name.compare ("dseupd") == 0)
        msg = "HOWMNY = 'S' not yet implemented";

      break;

    case -9999:
      if (fcn_name.compare ("dnaupd") == 0)
        msg = "Could not build an Arnoldi factorization.  IPARAM(5) returns the size of the current Arnoldi factorization";

      break;

    case 1:
      if (fcn_name.compare ("dneupd") == 0)
        msg = "The Schur form computed by LAPACK routine dlahqr could not be reordered by LAPACK routine dtrsen.  Re-enter subroutine DNEUPD with IPARAM(5)=NCV and increase the size of the arrays DR and DI to have dimension at least dimension NCV and allocate at least NCV columns for Z.  NOTE: Not necessary if Z and V share the same space.  Please notify the authors if this error occurs.";
      else if (fcn_name.compare ("dnaupd") == 0
               || fcn_name.compare ("znaupd") == 0
               || fcn_name.compare ("dsaupd") == 0)
        msg = "Maximum number of iterations taken.  All possible eigenvalues of OP has been found.  IPARAM(5) returns the number of wanted converged Ritz values";
      else if (fcn_name.compare ("znaupd") == 0)
        msg = "The Schur form computed by LAPACK routine csheqr could not be reordered by LAPACK routine ztrsen.  Re-enter subroutine ZNEUPD with IPARAM(5)=NCV and increase the size of the array D to have dimension at least dimension NCV and allocate at least NCV columns for Z.  NOTE: Not necessary if Z and V share the same space.  Please notify the authors if this error occurs.";

      break;

    case 2:
      if (fcn_name.compare ("dnaupd") == 0
          || fcn_name.compare ("znaupd") == 0
          || fcn_name.compare ("dsaupd") == 0)
        msg = "No longer an informational error.  Deprecated starting with release 2 of ARPACK.";

      break;

    case 3:
      if (fcn_name.compare ("dnaupd") == 0
          || fcn_name.compare ("znaupd") == 0
          || fcn_name.compare ("dsaupd") == 0)
        msg = "No shifts could be applied during a cycle of the implicitly restarted Arnoldi iteration.  One possibility is to increase the size of NCV relative to NEV.";

      break;

    }

  if ((errnum != -9) && (errnum != -14) && (errnum != -9999))
    msg.append (bug_msg);  // This is a bug in Octave interface to ARPACK

  return msg;
}

template <typename M, typename SM>
static octave_idx_type
lusolve (const SM& L, const SM& U, M& m)
{
  octave_idx_type err = 0;
  double rcond;
  MatrixType utyp (MatrixType::Upper);

  // Sparse L is lower triangular, Dense L is permuted lower triangular!!!
  MatrixType ltyp (MatrixType::Lower);
  m = L.solve (ltyp, m, err, rcond, nullptr);
  if (err)
    return err;

  m = U.solve (utyp, m, err, rcond, nullptr);

  return err;
}

template <typename SM, typename M>
static M
ltsolve (const SM& L, const ColumnVector& Q, const M& m)
{
  // Solve (Q_mat * L) * x = m, that is L * x = Q_mat' * m = m(Q)
  octave_idx_type n = L.cols ();
  octave_idx_type b_nc = m.cols ();
  octave_idx_type err = 0;
  double rcond;
  MatrixType ltyp (MatrixType::Lower);
  M retval (n, b_nc);
  const double *qv = Q.data ();
  for (octave_idx_type j = 0; j < b_nc; j++)
    {
      for (octave_idx_type i = 0; i < n; i++)
        retval.elem (i, j) = m.elem (static_cast<octave_idx_type> (qv[i]), j);
    }
  return L.solve (ltyp, retval, err, rcond, nullptr);
}

template <typename SM, typename M>
static M
utsolve (const SM& U, const ColumnVector& Q, const M& m)
{
  // Solve (U * Q_mat') * x = m by U * tmp = m, x(Q) = tmp (Q_mat * tmp = x)
  octave_idx_type n = U.cols ();
  octave_idx_type b_nc = m.cols ();
  octave_idx_type err = 0;
  double rcond;
  MatrixType utyp (MatrixType::Upper);
  M tmp = U.solve (utyp, m, err, rcond, nullptr);
  M retval;
  const double *qv = Q.data ();

  if (! err)
    {
      retval.resize (n, b_nc);
      for (octave_idx_type j = 0; j < b_nc; j++)
        {
          for (octave_idx_type i = 0; i < n; i++)
            retval.elem (static_cast<octave_idx_type> (qv[i]), j)
              = tmp.elem (i, j);
        }
    }

  return retval;
}

static bool
vector_product (const SparseMatrix& m, const double *x, double *y)
{
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    y[j] = 0.;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      y[m.ridx (i)] += m.data (i) * x[j];

  return true;
}

static bool
vector_product (const Matrix& m, const double *x, double *y)
{
  F77_INT nr = octave::to_f77_int (m.rows ());
  F77_INT nc = octave::to_f77_int (m.cols ());

  F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           nr, nc, 1.0,  m.data (), nr,
                           x, 1, 0.0, y, 1
                           F77_CHAR_ARG_LEN (1)));

  return true;
}

static bool
vector_product (const SparseComplexMatrix& m, const Complex *x,
                Complex *y)
{
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    y[j] = 0.;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      y[m.ridx (i)] += m.data (i) * x[j];

  return true;
}

static bool
vector_product (const ComplexMatrix& m, const Complex *x, Complex *y)
{
  F77_INT nr = octave::to_f77_int (m.rows ());
  F77_INT nc = octave::to_f77_int (m.cols ());

  F77_XFCN (zgemv, ZGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
                           nr, nc, 1.0, F77_CONST_DBLE_CMPLX_ARG (m.data ()),
                           nr,
                           F77_CONST_DBLE_CMPLX_ARG (x), 1, 0.0,
                           F77_DBLE_CMPLX_ARG (y), 1
                           F77_CHAR_ARG_LEN (1)));

  return true;
}

static bool
make_cholb (Matrix& b, Matrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  octave::math::chol<Matrix> fact (b, info);
  octave_idx_type n = b.cols ();

  if (info != 0)
    return false;
  else
    {
      bt = fact.chol_matrix (); // upper triangular
      b = bt.transpose ();
      permB = ColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        permB(i) = i;
      return true;
    }
}

static bool
make_cholb (SparseMatrix& b, SparseMatrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  octave::math::sparse_chol<SparseMatrix> fact (b, info, false);

  if (info != 0)
    return false;
  else
    {
      b = fact.L (); // lower triangular
      bt = b.transpose ();
      permB = fact.perm () - 1.0;
      return true;
    }
}

static bool
make_cholb (ComplexMatrix& b, ComplexMatrix& bt, ColumnVector& permB)
{
  octave_idx_type info;
  octave::math::chol<ComplexMatrix> fact (b, info);
  octave_idx_type n = b.cols ();

  if (info != 0)
    return false;
  else
    {
      bt = fact.chol_matrix (); // upper triangular
      b = bt.hermitian ();
      permB = ColumnVector (n);
      for (octave_idx_type i = 0; i < n; i++)
        permB(i) = i;
      return true;
    }
}

static bool
make_cholb (SparseComplexMatrix& b, SparseComplexMatrix& bt,
            ColumnVector& permB)
{
  octave_idx_type info;
  octave::math::sparse_chol<SparseComplexMatrix> fact (b, info, false);

  if (info != 0)
    return false;
  else
    {
      b = fact.L (); // lower triangular
      bt = b.hermitian ();
      permB = fact.perm () - 1.0;
      return true;
    }
}

static bool
LuAminusSigmaB (const SparseMatrix& m, const SparseMatrix& b,
                bool cholB, const ColumnVector& permB, double sigma,
                SparseMatrix& L, SparseMatrix& U, octave_idx_type *P,
                octave_idx_type *Q, ColumnVector& r)
{
  bool have_b = ! b.isempty ();
  octave_idx_type n = m.rows ();

  // Calculate LU decomposition of 'M = A - sigma * B'
  // P * (R \ M) * Q = L * U
  SparseMatrix AminusSigmaB (m);

  if (sigma != 0.0)
    {
      if (have_b)
        {
          if (cholB)
            {
              if (permB.numel ())
                {
                  SparseMatrix tmp (n, n, n);
                  for (octave_idx_type i = 0; i < n; i++)
                    {
                      tmp.xcidx (i) = i;
                      tmp.xridx (i) = static_cast<octave_idx_type> (permB(i));
                      tmp.xdata (i) = 1;
                    }
                  tmp.xcidx (n) = n;

                  AminusSigmaB -= sigma * tmp *
                                  b.transpose () * b * tmp.transpose ();
                }
              else
                AminusSigmaB -= sigma * b.transpose () * b;
            }
          else
            AminusSigmaB -= sigma * b;
        }
      else
        {
          SparseMatrix sigmat (n, n, n);

          // Create sigma * speye (n,n)
          sigmat.xcidx (0) = 0;
          for (octave_idx_type i = 0; i < n; i++)
            {
              sigmat.xdata (i) = sigma;
              sigmat.xridx (i) = i;
              sigmat.xcidx (i+1) = i + 1;
            }

          AminusSigmaB -= sigmat;
        }
    }

  octave::math::sparse_lu<SparseMatrix> fact (AminusSigmaB, Matrix (), true);

  L = fact.L ();
  U = fact.U ();
  SparseMatrix R = fact.R ();
  for (octave_idx_type i = 0; i < n; i++)
    r(i) = R.xdata(i);

  const octave_idx_type *P2 = fact.row_perm ();
  const octave_idx_type *Q2 = fact.col_perm ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      P[j] = P2[j];
      Q[j] = Q2[j];
    }

  // Test condition number of LU decomposition
  double minU = octave::numeric_limits<double>::NaN ();
  double maxU = octave::numeric_limits<double>::NaN ();
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = 0.;
      if (U.xcidx (j+1) > U.xcidx (j)
          && U.xridx (U.xcidx (j+1)-1) == j)
        d = std::abs (U.xdata (U.xcidx (j+1)-1));

      if (octave::math::isnan (minU) || d < minU)
        minU = d;

      if (octave::math::isnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || octave::math::isnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const Matrix& m, const Matrix& b,
                bool cholB, const ColumnVector& permB, double sigma,
                Matrix& L, Matrix& U, octave_idx_type *P, octave_idx_type *Q,
                ColumnVector& r)
{
  bool have_b = ! b.isempty ();
  octave_idx_type n = m.cols ();

  // Calculate LU decomposition of 'M = A - sigma * B'
  // P * M = L * U
  Matrix AminusSigmaB (m);

  if (sigma != 0.0)
    {
      if (have_b)
        {
          if (cholB)
            {
              Matrix tmp = sigma * b.transpose () * b;
              const double *pB = permB.data ();
              double *p = AminusSigmaB.fortran_vec ();

              if (permB.numel ())
                {
                  for (octave_idx_type j = 0;
                       j < b.cols (); j++)
                    for (octave_idx_type i = 0;
                         i < b.rows (); i++)
                      *p++ -= tmp.xelem (static_cast<octave_idx_type> (pB[i]),
                                         static_cast<octave_idx_type> (pB[j]));
                }
              else
                AminusSigmaB -= tmp;
            }
          else
            AminusSigmaB -= sigma * b;
        }
      else
        {
          double *p = AminusSigmaB.fortran_vec ();

          for (octave_idx_type i = 0; i < n; i++)
            p[i*(n+1)] -= sigma;
        }
    }

  octave::math::lu<Matrix> fact (AminusSigmaB);

  L = fact.L ();
  U = fact.U ();
  ColumnVector P2 = fact.P_vec();

  for (octave_idx_type j = 0; j < n; j++)
    {
      Q[j] = j;
      P[j] = P2(j) - 1;
      r(j) = 1.;
    }

  // Test condition number of LU decomposition
  double minU = octave::numeric_limits<double>::NaN ();
  double maxU = octave::numeric_limits<double>::NaN ();
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = std::abs (U.xelem (j, j));
      if (octave::math::isnan (minU) || d < minU)
        minU = d;

      if (octave::math::isnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || octave::math::isnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const SparseComplexMatrix& m, const SparseComplexMatrix& b,
                bool cholB, const ColumnVector& permB, Complex sigma,
                SparseComplexMatrix& L, SparseComplexMatrix& U,
                octave_idx_type *P, octave_idx_type *Q, ColumnVector& r)
{
  bool have_b = ! b.isempty ();
  octave_idx_type n = m.rows ();

  // Calculate LU decomposition of 'M = A - sigma * B'
  // P * (R \ M) * Q = L * U
  SparseComplexMatrix AminusSigmaB (m);

  if (std::real (sigma) != 0.0 || std::imag (sigma) != 0.0)
    {
      if (have_b)
        {
          if (cholB)
            {
              if (permB.numel ())
                {
                  SparseMatrix tmp (n, n, n);
                  for (octave_idx_type i = 0; i < n; i++)
                    {
                      tmp.xcidx (i) = i;
                      tmp.xridx (i) = static_cast<octave_idx_type> (permB(i));
                      tmp.xdata (i) = 1;
                    }
                  tmp.xcidx (n) = n;

                  AminusSigmaB -= tmp * b.hermitian () * b *
                                  tmp.transpose () * sigma;
                }
              else
                AminusSigmaB -= sigma * b.hermitian () * b;
            }
          else
            AminusSigmaB -= sigma * b;
        }
      else
        {
          SparseComplexMatrix sigmat (n, n, n);

          // Create sigma * speye (n,n)
          sigmat.xcidx (0) = 0;
          for (octave_idx_type i = 0; i < n; i++)
            {
              sigmat.xdata (i) = sigma;
              sigmat.xridx (i) = i;
              sigmat.xcidx (i+1) = i + 1;
            }

          AminusSigmaB -= sigmat;
        }
    }

  octave::math::sparse_lu<SparseComplexMatrix> fact (AminusSigmaB, Matrix(),
                                                     true);

  L = fact.L ();
  U = fact.U ();
  SparseMatrix R = fact.R ();
  for (octave_idx_type i = 0; i < n; i++)
    r(i) = R.xdata(i);

  const octave_idx_type *P2 = fact.row_perm ();
  const octave_idx_type *Q2 = fact.col_perm ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      P[j] = P2[j];
      Q[j] = Q2[j];
    }

  // Test condition number of LU decomposition
  double minU = octave::numeric_limits<double>::NaN ();
  double maxU = octave::numeric_limits<double>::NaN ();
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = 0.;
      if (U.xcidx (j+1) > U.xcidx (j)
          && U.xridx (U.xcidx (j+1)-1) == j)
        d = std::abs (U.xdata (U.xcidx (j+1)-1));

      if (octave::math::isnan (minU) || d < minU)
        minU = d;

      if (octave::math::isnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || octave::math::isnan (rcond))
    warn_convergence ();

  return true;
}

static bool
LuAminusSigmaB (const ComplexMatrix& m, const ComplexMatrix& b,
                bool cholB, const ColumnVector& permB, Complex sigma,
                ComplexMatrix& L, ComplexMatrix& U, octave_idx_type *P,
                octave_idx_type *Q, ColumnVector& r)
{
  bool have_b = ! b.isempty ();
  octave_idx_type n = m.cols ();

  // Calculate LU decomposition of 'M = A - sigma * B'
  // P * M = L * U
  ComplexMatrix AminusSigmaB (m);

  if (std::real (sigma) != 0.0 || std::imag (sigma) != 0.0)
    {
      if (have_b)
        {
          if (cholB)
            {
              ComplexMatrix tmp = sigma * b.hermitian () * b;
              const double *pB = permB.data ();
              Complex *p = AminusSigmaB.fortran_vec ();

              if (permB.numel ())
                {
                  for (octave_idx_type j = 0;
                       j < b.cols (); j++)
                    for (octave_idx_type i = 0;
                         i < b.rows (); i++)
                      *p++ -= tmp.xelem (static_cast<octave_idx_type> (pB[i]),
                                         static_cast<octave_idx_type> (pB[j]));
                }
              else
                AminusSigmaB -= tmp;
            }
          else
            AminusSigmaB -= sigma * b;
        }
      else
        {
          Complex *p = AminusSigmaB.fortran_vec ();

          for (octave_idx_type i = 0; i < n; i++)
            p[i*(n+1)] -= sigma;
        }
    }

  octave::math::lu<ComplexMatrix> fact (AminusSigmaB);

  L = fact.L ();
  U = fact.U ();
  ColumnVector P2 = fact.P_vec ();

  for (octave_idx_type j = 0; j < n; j++)
    {
      Q[j] = j;
      P[j] = P2(j) - 1;
      r(j) = 1.;
    }

  // Test condition number of LU decomposition
  double minU = octave::numeric_limits<double>::NaN ();
  double maxU = octave::numeric_limits<double>::NaN ();
  for (octave_idx_type j = 0; j < n; j++)
    {
      double d = std::abs (U.xelem (j, j));
      if (octave::math::isnan (minU) || d < minU)
        minU = d;

      if (octave::math::isnan (maxU) || d > maxU)
        maxU = d;
    }

  double rcond = (minU / maxU);
  volatile double rcond_plus_one = rcond + 1.0;

  if (rcond_plus_one == 1.0 || octave::math::isnan (rcond))
    warn_convergence ();

  return true;
}

template <typename M>
octave_idx_type
EigsRealSymmetricMatrix (const M& m, const std::string typ,
                         octave_idx_type k_arg, octave_idx_type p_arg,
                         octave_idx_type& info, Matrix& eig_vec,
                         ColumnVector& eig_val, const M& _b,
                         ColumnVector& permB, ColumnVector& resid,
                         std::ostream& os, double tol, bool rvec,
                         bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 1;
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  double sigma = 0.;
  M bt;

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k < 1 || k > n - 2)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

  if (typ == "LI" || typ == "SI" || typ == "LR" || typ == "SR")
    (*current_liboctave_error_handler)
      ("eigs: invalid sigma value for real symmetric problem");

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (dsaupd, DSAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              Matrix mtmp (n, 1);
              for (F77_INT i = 0; i < n; i++)
                mtmp(i, 0) = workd[i + iptr(0) - 1];

              mtmp = ltsolve (b, permB, m * utsolve (bt, permB, mtmp));

              for (F77_INT i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i, 0);
            }
          else if (! vector_product (m, workd + iptr(0) - 1,
                                     workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dsaupd: %s",
               arpack_errno2str (info, "dsaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
   ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
   F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = octave::numeric_limits<double>::NaN ();
      F77_INT k2 = ip(4) / 2;
      if (typ != "SM" && typ != "BE" && ! (typ == "SA" && rvec))
        {
          for (F77_INT i = 0; i < k2; i++)
            {
              double dtmp = d[i];
              d[i] = d[ip(4) - i - 1];
              d[ip(4) - i - 1] = dtmp;
            }
        }

      if (rvec)
        {
          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = octave::numeric_limits<double>::NaN ();
            }
          if (typ != "SM" && typ != "BE" && typ != "SA")
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (F77_INT i = 0; i < k2; i++)
                {
                  F77_INT off1 = i * n;
                  F77_INT off2 = (ip(4) - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (F77_INT j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (F77_INT j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (F77_INT j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }
            }

          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dseupd: %s",
       arpack_errno2str (info2, "dseupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsRealSymmetricMatrixShift (const M& m, double sigma,
                              octave_idx_type k_arg, octave_idx_type p_arg,
                              octave_idx_type& info, Matrix& eig_vec,
                              ColumnVector& eig_val, const M& _b,
                              ColumnVector& permB, ColumnVector& resid,
                              std::ostream& os, double tol, bool rvec,
                              bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 3;
  bool have_b = ! b.isempty ();
  std::string typ = "LM";

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigma))
  //  return EigsRealSymmetricMatrix (m, "SM", k, p, info, eig_vec, eig_val,
  //                                _b, permB, resid, os, tol, rvec, cholB,
  //                                disp, maxit);

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  M L, U;
  ColumnVector r(n);

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigma, L, U, P, Q, r))
    return -1;

  F77_INT lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (dsaupd, DSAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));
      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  vector_product (b, workd+iptr(0)-1, dtmp);

                  Matrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = dtmp[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  double *ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
              else if (ido == 2)
                vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
              else
                {
                  double *ip2 = workd+iptr(2)-1;
                  Matrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = ip2[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
            }
          else
            {
              // ido cannot be 2 for non-generalized problems (see dsaupd2).
              double *ip2 = workd+iptr(0)-1;
              Matrix tmp (n, 1);

              for (F77_INT i = 0; i < n; i++)
                tmp(i, 0) = ip2[P[i]] / r(P[i]);

              lusolve (L, U, tmp);

              ip2 = workd+iptr(1)-1;
              for (F77_INT i = 0; i < n; i++)
                ip2[Q[i]] = tmp(i, 0);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dsaupd: %s",
               arpack_errno2str (info, "dsaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
   k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, info2
   F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = octave::numeric_limits<double>::NaN ();
      F77_INT k2 = ip(4) / 2;
      for (F77_INT i = 0; i < k2; i++)
        {
          double dtmp = d[i];
          d[i] = d[ip(4) - i - 1];
          d[ip(4) - i - 1] = dtmp;
        }

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (double, dtmp, n);

          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = octave::numeric_limits<double>::NaN ();
            }
          for (F77_INT i = 0; i < k2; i++)
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (ip(4) - i - 1) * n;

              if (off1 == off2)
                continue;

              for (F77_INT j = 0; j < n; j++)
                dtmp[j] = z[off1 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off2 + j] = dtmp[j];
            }
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dseupd: %s",
       arpack_errno2str (info2, "dseupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsRealSymmetricFunc (EigsFunc fcn, octave_idx_type n_arg,
                       const std::string& _typ, double sigma,
                       octave_idx_type k_arg, octave_idx_type p_arg,
                       octave_idx_type& info, Matrix& eig_vec,
                       ColumnVector& eig_val, const M& _b,
                       ColumnVector& permB, ColumnVector& resid,
                       std::ostream& os, double tol, bool rvec,
                       bool cholB, int disp, int maxit)
{
  F77_INT n = octave::to_f77_int (n_arg);
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  std::string typ (_typ);
  bool have_sigma = (sigma ? true : false);
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  F77_INT mode = 1;
  int err = 0;
  M bt;

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (n != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

      if (typ == "LI" || typ == "SI" || typ == "LR" || typ == "SR")
        (*current_liboctave_error_handler)
          ("eigs: invalid sigma value for real symmetric problem");

      if (typ != "SM" && have_b)
        note3 = true;

      if (typ == "SM")
        {
          typ = "LM";
          sigma = 0.;
          mode = 3;
          if (have_b)
            bmat = 'G';
        }
    }
  else if (! std::abs (sigma))
    {
      typ = "SM";
      if (have_b)
        bmat = 'G';
    }
  else
    {
      typ = "LM";
      mode = 3;
      if (have_b)
        bmat = 'G';
    }

  if (mode == 1 && have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = p * (p + 8);

  OCTAVE_LOCAL_BUFFER (double, v, n * p);
  OCTAVE_LOCAL_BUFFER (double, workl, lwork);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (dsaupd, DSAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (mode == 1) // regular mode with factorized B
                {
                  Matrix mtmp (n, 1);
                  for (F77_INT i = 0; i < n; i++)
                    mtmp(i, 0) = workd[i + iptr(0) - 1];

                  mtmp = utsolve (bt, permB, mtmp);
                  ColumnVector y = fcn (mtmp, err);

                  if (err)
                    return false;

                  mtmp = ltsolve (b, permB, y);

                  for (F77_INT i = 0; i < n; i++)
                    workd[i+iptr(1)-1] = mtmp(i, 0);
                }
              else // shift-invert mode
                {
                  if (ido == -1)
                    {
                      OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                      vector_product (b, workd+iptr(0)-1, dtmp);

                      ColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = dtmp[i];

                      ColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      double *ip2 = workd + iptr(1) - 1;
                      for (F77_INT i = 0; i < n; i++)
                        ip2[i] = y(i);
                    }
                  else if (ido == 2)
                    vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
                  else
                    {
                      double *ip2 = workd+iptr(2)-1;
                      ColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = *ip2++;

                      ColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      ip2 = workd + iptr(1) - 1;
                      for (F77_INT i = 0; i < n; i++)
                        *ip2++ = y(i);
                    }
                }
            }
          else
            {
              double *ip2 = workd + iptr(0) - 1;
              ColumnVector x(n);

              for (F77_INT i = 0; i < n; i++)
                x(i) = *ip2++;

              ColumnVector y = fcn (x, err);

              if (err)
                return false;

              ip2 = workd + iptr(1) - 1;
              for (F77_INT i = 0; i < n; i++)
                *ip2++ = y(i);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dsaupd: %s",
               arpack_errno2str (info, "dsaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  double *z = eig_vec.fortran_vec ();

  eig_val.resize (k);
  double *d = eig_val.fortran_vec ();

  F77_FUNC (dseupd, DSEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, d, z, n, sigma,
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
   k, tol, presid, p, v, n, iparam, ipntr, workd, workl, lwork, info2
   F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = octave::numeric_limits<double>::NaN ();
      F77_INT k2 = ip(4) / 2;
      if (mode == 3 || (mode == 1 && typ != "SM" && typ != "BE"))
        {
          for (F77_INT i = 0; i < k2; i++)
            {
              double dtmp = d[i];
              d[i] = d[ip(4) - i - 1];
              d[ip(4) - i - 1] = dtmp;
            }
        }

      if (rvec)
        {
          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = octave::numeric_limits<double>::NaN ();
            }
          if (mode == 3 || (mode == 1 && typ != "SM" && typ != "BE"))
            {
              OCTAVE_LOCAL_BUFFER (double, dtmp, n);

              for (F77_INT i = 0; i < k2; i++)
                {
                  F77_INT off1 = i * n;
                  F77_INT off2 = (ip(4) - i - 1) * n;

                  if (off1 == off2)
                    continue;

                  for (F77_INT j = 0; j < n; j++)
                    dtmp[j] = z[off1 + j];

                  for (F77_INT j = 0; j < n; j++)
                    z[off1 + j] = z[off2 + j];

                  for (F77_INT j = 0; j < n; j++)
                    z[off2 + j] = dtmp[j];
                }
            }
          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dseupd: %s",
       arpack_errno2str (info2, "dseupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsRealNonSymmetricMatrix (const M& m, const std::string typ,
                            octave_idx_type k_arg, octave_idx_type p_arg,
                            octave_idx_type& info, ComplexMatrix& eig_vec,
                            ComplexColumnVector& eig_val, const M& _b,
                            ColumnVector& permB, ColumnVector& resid,
                            std::ostream& os, double tol, bool rvec,
                            bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 1;
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  double sigmar = 0.;
  double sigmai = 0.;
  M bt;

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

  if (typ == "LA" || typ == "SA" || typ == "BE")
    (*current_liboctave_error_handler)
      ("eigs: invalid sigma value for unsymmetric problem");

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      // On exit, ip(4) <= k + 1 is the number of converged eigenvalues.
      // See dnaupd2.
      F77_FUNC (dnaupd, DNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));
      // k is not changed

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  os << "    " << workl[iptr(5)+k] << "\n";
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k - 1; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              Matrix mtmp (n, 1);
              for (F77_INT i = 0; i < n; i++)
                mtmp(i, 0) = workd[i + iptr(0) - 1];

              mtmp = ltsolve (b, permB, m * utsolve (bt, permB, mtmp));

              for (F77_INT i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i, 0);
            }
          else if (! vector_product (m, workd + iptr(0) - 1,
                                     workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dnaupd: %s",
               arpack_errno2str (info, "dnaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (F77_INT i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_INT k0 = k;  // original number of eigenvalues required
  F77_FUNC (dneupd, DNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
   sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
   ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
   F77_CHAR_ARG_LEN(2));
  // on exit, if (and only if) rvec == true, k may have been increased by one
  // and be equal to ip(4), see dngets.

  if (! rvec && ip(4) > k)
    k = ip(4);

  eig_val.resize (k);
  Complex *d = eig_val.fortran_vec ();

  if (info2 == 0)
    {
      bool have_cplx_eig = false;
      for (F77_INT i = 0; i < ip(4); i++)
        {
          if (di[i] == 0)
            d[i] = Complex (dr[i], 0.);
          else
            {
              have_cplx_eig = true;
              d[i] = Complex (dr[i], di[i]);
            }
        }
      if (have_cplx_eig)
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (),
                            octave::numeric_limits<double>::NaN ());
        }
      else
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (), 0.);
        }
      if (! rvec)
        {
          // ARPACK seems to give the eigenvalues in reversed order
          F77_INT k2 = ip(4) / 2;
          for (F77_INT i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[ip(4) - i - 1];
              d[ip(4) - i - 1] = dtmp;
            }
        }
      else
        {
          // When eigenvectors required, ARPACK seems to give the right order
          eig_vec.resize (n, k);
          F77_INT i = 0;
          while (i < ip(4))
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (i+1) * n;
              if (std::imag (eig_val(i)) == 0)
                {
                  for (F77_INT j = 0; j < n; j++)
                    eig_vec(j, i) = Complex (z[j+off1], 0.);
                  i++;
                }
              else
                {
                  for (F77_INT j = 0; j < n; j++)
                    {
                      eig_vec(j, i) = Complex (z[j+off1], z[j+off2]);
                      if (i < ip(4) - 1)
                        eig_vec(j, i+1) = Complex (z[j+off1], -z[j+off2]);
                    }
                  i+=2;
                }
            }
          if (have_cplx_eig)
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (),
                               octave::numeric_limits<double>::NaN ());
            }
          else
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (), 0.);
            }
          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
      if (k0 < k)
        {
          eig_val.resize (k0);
          eig_vec.resize (n, k0);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dneupd: %s",
       arpack_errno2str (info2, "dneupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsRealNonSymmetricMatrixShift (const M& m, double sigmar,
                                 octave_idx_type k_arg, octave_idx_type p_arg,
                                 octave_idx_type& info,
                                 ComplexMatrix& eig_vec,
                                 ComplexColumnVector& eig_val, const M& _b,
                                 ColumnVector& permB, ColumnVector& resid,
                                 std::ostream& os, double tol, bool rvec,
                                 bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 3;
  bool have_b = ! b.isempty ();
  std::string typ = "LM";
  double sigmai = 0.;

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigmar))
  //  return EigsRealNonSymmetricMatrix (m, "SM", k, p, info, eig_vec, eig_val,
  //                                   _b, permB, resid, os, tol, rvec, cholB,
  //                                   disp, maxit);

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check that we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  M L, U;
  ColumnVector r(n);

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigmar, L, U, P, Q, r))
    return -1;

  F77_INT lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      // On exit, ip(4) <= k + 1 is the number of converged eigenvalues.
      // See dnaupd2.
      F77_FUNC (dnaupd, DNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));
      // k is not changed

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  os << "    " << workl[iptr(5)+k] << "\n";
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k - 1; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                  vector_product (b, workd+iptr(0)-1, dtmp);

                  Matrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = dtmp[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  double *ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
              else if (ido == 2)
                vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
              else
                {
                  double *ip2 = workd+iptr(2)-1;
                  Matrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = ip2[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
            }
          else
            {
              // ido cannot be 2 for non-generalized problems (see dnaupd2).
              double *ip2 = workd+iptr(0)-1;
              Matrix tmp (n, 1);

              for (F77_INT i = 0; i < n; i++)
                tmp(i, 0) = ip2[P[i]] / r(P[i]);

              lusolve (L, U, tmp);

              ip2 = workd+iptr(1)-1;
              for (F77_INT i = 0; i < n; i++)
                ip2[Q[i]] = tmp(i, 0);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dnaupd: %s",
               arpack_errno2str (info, "dnaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (F77_INT i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_INT k0 = k;  // original number of eigenvalues required
  F77_FUNC (dneupd, DNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
   sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
   ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
   F77_CHAR_ARG_LEN(2));
  // On exit, if (and only if) rvec == true, k may have been increased by one
  // and be equal to ip(4), see dngets.

  if (! rvec && ip(4) > k)
    k = ip(4);

  eig_val.resize (k);
  Complex *d = eig_val.fortran_vec ();

  if (info2 == 0)
    {
      bool have_cplx_eig = false;
      for (F77_INT i = 0; i < ip(4); i++)
        {
          if (di[i] == 0.)
            d[i] = Complex (dr[i], 0.);
          else
            {
              have_cplx_eig = true;
              d[i] = Complex (dr[i], di[i]);
            }
        }
      if (have_cplx_eig)
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (),
                            octave::numeric_limits<double>::NaN ());
        }
      else
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (), 0.);
        }

      if (! rvec)
        {
          // ARPACK seems to give the eigenvalues in reversed order
          F77_INT k2 = ip(4) / 2;
          for (F77_INT i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[ip(4) - i - 1];
              d[ip(4) - i - 1] = dtmp;
            }
        }
      else
        {
          // When eigenvectors required, ARPACK seems to give the right order
          eig_vec.resize (n, k);
          F77_INT i = 0;
          while (i < ip(4))
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (i+1) * n;
              if (std::imag (eig_val(i)) == 0)
                {
                  for (F77_INT j = 0; j < n; j++)
                    eig_vec(j, i) = Complex (z[j+off1], 0.);
                  i++;
                }
              else
                {
                  for (F77_INT j = 0; j < n; j++)
                    {
                      eig_vec(j, i) = Complex (z[j+off1], z[j+off2]);
                      if (i < ip(4) - 1)
                        eig_vec(j, i+1) = Complex (z[j+off1], -z[j+off2]);
                    }
                  i+=2;
                }
            }
          if (have_cplx_eig)
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (),
                               octave::numeric_limits<double>::NaN ());
            }
          else
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (), 0.);
            }
        }
      if (k0 < k)
        {
          eig_val.resize (k0);
          eig_vec.resize (n, k0);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dneupd: %s",
       arpack_errno2str (info2, "dneupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsRealNonSymmetricFunc (EigsFunc fcn, octave_idx_type n_arg,
                          const std::string& _typ, double sigmar,
                          octave_idx_type k_arg, octave_idx_type p_arg,
                          octave_idx_type& info, ComplexMatrix& eig_vec,
                          ComplexColumnVector& eig_val, const M& _b,
                          ColumnVector& permB, ColumnVector& resid,
                          std::ostream& os, double tol, bool rvec,
                          bool cholB, int disp, int maxit)
{
  F77_INT n = octave::to_f77_int (n_arg);
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  std::string typ (_typ);
  bool have_sigma = (sigmar ? true : false);
  double sigmai = 0.;
  F77_INT mode = 1;
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  int err = 0;
  M bt;

  if (resid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      resid = ColumnVector (octave::rand::vector (n));
      octave::rand::distribution (rand_dist);
    }
  else if (n != resid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

      if (typ == "LA" || typ == "SA" || typ == "BE")
        (*current_liboctave_error_handler)
          ("eigs: invalid sigma value for unsymmetric problem");

      if (typ != "SM" && have_b)
        note3 = true;

      if (typ == "SM")
        {
          typ = "LM";
          sigmar = 0.;
          mode = 3;
          if (have_b)
            bmat = 'G';
        }
    }
  else if (! std::abs (sigmar))
    {
      typ = "SM";
      if (have_b)
        bmat = 'G';
    }
  else
    {
      typ = "LM";
      mode = 3;
      if (have_b)
        bmat = 'G';
    }

  if (mode == 1 && have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.transpose ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = 3 * p * (p + 2);

  OCTAVE_LOCAL_BUFFER (double, v, n * (p + 1));
  OCTAVE_LOCAL_BUFFER (double, workl, lwork + 1);
  OCTAVE_LOCAL_BUFFER (double, workd, 3 * n + 1);
  double *presid = resid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      // On exit, ip(4) <= k + 1 is the number of converged eigenvalues
      // see dnaupd2.
      F77_FUNC (dnaupd, DNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, presid, p, v, n, iparam,
       ipntr, workd, workl, lwork, tmp_info
       F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));
      // k is not changed

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  os << "    " << workl[iptr(5)+k] << "\n";
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k - 1; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (mode == 1) // regular mode with factorized B
                {
                  Matrix mtmp (n, 1);
                  for (F77_INT i = 0; i < n; i++)
                    mtmp(i, 0) = workd[i + iptr(0) - 1];

                  mtmp = utsolve (bt, permB, mtmp);
                  ColumnVector y = fcn (mtmp, err);

                  if (err)
                    return false;

                  mtmp = ltsolve (b, permB, y);

                  for (F77_INT i = 0; i < n; i++)
                    workd[i+iptr(1)-1] = mtmp(i, 0);
                }
              else // shift-invert mode
                {
                  if (ido == -1)
                    {
                      OCTAVE_LOCAL_BUFFER (double, dtmp, n);

                      vector_product (b, workd+iptr(0)-1, dtmp);

                      ColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = dtmp[i];

                      ColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      double *ip2 = workd + iptr(1) - 1;
                      for (F77_INT i = 0; i < n; i++)
                        ip2[i] = y(i);
                    }
                  else if (ido == 2)
                    vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
                  else
                    {
                      double *ip2 = workd+iptr(2)-1;
                      ColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = *ip2++;

                      ColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      ip2 = workd + iptr(1) - 1;
                      for (F77_INT i = 0; i < n; i++)
                        *ip2++ = y(i);
                    }
                }
            }
          else
            {
              double *ip2 = workd + iptr(0) - 1;
              ColumnVector x(n);

              for (F77_INT i = 0; i < n; i++)
                x(i) = *ip2++;

              ColumnVector y = fcn (x, err);

              if (err)
                return false;

              ip2 = workd + iptr(1) - 1;
              for (F77_INT i = 0; i < n; i++)
                *ip2++ = y(i);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in dnaupd: %s",
               arpack_errno2str (info, "dnaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  // FIXME: initialize eig_vec2 to zero; apparently dneupd can skip
  // the assignment to elements of Z that represent imaginary parts.
  // Found with valgrind and
  //
  //   A = [1,0,0,-1;0,1,0,0;0,0,1,0;0,0,2,1];
  //   [vecs, vals, f] = eigs (A, 1)

  Matrix eig_vec2 (n, k + 1, 0.0);
  double *z = eig_vec2.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, dr, k + 1);
  OCTAVE_LOCAL_BUFFER (double, di, k + 1);
  OCTAVE_LOCAL_BUFFER (double, workev, 3 * p);
  for (F77_INT i = 0; i < k+1; i++)
    dr[i] = di[i] = 0.;

  F77_INT k0 = k;  // original number of eigenvalues required
  F77_FUNC (dneupd, DNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, dr, di, z, n, sigmar,
   sigmai, workev,  F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2), k, tol, presid, p, v, n, iparam,
   ipntr, workd, workl, lwork, info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1)
   F77_CHAR_ARG_LEN(2));
  // On exit, if (and only if) rvec == true, k may have been increased by one
  // and be equal to ip(4), see dngets.

  if (! rvec && ip(4) > k)
    k = ip(4);

  eig_val.resize (k);
  Complex *d = eig_val.fortran_vec ();

  if (info2 == 0)
    {
      bool have_cplx_eig = false;
      for (F77_INT i = 0; i < ip(4); i++)
        {
          if (di[i] == 0.)
            d[i] = Complex (dr[i], 0.);
          else
            {
              have_cplx_eig = true;
              d[i] = Complex (dr[i], di[i]);
            }
        }
      if (have_cplx_eig)
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (),
                            octave::numeric_limits<double>::NaN ());
        }
      else
        {
          for (F77_INT i = ip(4); i < k; i++)
            d[i] = Complex (octave::numeric_limits<double>::NaN (), 0.);
        }

      if (! rvec)
        {
          // ARPACK seems to give the eigenvalues in reversed order
          octave_idx_type k2 = ip(4) / 2;
          for (F77_INT i = 0; i < k2; i++)
            {
              Complex dtmp = d[i];
              d[i] = d[ip(4) - i - 1];
              d[ip(4) - i - 1] = dtmp;
            }
        }
      else
        {
          // ARPACK seems to give the eigenvalues in reversed order
          eig_vec.resize (n, k);
          F77_INT i = 0;
          while (i < ip(4))
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (i+1) * n;
              if (std::imag (eig_val(i)) == 0)
                {
                  for (F77_INT j = 0; j < n; j++)
                    eig_vec(j, i) = Complex (z[j+off1], 0.);
                  i++;
                }
              else
                {
                  for (F77_INT j = 0; j < n; j++)
                    {
                      eig_vec(j, i) = Complex (z[j+off1], z[j+off2]);
                      if (i < ip(4) - 1)
                        eig_vec(j, i+1) = Complex (z[j+off1], -z[j+off2]);
                    }
                  i+=2;
                }
            }
          if (have_cplx_eig)
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (),
                               octave::numeric_limits<double>::NaN ());
            }
          else
            {
              for (F77_INT ii = ip(4); ii < k; ii++)
                for (F77_INT jj = 0; jj < n; jj++)
                  eig_vec(jj, ii)
                    = Complex (octave::numeric_limits<double>::NaN (), 0.);
            }
          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
      if (k0 < k)
        {
          eig_val.resize (k0);
          eig_vec.resize (n, k0);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in dneupd: %s",
       arpack_errno2str (info2, "dneupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsComplexNonSymmetricMatrix (const M& m, const std::string typ,
                               octave_idx_type k_arg, octave_idx_type p_arg,
                               octave_idx_type& info, ComplexMatrix& eig_vec,
                               ComplexColumnVector& eig_val, const M& _b,
                               ColumnVector& permB,
                               ComplexColumnVector& cresid,
                               std::ostream& os, double tol, bool rvec,
                               bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 1;
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  Complex sigma = 0.;
  M bt;

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  if (cresid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      Array<double> rr (octave::rand::vector (n));
      Array<double> ri (octave::rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (F77_INT i = 0; i < n; i++)
        cresid(i) = Complex (rr(i), ri(i));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != cresid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
      && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
      && typ != "SI")
    (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

  if (typ == "LA" || typ == "SA" || typ == "BE")
    (*current_liboctave_error_handler)
      ("eigs: invalid sigma value for complex problem");

  if (have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.hermitian ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (znaupd, ZNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 (typ.c_str (), 2),
       k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
       iparam, ipntr,
       F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
       tmp_info F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan (workl[iptr (5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              ComplexMatrix mtmp (n, 1);
              for (F77_INT i = 0; i < n; i++)
                mtmp(i, 0) = workd[i + iptr(0) - 1];
              mtmp = ltsolve (b, permB, m * utsolve (bt, permB, mtmp));
              for (F77_INT i = 0; i < n; i++)
                workd[i+iptr(1)-1] = mtmp(i, 0);

            }
          else if (! vector_product (m, workd + iptr(0) - 1,
                                     workd + iptr(1) - 1))
            break;
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in znaupd: %s",
               arpack_errno2str (info, "znaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, F77_DBLE_CMPLX_ARG (d),
   F77_DBLE_CMPLX_ARG (z), n, F77_CONST_DBLE_CMPLX_ARG (&sigma),
   F77_DBLE_CMPLX_ARG (workev),
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
   k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
   iparam, ipntr,
   F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
   info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = Complex (octave::numeric_limits<double>::NaN (),
                        octave::numeric_limits<double>::NaN ());

      F77_INT k2 = ip(4) / 2;
      for (F77_INT i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[ip(4) - i - 1];
          d[ip(4) - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = Complex (octave::numeric_limits<double>::NaN (),
                                       octave::numeric_limits<double>::NaN ());
            }

          for (F77_INT i = 0; i < k2; i++)
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (ip(4) - i - 1) * n;

              if (off1 == off2)
                continue;

              for (F77_INT j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }

          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in zneupd: %s",
       arpack_errno2str (info2, "zneupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsComplexNonSymmetricMatrixShift (const M& m, Complex sigma,
                                    octave_idx_type k_arg, octave_idx_type p_arg,
                                    octave_idx_type& info,
                                    ComplexMatrix& eig_vec,
                                    ComplexColumnVector& eig_val, const M& _b,
                                    ColumnVector& permB,
                                    ComplexColumnVector& cresid,
                                    std::ostream& os, double tol, bool rvec,
                                    bool cholB, int disp, int maxit)
{
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  F77_INT n = octave::to_f77_int (m.cols ());
  F77_INT mode = 3;
  bool have_b = ! b.isempty ();
  std::string typ = "LM";

  if (m.rows () != m.cols ())
    (*current_liboctave_error_handler) ("eigs: A must be square");
  if (have_b && (m.rows () != b.rows () || m.rows () != b.cols ()))
    (*current_liboctave_error_handler)
      ("eigs: B must be square and the same size as A");

  // FIXME: The "SM" type for mode 1 seems unstable though faster!!
  //if (! std::abs (sigma))
  //  return EigsComplexNonSymmetricMatrix (m, "SM", k, p, info, eig_vec,
  //                                      eig_val, _b, permB, cresid, os, tol,
  //                                      rvec, cholB, disp, maxit);

  if (cresid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      Array<double> rr (octave::rand::vector (n));
      Array<double> ri (octave::rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (F77_INT i = 0; i < n; i++)
        cresid(i) = Complex (rr(i), ri(i));
      octave::rand::distribution (rand_dist);
    }
  else if (m.cols () != cresid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check that we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  char bmat = 'I';
  if (have_b)
    bmat = 'G';

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  M L, U;
  ColumnVector r(n);

  OCTAVE_LOCAL_BUFFER (octave_idx_type, P, (have_b ? b.rows () : m.rows ()));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Q, (have_b ? b.cols () : m.cols ()));

  if (! LuAminusSigmaB (m, b, cholB, permB, sigma, L, U, P, Q, r))
    return -1;

  F77_INT lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (znaupd, ZNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
       iparam, ipntr,
       F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
       tmp_info F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (ido == -1)
                {
                  OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

                  vector_product (b, workd+iptr(0)-1, ctmp);

                  ComplexMatrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = ctmp[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  Complex *ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
              else if (ido == 2)
                vector_product (b, workd + iptr(0) - 1, workd + iptr(1) - 1);
              else
                {
                  Complex *ip2 = workd+iptr(2)-1;
                  ComplexMatrix tmp (n, 1);

                  for (F77_INT i = 0; i < n; i++)
                    tmp(i, 0) = ip2[P[i]] / r(P[i]);

                  lusolve (L, U, tmp);

                  ip2 = workd+iptr(1)-1;
                  for (F77_INT i = 0; i < n; i++)
                    ip2[Q[i]] = tmp(i, 0);
                }
            }
          else
            {
              // ido cannot be 2 for non-generalized problems (see znaup2).
              Complex *ip2 = workd+iptr(0)-1;
              ComplexMatrix tmp (n, 1);

              for (F77_INT i = 0; i < n; i++)
                tmp(i, 0) = ip2[P[i]] / r(P[i]);

              lusolve (L, U, tmp);

              ip2 = workd+iptr(1)-1;
              for (F77_INT i = 0; i < n; i++)
                ip2[Q[i]] = tmp(i, 0);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in znaupd: %s",
               arpack_errno2str (info, "znaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, F77_DBLE_CMPLX_ARG (d),
   F77_DBLE_CMPLX_ARG (z), n, F77_CONST_DBLE_CMPLX_ARG (&sigma),
   F77_DBLE_CMPLX_ARG (workev),
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
   k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
   iparam, ipntr,
   F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
   info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = Complex (octave::numeric_limits<double>::NaN (),
                        octave::numeric_limits<double>::NaN ());

      F77_INT k2 = ip(4) / 2;
      for (F77_INT i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[ip(4) - i - 1];
          d[ip(4) - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = Complex (octave::numeric_limits<double>::NaN (),
                                       octave::numeric_limits<double>::NaN ());
            }

          for (F77_INT i = 0; i < k2; i++)
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (ip(4) - i - 1) * n;

              if (off1 == off2)
                continue;

              for (F77_INT j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in zneupd: %s",
       arpack_errno2str (info2, "zneupd").c_str ());

  return ip(4);
}

template <typename M>
octave_idx_type
EigsComplexNonSymmetricFunc (EigsComplexFunc fcn, octave_idx_type n_arg,
                             const std::string& _typ, Complex sigma,
                             octave_idx_type k_arg, octave_idx_type p_arg,
                             octave_idx_type& info, ComplexMatrix& eig_vec,
                             ComplexColumnVector& eig_val, const M& _b,
                             ColumnVector& permB, ComplexColumnVector& cresid,
                             std::ostream& os, double tol, bool rvec,
                             bool cholB, int disp, int maxit)
{
  F77_INT n = octave::to_f77_int (n_arg);
  F77_INT k = octave::to_f77_int (k_arg);
  F77_INT p = octave::to_f77_int (p_arg);
  M b(_b);
  std::string typ (_typ);
  bool have_sigma = (std::abs (sigma) ? true : false);
  F77_INT mode = 1;
  bool have_b = ! b.isempty ();
  bool note3 = false;
  char bmat = 'I';
  int err = 0;
  M bt;

  if (cresid.isempty ())
    {
      std::string rand_dist = octave::rand::distribution ();
      octave::rand::distribution ("uniform");
      Array<double> rr (octave::rand::vector (n));
      Array<double> ri (octave::rand::vector (n));
      cresid = ComplexColumnVector (n);
      for (F77_INT i = 0; i < n; i++)
        cresid(i) = Complex (rr(i), ri(i));
      octave::rand::distribution (rand_dist);
    }
  else if (n != cresid.numel ())
    (*current_liboctave_error_handler) ("eigs: opts.v0 must be n-by-1");

  if (n < 3)
    (*current_liboctave_error_handler) ("eigs: n must be at least 3");

  if (p < 0)
    {
      p = k * 2 + 1;

      if (p < 20)
        p = 20;

      if (p > n)
        p = n;
    }

  if (k <= 0 || k >= n - 1)
    (*current_liboctave_error_handler)
      ("eigs: Invalid number of eigenvalues to extract"
       " (must be 0 < k < n-1).\n"
       "      Use 'eig (full (A))' instead");

  if (p <= k || p > n)
    (*current_liboctave_error_handler)
      ("eigs: opts.p must be greater than k and less than or equal to n");

  if (have_b && cholB && ! permB.isempty ())
    {
      // Check the we really have a permutation vector
      if (permB.numel () != n)
        (*current_liboctave_error_handler) ("eigs: permB vector invalid");

      Array<bool> checked (dim_vector (n, 1), false);
      for (F77_INT i = 0; i < n; i++)
        {
          octave_idx_type bidx = static_cast<octave_idx_type> (permB(i));

          if (checked(bidx) || bidx < 0 || bidx >= n
              || octave::math::x_nint (bidx) != bidx)
            (*current_liboctave_error_handler) ("eigs: permB vector invalid");
        }
    }

  if (! have_sigma)
    {
      if (typ != "LM" && typ != "SM" && typ != "LA" && typ != "SA"
          && typ != "BE" && typ != "LR" && typ != "SR" && typ != "LI"
          && typ != "SI")
        (*current_liboctave_error_handler) ("eigs: unrecognized sigma value");

      if (typ == "LA" || typ == "SA" || typ == "BE")
        (*current_liboctave_error_handler)
          ("eigs: invalid sigma value for complex problem");

      if (typ != "SM" && have_b)
        note3 = true;

      if (typ == "SM")
        {
          typ = "LM";
          sigma = 0.;
          mode = 3;
          if (have_b)
            bmat ='G';
        }
    }
  else if (! std::abs (sigma))
    {
      typ = "SM";
      if (have_b)
        bmat = 'G';
    }
  else
    {
      typ = "LM";
      mode = 3;
      if (have_b)
        bmat = 'G';
    }

  if (mode == 1 && have_b)
    {
      // See Note 3 dsaupd
      note3 = true;
      if (cholB)
        {
          bt = b;
          b = b.hermitian ();
          if (permB.isempty ())
            {
              permB = ColumnVector (n);
              for (F77_INT i = 0; i < n; i++)
                permB(i) = i;
            }
        }
      else
        {
          if (! make_cholb (b, bt, permB))
            (*current_liboctave_error_handler)
              ("eigs: The matrix B is not positive definite");
        }
    }

  Array<F77_INT> ip (dim_vector (11, 1));
  F77_INT *iparam = ip.fortran_vec ();

  ip(0) = 1; //ishift
  ip(1) = 0;   // ip(1) not referenced
  ip(2) = maxit; // mxiter, maximum number of iterations
  ip(3) = 1; // NB blocksize in recurrence
  ip(4) = 0; // nconv, number of Ritz values that satisfy convergence
  ip(5) = 0; //ip(5) not referenced
  ip(6) = mode; // mode
  ip(7) = 0;
  ip(8) = 0;
  ip(9) = 0;
  ip(10) = 0;
  // ip(7) to ip(10) return values

  Array<F77_INT> iptr (dim_vector (14, 1));
  F77_INT *ipntr = iptr.fortran_vec ();

  F77_INT ido = 0;
  int iter = 0;
  F77_INT lwork = p * (3 * p + 5);

  OCTAVE_LOCAL_BUFFER (Complex, v, n * p);
  OCTAVE_LOCAL_BUFFER (Complex, workl, lwork);
  OCTAVE_LOCAL_BUFFER (Complex, workd, 3 * n);
  OCTAVE_LOCAL_BUFFER (double, rwork, p);
  Complex *presid = cresid.fortran_vec ();

  do
    {
      F77_INT tmp_info = octave::to_f77_int (info);

      F77_FUNC (znaupd, ZNAUPD)
      (ido, F77_CONST_CHAR_ARG2 (&bmat, 1), n,
       F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
       k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
       iparam, ipntr,
       F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
       tmp_info F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

      info = tmp_info;

      if (disp > 0 && ! octave::math::isnan(workl[iptr(5)-1]))
        {
          if (iter++)
            {
              os << "Iteration " << iter - 1 <<
                 ": a few Ritz values of the " << p << "-by-" <<
                 p << " matrix\n";
              if (ido == 99) // convergence
                {
                  for (F77_INT i = 0; i < k; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
              else
                {
                  // the wanted Ritz estimates are at the end
                  for (F77_INT i = p - k; i < p; i++)
                    os << "    " << workl[iptr(5)+i-1] << "\n";
                }
            }

          // This is a kludge, as ARPACK doesn't give its
          // iteration pointer.  But as workl[iptr(5)-1] is
          // an output value updated at each iteration, setting
          // a value in this array to NaN and testing for it
          // is a way of obtaining the iteration counter.
          if (ido != 99)
            workl[iptr(5)-1] = octave::numeric_limits<double>::NaN ();
        }

      if (ido == -1 || ido == 1 || ido == 2)
        {
          if (have_b)
            {
              if (mode == 1) // regular mode with factorized B
                {
                  ComplexMatrix mtmp (n, 1);
                  for (F77_INT i = 0; i < n; i++)
                    mtmp(i, 0) = workd[i + iptr(0) - 1];

                  mtmp = utsolve (bt, permB, mtmp);
                  ComplexColumnVector y = fcn (mtmp, err);

                  if (err)
                    return false;

                  mtmp = ltsolve (b, permB, y);

                  for (F77_INT i = 0; i < n; i++)
                    workd[i+iptr(1)-1] = mtmp(i, 0);
                }
              else // shift-invert mode
                {
                  if (ido == -1)
                    {
                      OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

                      vector_product (b, workd+iptr(0)-1, ctmp);

                      ComplexColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = ctmp[i];

                      ComplexColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      Complex *ip2 = workd+iptr(1)-1;
                      for (F77_INT i = 0; i < n; i++)
                        ip2[i] = y(i);
                    }
                  else if (ido == 2)
                    vector_product (b, workd+iptr(0)-1, workd+iptr(1)-1);
                  else
                    {
                      Complex *ip2 = workd+iptr(2)-1;
                      ComplexColumnVector x(n);

                      for (F77_INT i = 0; i < n; i++)
                        x(i) = *ip2++;

                      ComplexColumnVector y = fcn (x, err);

                      if (err)
                        return false;

                      ip2 = workd + iptr(1) - 1;
                      for (F77_INT i = 0; i < n; i++)
                        *ip2++ = y(i);
                    }
                }
            }
          else
            {
              Complex *ip2 = workd + iptr(0) - 1;
              ComplexColumnVector x(n);

              for (F77_INT i = 0; i < n; i++)
                x(i) = *ip2++;

              ComplexColumnVector y = fcn (x, err);

              if (err)
                return false;

              ip2 = workd + iptr(1) - 1;
              for (F77_INT i = 0; i < n; i++)
                *ip2++ = y(i);
            }
        }
      else
        {
          if (info < 0)
            (*current_liboctave_error_handler)
              ("eigs: error in znaupd: %s",
               arpack_errno2str (info, "znaupd").c_str ());

          break;
        }
    }
  while (1);

  F77_INT info2;

  // We have a problem in that the size of the C++ bool
  // type relative to the fortran logical type.  It appears
  // that fortran uses 4- or 8-bytes per logical and C++ 1-byte
  // per bool, though this might be system dependent.  As
  // long as the HOWMNY arg is not "S", the logical array
  // is just workspace for ARPACK, so use int type to
  // avoid problems.
  Array<F77_INT> s (dim_vector (p, 1));
  F77_INT *sel = s.fortran_vec ();

  eig_vec.resize (n, k);
  Complex *z = eig_vec.fortran_vec ();

  eig_val.resize (k+1);
  Complex *d = eig_val.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, workev, 2 * p);

  F77_FUNC (zneupd, ZNEUPD)
  (rvec, F77_CONST_CHAR_ARG2 ("A", 1), sel, F77_DBLE_CMPLX_ARG (d),
   F77_DBLE_CMPLX_ARG (z), n, F77_DBLE_CMPLX_ARG (&sigma),
   F77_DBLE_CMPLX_ARG (workev),
   F77_CONST_CHAR_ARG2 (&bmat, 1), n,
   F77_CONST_CHAR_ARG2 ((typ.c_str ()), 2),
   k, tol, F77_DBLE_CMPLX_ARG (presid), p, F77_DBLE_CMPLX_ARG (v), n,
   iparam, ipntr,
   F77_DBLE_CMPLX_ARG (workd), F77_DBLE_CMPLX_ARG (workl), lwork, rwork,
   info2 F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(1) F77_CHAR_ARG_LEN(2));

  if (info2 == 0)
    {
      for (F77_INT i = ip(4); i < k; i++)
        d[i] = Complex (octave::numeric_limits<double>::NaN (),
                        octave::numeric_limits<double>::NaN ());

      F77_INT k2 = ip(4) / 2;
      for (F77_INT i = 0; i < k2; i++)
        {
          Complex ctmp = d[i];
          d[i] = d[ip(4) - i - 1];
          d[ip(4) - i - 1] = ctmp;
        }
      eig_val.resize (k);

      if (rvec)
        {
          OCTAVE_LOCAL_BUFFER (Complex, ctmp, n);

          for (F77_INT i = ip(4); i < k; i++)
            {
              F77_INT off1 = i * n;
              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = Complex (octave::numeric_limits<double>::NaN (),
                                       octave::numeric_limits<double>::NaN ());
            }

          for (F77_INT i = 0; i < k2; i++)
            {
              F77_INT off1 = i * n;
              F77_INT off2 = (ip(4) - i - 1) * n;

              if (off1 == off2)
                continue;

              for (F77_INT j = 0; j < n; j++)
                ctmp[j] = z[off1 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off1 + j] = z[off2 + j];

              for (F77_INT j = 0; j < n; j++)
                z[off2 + j] = ctmp[j];
            }
          if (note3)
            eig_vec = utsolve (bt, permB, eig_vec);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("eigs: error in zneupd: %s",
       arpack_errno2str (info2, "zneupd").c_str ());

  return ip(4);
}

// Instantiations for the types we need.

// Matrix

template
OCTAVE_API octave_idx_type
EigsRealSymmetricMatrix<Matrix>
  (const Matrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, Matrix& eig_vec,
   ColumnVector& eig_val, const Matrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealSymmetricMatrixShift<Matrix>
  (const Matrix& m, double sigma, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, Matrix& eig_vec,
   ColumnVector& eig_val, const Matrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealSymmetricFunc<Matrix>
(EigsFunc fcn, octave_idx_type n, const std::string& _typ, double sigma,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   Matrix& eig_vec, ColumnVector& eig_val, const Matrix& _b,
   ColumnVector& permB, ColumnVector& resid, std::ostream& os, double tol,
   bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricMatrix<Matrix>
  (const Matrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const Matrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricMatrixShift<Matrix>
  (const Matrix& m, double sigmar, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const Matrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricFunc<Matrix>
(EigsFunc fcn, octave_idx_type n, const std::string& _typ, double sigmar,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   ComplexMatrix& eig_vec, ComplexColumnVector& eig_val, const Matrix& _b,
   ColumnVector& permB, ColumnVector& resid, std::ostream& os, double tol,
   bool rvec, bool cholB, int disp, int maxit);

// SparseMatrix

template
OCTAVE_API octave_idx_type
EigsRealSymmetricMatrix<SparseMatrix>
  (const SparseMatrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, Matrix& eig_vec,
   ColumnVector& eig_val, const SparseMatrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealSymmetricMatrixShift<SparseMatrix>
  (const SparseMatrix& m, double sigma, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, Matrix& eig_vec,
   ColumnVector& eig_val, const SparseMatrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealSymmetricFunc<SparseMatrix>
(EigsFunc fcn, octave_idx_type n, const std::string& _typ, double sigma,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   Matrix& eig_vec, ColumnVector& eig_val, const SparseMatrix& _b,
   ColumnVector& permB, ColumnVector& resid, std::ostream& os, double tol,
   bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricMatrix<SparseMatrix>
  (const SparseMatrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const SparseMatrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricMatrixShift<SparseMatrix>
  (const SparseMatrix& m, double sigmar, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const SparseMatrix& _b, ColumnVector& permB,
   ColumnVector& resid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsRealNonSymmetricFunc<SparseMatrix>
(EigsFunc fcn, octave_idx_type n, const std::string& _typ, double sigmar,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   ComplexMatrix& eig_vec, ComplexColumnVector& eig_val,
   const SparseMatrix& _b, ColumnVector& permB, ColumnVector& resid,
   std::ostream& os, double tol, bool rvec, bool cholB, int disp, int maxit);

// ComplexMatrix

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricMatrix<ComplexMatrix>
  (const ComplexMatrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const ComplexMatrix& _b, ColumnVector& permB,
   ComplexColumnVector& cresid, std::ostream& os, double tol,
   bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricMatrixShift<ComplexMatrix>
  (const ComplexMatrix& m, Complex sigma, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const ComplexMatrix& _b, ColumnVector& permB,
   ComplexColumnVector& cresid, std::ostream& os, double tol,
   bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricFunc<ComplexMatrix>
(EigsComplexFunc fcn, octave_idx_type n, const std::string& _typ, Complex sigma,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   ComplexMatrix& eig_vec, ComplexColumnVector& eig_val,
   const ComplexMatrix& _b, ColumnVector& permB, ComplexColumnVector& cresid,
   std::ostream& os, double tol, bool rvec, bool cholB, int disp, int maxit);

// SparseComplexMatrix

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricMatrix<SparseComplexMatrix>
  (const SparseComplexMatrix& m, const std::string typ, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const SparseComplexMatrix& _b,
   ColumnVector& permB, ComplexColumnVector& cresid, std::ostream& os,
   double tol, bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricMatrixShift<SparseComplexMatrix>
  (const SparseComplexMatrix& m, Complex sigma, octave_idx_type k,
   octave_idx_type p, octave_idx_type& info, ComplexMatrix& eig_vec,
   ComplexColumnVector& eig_val, const SparseComplexMatrix& _b,
   ColumnVector& permB, ComplexColumnVector& cresid, std::ostream& os,
   double tol, bool rvec, bool cholB, int disp, int maxit);

template
OCTAVE_API octave_idx_type
EigsComplexNonSymmetricFunc<SparseComplexMatrix>
(EigsComplexFunc fcn, octave_idx_type n, const std::string& _typ, Complex sigma,
   octave_idx_type k, octave_idx_type p, octave_idx_type& info,
   ComplexMatrix& eig_vec, ComplexColumnVector& eig_val,
   const SparseComplexMatrix& _b, ColumnVector& permB,
   ComplexColumnVector& cresid, std::ostream& os, double tol, bool rvec,
   bool cholB, int disp, int maxit);
#endif
