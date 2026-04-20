////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2026 The Octave Project Developers
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

#include <algorithm>
#include <iostream>
#include <limits>
#include <type_traits>
#include <vector>

#include "Array-oct.h"
#include "CColVector.h"
#include "CMatrix.h"
#include "CRowVector.h"
#include "CSparse.h"
#include "MArray.h"
#include "blas-proto.h"
#include "dColVector.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "dSparse.h"
#include "f77-fcn.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "lapack-proto.h"
#include "lo-ieee.h"
#include "mappers.h"
#include "mx-cm-s.h"
#include "mx-fcm-fs.h"
#include "mx-fs-fcm.h"
#include "mx-s-cm.h"
#include "oct-cmplx.h"
#include "oct-error.h"
#include "oct-norm.h"
#include "quit.h"
#include "svd.h"

#if defined (HAVE_ARPACK)
#  include "eigs-base.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

// BLAS NRM2 wrappers for vector 2-norm

inline double
blas_nrm2 (octave_idx_type n, const double *x, octave_idx_type incx)
{
  F77_INT f77_n = to_f77_int (n);
  F77_INT f77_incx = to_f77_int (incx);
  F77_DBLE result;
  F77_FUNC (xdnrm2, XDNRM2) (f77_n, x, f77_incx, result);
  return result;
}

inline float
blas_nrm2 (octave_idx_type n, const float *x, octave_idx_type incx)
{
  F77_INT f77_n = to_f77_int (n);
  F77_INT f77_incx = to_f77_int (incx);
  F77_REAL result;
  F77_FUNC (xsnrm2, XSNRM2) (f77_n, x, f77_incx, result);
  return result;
}

inline double
blas_nrm2 (octave_idx_type n, const Complex *x, octave_idx_type incx)
{
  F77_INT f77_n = to_f77_int (n);
  F77_INT f77_incx = to_f77_int (incx);
  F77_DBLE result;
  F77_FUNC (xdznrm2, XDZNRM2) (f77_n, F77_CONST_DBLE_CMPLX_ARG (x), f77_incx, result);
  return result;
}

inline float
blas_nrm2 (octave_idx_type n, const FloatComplex *x, octave_idx_type incx)
{
  F77_INT f77_n = to_f77_int (n);
  F77_INT f77_incx = to_f77_int (incx);
  F77_REAL result;
  F77_FUNC (xscnrm2, XSCNRM2) (f77_n, F77_CONST_CMPLX_ARG (x), f77_incx, result);
  return result;
}

// LAPACK LANGE wrappers for dense matrix norms.
// norm_type: 'O'/'1' = 1-norm, 'I' = inf-norm, 'F' = Frobenius
//
// Note: LANGE mishandles Inf due to internal scaling (Inf/Inf = NaN).
// We fix this by checking for Inf when LANGE returns NaN.

template <typename T, typename R>
inline R
lange_inf_fixup (R result, const T *data, octave_idx_type numel)
{
  // LANGE returned NaN - check if it should be Inf instead.
  // If there's a genuine NaN in input, keep returning NaN.
  // Only scan the matrix if LANGE returned NaN (no overhead otherwise).
  if (math::isnan (result))
    {
      bool has_inf = false;
      for (octave_idx_type i = 0; i < numel; i++)
        {
          if (math::isnan (data[i]))
            return result;  // Genuine NaN takes precedence
          if (math::isinf (data[i]))
            has_inf = true;
        }
      if (has_inf)
        return numeric_limits<R>::Inf ();
    }
  return result;
}

inline double
lange (char norm_type, const Matrix& m)
{
  F77_INT mm = to_f77_int (m.rows ());
  F77_INT nn = to_f77_int (m.cols ());
  F77_INT lda = std::max (mm, static_cast<F77_INT> (1));

  OCTAVE_LOCAL_BUFFER (double, work, mm);

  F77_DBLE result;
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG (&norm_type), mm, nn,
                               m.data (), lda, work, result
                               F77_CHAR_ARG_LEN (1));
  return lange_inf_fixup (result, m.data (), m.numel ());
}

inline float
lange (char norm_type, const FloatMatrix& m)
{
  F77_INT mm = to_f77_int (m.rows ());
  F77_INT nn = to_f77_int (m.cols ());
  F77_INT lda = std::max (mm, static_cast<F77_INT> (1));

  OCTAVE_LOCAL_BUFFER (float, work, mm);

  F77_REAL result;
  F77_FUNC (xslange, XSLANGE) (F77_CONST_CHAR_ARG (&norm_type), mm, nn,
                               m.data (), lda, work, result
                               F77_CHAR_ARG_LEN (1));
  return lange_inf_fixup (result, m.data (), m.numel ());
}

inline double
lange (char norm_type, const ComplexMatrix& m)
{
  F77_INT mm = to_f77_int (m.rows ());
  F77_INT nn = to_f77_int (m.cols ());
  F77_INT lda = std::max (mm, static_cast<F77_INT> (1));

  OCTAVE_LOCAL_BUFFER (double, work, mm);

  F77_DBLE result;
  F77_FUNC (xzlange, XZLANGE) (F77_CONST_CHAR_ARG (&norm_type), mm, nn,
                               F77_CONST_DBLE_CMPLX_ARG (m.data ()),
                               lda, work, result
                               F77_CHAR_ARG_LEN (1));
  return lange_inf_fixup (result, m.data (), m.numel ());
}

inline float
lange (char norm_type, const FloatComplexMatrix& m)
{
  F77_INT mm = to_f77_int (m.rows ());
  F77_INT nn = to_f77_int (m.cols ());
  F77_INT lda = std::max (mm, static_cast<F77_INT> (1));

  OCTAVE_LOCAL_BUFFER (float, work, mm);

  F77_REAL result;
  F77_FUNC (xclange, XCLANGE) (F77_CONST_CHAR_ARG (&norm_type),
                               mm, nn, F77_CONST_CMPLX_ARG (m.data ()),
                               lda, work, result
                               F77_CHAR_ARG_LEN (1));
  return lange_inf_fixup (result, m.data (), m.numel ());
}

// Norm accumulators for various norms.
// Reference: N. Higham, "Estimating the Matrix p-Norm",
// Numer. Math., 62, pp. 539-555, 1992.
//
// For float types, we accumulate in double for better precision.
// For double types, we use Hammarling scaling to avoid overflow/underflow.
// Kahan compensation is NOT used since these accumulators are primarily
// used in iterative methods where sqrt(eps) precision is sufficient.

// Accumulator type: float promotes to double for better precision
template <typename R>
using accum_type = std::conditional_t<std::is_same_v<R, float>, double, R>;

// norm accumulator for the p-norm
// Uses Hammarling scaling to avoid overflow.
template <typename R>
class norm_accumulator_p
{
public:
  using AT = accum_type<R>;

  norm_accumulator_p (R pp) : m_p (pp), m_scl (0), m_sum (1) { }

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (norm_accumulator_p)

  template <typename U>
  void accum (U val)
  {
    AT t = std::abs (val);
    if (t != 0)
      {
        if (m_scl == t)
          m_sum += 1;
        else if (m_scl < t)
          {
            m_sum *= std::pow (m_scl / t, static_cast<AT> (m_p));
            m_sum += 1;
            m_scl = t;
          }
        else
          m_sum += std::pow (t / m_scl, static_cast<AT> (m_p));
      }
  }

  operator R ()
  {
    return static_cast<R> (m_scl * std::pow (m_sum, 1 / static_cast<AT> (m_p)));
  }

private:
  R m_p;
  AT m_scl, m_sum;
};

// norm accumulator for the minus p-pseudonorm
template <typename R>
class norm_accumulator_mp
{
public:
  using AT = accum_type<R>;

  norm_accumulator_mp (R pp) : m_p (pp), m_scl (0), m_sum (1) { }

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (norm_accumulator_mp)

  template <typename U>
  void accum (U val)
  {
    AT t = 1 / std::abs (val);
    if (m_scl == t)
      m_sum += 1;
    else if (m_scl < t)
      {
        m_sum *= std::pow (m_scl / t, static_cast<AT> (m_p));
        m_sum += 1;
        m_scl = t;
      }
    else if (t != 0)
      m_sum += std::pow (t / m_scl, static_cast<AT> (m_p));
  }

  operator R ()
  {
    return static_cast<R> (m_scl * std::pow (m_sum, -1 / static_cast<AT> (m_p)));
  }

private:
  R m_p;
  AT m_scl, m_sum;
};

// norm accumulator for the 2-norm (euclidean)
// Uses Hammarling scaling to avoid overflow.
template <typename R>
class norm_accumulator_2
{
public:
  using AT = accum_type<R>;

  norm_accumulator_2 () : m_scl (0), m_sum (0) { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (norm_accumulator_2)

  void accum (R val)
  {
    AT t = std::abs (val);
    if (t != 0)
      {
        if (m_scl == t)
          m_sum += 1;
        else if (m_scl < t)
          {
            AT r = m_scl / t;
            m_sum *= r * r;
            m_sum += 1;
            m_scl = t;
          }
        else
          {
            AT r = t / m_scl;
            m_sum += r * r;
          }
      }
  }

  void accum (std::complex<R> val)
  {
    accum (val.real ());
    accum (val.imag ());
  }

  operator R ()
  {
    return static_cast<R> (m_scl * std::sqrt (m_sum));
  }

private:
  AT m_scl, m_sum;
};

// norm accumulator for the 1-norm (city metric)
template <typename R>
class norm_accumulator_1
{
public:
  using AT = accum_type<R>;

  norm_accumulator_1 () : m_sum (0) { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (norm_accumulator_1)

  template <typename U>
  void accum (U val)
  {
    m_sum += std::abs (val);
  }

  operator R () { return static_cast<R> (m_sum); }

private:
  AT m_sum;
};

// norm accumulator for the inf-norm (max metric)
template <typename R>
class norm_accumulator_inf
{
public:

  norm_accumulator_inf () : m_max (0) { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (norm_accumulator_inf)

  template <typename U>
  void accum (U val)
  {
    if (math::isnan (val))
      m_max = numeric_limits<R>::NaN ();
    else
      m_max = std::max (m_max, std::abs (val));
  }

  operator R () { return m_max; }

private:
  R m_max;
};

// norm accumulator for the -inf pseudonorm (min abs value)
template <typename R>
class norm_accumulator_minf
{
public:

  norm_accumulator_minf () : m_min (numeric_limits<R>::Inf ()) { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (norm_accumulator_minf)

  template <typename U>
  void accum (U val)
  {
    if (math::isnan (val))
      m_min = numeric_limits<R>::NaN ();
    else
      m_min = std::min (m_min, std::abs (val));
  }

  operator R () { return m_min; }

private:
  R m_min;
};

// norm accumulator for the 0-pseudonorm (hamming distance)
template <typename R>
class norm_accumulator_0
{
public:

  norm_accumulator_0 () : m_num (0) { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (norm_accumulator_0)

  template <typename U>
  void accum (U val)
  {
    if (val != static_cast<U> (0)) ++m_num;
  }

  operator R () { return m_num; }

private:
  unsigned int m_num;
};

// OK, we're armed :) Now let's go for the fun

template <typename T, typename R, typename ACC>
inline void
vector_norm (const Array<T>& v, R& res, ACC acc)
{
  for (octave_idx_type i = 0; i < v.numel (); i++)
    acc.accum (v(i));

  res = acc;
}

// sparse version - only iterates over nonzero elements
template <typename T, typename R, typename ACC>
inline void
vector_norm (const MSparse<T>& v, R& res, ACC acc)
{
  octave_idx_type nnz = v.nnz ();
  for (octave_idx_type i = 0; i < nnz; i++)
    acc.accum (v.data (i));

  res = acc;
}

// dense versions
template <typename T, typename R, typename ACC>
void
column_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (1, m.columns ()));
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      ACC accj = acc;
      for (octave_idx_type i = 0; i < m.rows (); i++)
        accj.accum (m(i, j));

      res.xelem (j) = accj;
    }
}

template <typename T, typename R, typename ACC>
void
row_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (m.rows (), 1));
  std::vector<ACC> acci (m.rows (), acc);
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      for (octave_idx_type i = 0; i < m.rows (); i++)
        acci[i].accum (m(i, j));
    }

  for (octave_idx_type i = 0; i < m.rows (); i++)
    res.xelem (i) = acci[i];
}

// sparse versions
template <typename T, typename R, typename ACC>
void
column_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (1, m.columns ()));
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      ACC accj = acc;
      for (octave_idx_type k = m.cidx (j); k < m.cidx (j+1); k++)
        accj.accum (m.data (k));

      res.xelem (j) = accj;
    }
}

template <typename T, typename R, typename ACC>
void
row_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
{
  res = MArray<R> (dim_vector (m.rows (), 1));
  std::vector<ACC> acci (m.rows (), acc);
  for (octave_idx_type j = 0; j < m.columns (); j++)
    {
      for (octave_idx_type k = m.cidx (j); k < m.cidx (j+1); k++)
        acci[m.ridx (k)].accum (m.data (k));
    }

  for (octave_idx_type i = 0; i < m.rows (); i++)
    res.xelem (i) = acci[i];
}

// Dense vector 2-norm using BLAS NRM2
template <typename T, typename R>
inline R
vector_norm_2_blas (const MArray<T>& v)
{
  return blas_nrm2 (v.numel (), v.data (), 1);
}

// Dispatcher for dense arrays - use BLAS for 2-norm
template <typename T, typename R>
R
vector_norm (const MArray<T>& v, R p)
{
  R res;
  if (p == 2)
    res = vector_norm_2_blas<T, R> (v);
  else if (p == 1)
    vector_norm (v, res, norm_accumulator_1<R> ());
  else if (math::isinf (p))
    {
      if (p > 0)
        vector_norm (v, res, norm_accumulator_inf<R> ());
      else
        vector_norm (v, res, norm_accumulator_minf<R> ());
    }
  else if (p == 0)
    vector_norm (v, res, norm_accumulator_0<R> ());
  else if (p > 0)
    vector_norm (v, res, norm_accumulator_p<R> (p));
  else
    vector_norm (v, res, norm_accumulator_mp<R> (p));
  return res;
}

// Dispatcher for sparse arrays - accumulators only (no BLAS)
template <typename T, typename R>
R
vector_norm (const MSparse<T>& v, R p)
{
  R res;
  if (p == 2)
    vector_norm (v, res, norm_accumulator_2<R> ());
  else if (p == 1)
    vector_norm (v, res, norm_accumulator_1<R> ());
  else if (math::isinf (p))
    {
      if (p > 0)
        vector_norm (v, res, norm_accumulator_inf<R> ());
      else
        vector_norm (v, res, norm_accumulator_minf<R> ());
    }
  else if (p == 0)
    vector_norm (v, res, norm_accumulator_0<R> ());
  else if (p > 0)
    vector_norm (v, res, norm_accumulator_p<R> (p));
  else
    vector_norm (v, res, norm_accumulator_mp<R> (p));
  return res;
}

// Dispatchers for column/row norms (use accumulators)
#define DEFINE_COLROW_DISPATCHER(FCN_NAME, ARG_TYPE, RES_TYPE)  \
template <typename T, typename R>                               \
RES_TYPE FCN_NAME (const ARG_TYPE& v, R p)                      \
{                                                               \
  RES_TYPE res;                                                 \
  if (p == 2)                                                   \
    FCN_NAME (v, res, norm_accumulator_2<R> ());                \
  else if (p == 1)                                              \
    FCN_NAME (v, res, norm_accumulator_1<R> ());                \
  else if (math::isinf (p))                                     \
    {                                                           \
      if (p > 0)                                                \
        FCN_NAME (v, res, norm_accumulator_inf<R> ());          \
      else                                                      \
        FCN_NAME (v, res, norm_accumulator_minf<R> ());         \
    }                                                           \
  else if (p == 0)                                              \
    FCN_NAME (v, res, norm_accumulator_0<R> ());                \
  else if (p > 0)                                               \
    FCN_NAME (v, res, norm_accumulator_p<R> (p));               \
  else                                                          \
    FCN_NAME (v, res, norm_accumulator_mp<R> (p));              \
  return res;                                                   \
}

DEFINE_COLROW_DISPATCHER (column_norms, MArray<T>, MArray<R>)
DEFINE_COLROW_DISPATCHER (row_norms, MArray<T>, MArray<R>)
DEFINE_COLROW_DISPATCHER (column_norms, MSparse<T>, MArray<R>)
DEFINE_COLROW_DISPATCHER (row_norms, MSparse<T>, MArray<R>)

// The approximate subproblem in Higham's method.  Find lambda and mu such
// that norm ([lambda, mu], p) == 1 and norm (y*lambda + col*mu, p) is
// maximized.
// Real version.  As in Higham's paper.
template <typename ColVectorT, typename R>
static void
higham_subp (const ColVectorT& y, const ColVectorT& col,
             octave_idx_type nsamp, R p, R& lambda, R& mu)
{
  R nrm = 0;
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      R lambda1 = cos (fi);
      R mu1 = sin (fi);
      R lmnr = std::pow (std::pow (std::abs (lambda1), p)
                         + std::pow (std::abs (mu1), p), 1 / p);
      lambda1 /= lmnr; mu1 /= lmnr;
      R nrm1 = vector_norm (lambda1 * y + mu1 * col, p);
      if (nrm1 > nrm)
        {
          lambda = lambda1;
          mu = mu1;
          nrm = nrm1;
        }
    }
}

// Complex version.  Higham's paper does not deal with complex case, so we
// use a simple extension.  First, guess the magnitudes as in real version,
// then try to rotate lambda to improve further.
template <typename ColVectorT, typename R>
static void
higham_subp (const ColVectorT& y, const ColVectorT& col,
             octave_idx_type nsamp, R p,
             std::complex<R>& lambda, std::complex<R>& mu)
{
  typedef std::complex<R> CR;
  R nrm = 0;
  lambda = 1.0;
  CR lamcu = lambda / std::abs (lambda);
  // Probe magnitudes
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      R lambda1 = cos (fi);
      R mu1 = sin (fi);
      R lmnr = std::pow (std::pow (std::abs (lambda1), p)
                         + std::pow (std::abs (mu1), p), 1 / p);
      lambda1 /= lmnr; mu1 /= lmnr;
      R nrm1 = vector_norm (lambda1 * lamcu * y + mu1 * col, p);
      if (nrm1 > nrm)
        {
          lambda = lambda1 * lamcu;
          mu = mu1;
          nrm = nrm1;
        }
    }
  R lama = std::abs (lambda);
  // Probe orientation
  for (octave_idx_type i = 0; i < nsamp; i++)
    {
      octave_quit ();
      R fi = i * static_cast<R> (M_PI) / nsamp;
      lamcu = CR (cos (fi), sin (fi));
      R nrm1 = vector_norm (lama * lamcu * y + mu * col, p);
      if (nrm1 > nrm)
        {
          lambda = lama * lamcu;
          nrm = nrm1;
        }
    }
}

// the p-dual element (should work for both real and complex)
template <typename T, typename R>
inline T
elem_dual_p (T x, R p)
{
  return math::signum (x) * std::pow (std::abs (x), p - 1);
}

// the VectorT is used for vectors, but actually it has to be
// a Matrix type to allow all the operations.  For instance SparseMatrix
// does not support multiplication with column/row vectors.
// the dual vector
template <typename VectorT, typename R>
VectorT
dual_p (const VectorT& x, R p, R q)
{
  VectorT res (x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    res.xelem (i) = elem_dual_p (x(i), p);
  return res / vector_norm (res, q);
}

// Higham's hybrid method
template <typename MatrixT, typename VectorT, typename R>
R
higham (const MatrixT& m, R p, R tol, int maxiter,
        VectorT& x)
{
  x.resize (m.columns (), 1);
  // the OSE part
  VectorT y (m.rows (), 1, 0), z (m.rows (), 1);
  typedef typename VectorT::element_type RR;
  RR lambda = 0;
  RR mu = 1;
  for (octave_idx_type k = 0; k < m.columns (); k++)
    {
      octave_quit ();
      VectorT col (m.column (k));
      if (k > 0)
        higham_subp (y, col, 4*k, p, lambda, mu);
      for (octave_idx_type i = 0; i < k; i++)
        x(i) *= lambda;
      x(k) = mu;
      y = lambda * y + mu * col;
    }

  // the PM part
  x = x / vector_norm (x, p);
  R q = p / (p - 1);

  R gamma = 0, gamma1;
  int iter = 0;
  while (iter < maxiter)
    {
      octave_quit ();
      y = m * x;
      gamma1 = gamma;
      gamma = vector_norm (y, p);
      z = dual_p (y, p, q);
      z = z.hermitian ();
      z = z * m;

      if (iter > 0 && (vector_norm (z, q) <= gamma
                       || (gamma - gamma1) <= tol * gamma))
        break;

      z = z.hermitian ();
      x = dual_p (z, q, p);
      iter++;
    }

  return gamma;
}

// derive column vector and SVD types

constexpr const char *p_less1_gripe = "xnorm: p must be >= 1";

// Maximum number of iterations for iterative norm algorithms.
// 256 seems to be a good value.  Eventually, we can provide a means to change
// this constant from Octave.
constexpr int max_norm_iter = 256;

// version with SVD for dense matrices, uses LAPACK LANGE for 1/Inf norms
template <typename MatrixT, typename VectorT, typename R>
R
svd_matrix_norm (const MatrixT& m, R p, VectorT)
{
  R res = 0;
  if (p == 2)
    {
      math::svd<MatrixT> fact (m, math::svd<MatrixT>::Type::sigma_only);
      res = fact.singular_values () (0, 0);
    }
  else if (p == 1)
    res = lange ('O', m);
  else if (math::isinf (p) && p > 1)
    res = lange ('I', m);
  else if (p > 1)
    {
      VectorT x;
      const R sqrteps = std::sqrt (std::numeric_limits<R>::epsilon ());
      res = higham (m, p, sqrteps, max_norm_iter, x);
    }
  else
    (*current_liboctave_error_handler) ("%s", p_less1_gripe);

  return res;
}

// Helper for conjugate that works with real types (no-op for real)
template <typename T>
constexpr T safe_conj (T x)
{
  if constexpr (std::is_arithmetic_v<T>)
    return x;
  else
    return std::conj (x);
}

// Helper for dot product (conjugate-linear in first argument)
template <typename VectorT>
auto
vector_dot (const VectorT& x, const VectorT& y)
{
  using T = typename VectorT::element_type;
  T sum = T (0);
  for (octave_idx_type i = 0; i < x.numel (); i++)
    sum += safe_conj (x(i)) * y(i);
  return sum;
}

// Power iteration for largest singular value (sparse 2-norm)
// Computes sigma_max = sqrt(lambda_max(A'*A)) via power method on A'*A
// This avoids forming A'*A explicitly by using two sparse matvecs per iteration
// Used as fallback when ARPACK is not available.
template <typename MatrixT, typename VectorT, typename R>
R
sparse_2norm_power (const MatrixT& m, R tol, int maxiter, VectorT)
{
  octave_idx_type nc = m.columns ();
  octave_idx_type nr = m.rows ();

  if (nc == 0 || nr == 0)
    return R (0);

  // Work with smaller dimension for efficiency
  const bool use_aat = (nr < nc);
  octave_idx_type n = std::min (nr, nc);

  // Initialize x = ones(n,1) normalized
  VectorT x (n, 1, R (1) / std::sqrt (static_cast<R> (n)));
  VectorT y (n, 1);

  R lambda = 0, lambda_prev = 0;

  for (int iter = 0; iter < maxiter; iter++)
    {
      octave_quit ();

      // Compute y = (A'*A)*x or y = (A*A')*x via two sparse matvecs
      if (use_aat)
        {
          // y = A * (A' * x) -- fewer ops when nr < nc
          auto z = m.hermitian () * x;
          y = m * z;
        }
      else
        {
          // y = A' * (A * x) -- fewer ops when nc <= nr
          auto z = m * x;
          y = m.hermitian () * z;
        }

      // Rayleigh quotient: lambda = x' * y
      lambda_prev = lambda;
      lambda = std::abs (vector_dot (x, y));

      // Normalize y -> x for next iteration
      const R norm_y = vector_norm (y, R (2));

      if (norm_y == R (0))
        return R (0);

      x = y / norm_y;

      // Check convergence
      if (iter > 0 && std::abs (lambda - lambda_prev) <= tol * lambda)
        break;
    }

  return std::sqrt (lambda);
}

#if defined (HAVE_ARPACK)

// ARPACK-based sparse 2-norm using Implicitly Restarted Lanczos.
// Builds augmented matrix [0, A; A', 0] whose eigenvalues are +-sigma_i.
// More robust than power iteration for clustered singular values.
double
sparse_2norm_arpack (const SparseMatrix& m, double tol, int maxiter)
{
  octave_idx_type nc = m.cols ();
  octave_idx_type nr = m.rows ();

  if (nc == 0 || nr == 0)
    return 0.0;

  octave_quit ();

  // Build augmented sparse matrix: [0, A; A', 0]
  // Eigenvalues are +-sigma_i, so max |eigenvalue| = sigma_max
  octave_idx_type n = nr + nc;
  SparseMatrix aug (n, n);

  // Insert A in upper-right block (rows 0..nr-1, cols nr..n-1)
  aug = aug.insert (m, 0, nr);

  octave_quit ();

  // Insert A' in lower-left block (rows nr..n-1, cols 0..nr-1)
  aug = aug.insert (m.transpose (), nr, 0);

  octave_quit ();

  // Use ARPACK to find largest magnitude eigenvalue
  octave_idx_type k = 1;              // number of eigenvalues
  octave_idx_type p = -1;             // auto-select basis size (will become max(2k,20))
  octave_idx_type info = 0;           // start with random initial vector
  Matrix eig_vec;
  ColumnVector eig_val;
  SparseMatrix B;                     // empty = standard eigenproblem
  ColumnVector permB;
  ColumnVector resid;                 // empty = random initial vector

  EigsRealSymmetricMatrix (aug, std::string ("LM"), k, p, info,
                           eig_vec, eig_val, B, permB, resid,
                           std::cout, tol, false, false, 0, maxiter);

  octave_quit ();

  if (info < 0 || eig_val.numel () == 0)
    return 0.0;  // ARPACK failed, caller should fall back

  return std::abs (eig_val (0));
}

// Complex version
double
sparse_2norm_arpack (const SparseComplexMatrix& m, double tol, int maxiter)
{
  octave_idx_type nc = m.cols ();
  octave_idx_type nr = m.rows ();

  if (nc == 0 || nr == 0)
    return 0.0;

  octave_quit ();

  // Build augmented sparse matrix: [0, A; A', 0] where A' is conjugate transpose
  octave_idx_type n = nr + nc;
  SparseComplexMatrix aug (n, n);

  // Insert A in upper-right block
  aug = aug.insert (m, 0, nr);

  octave_quit ();

  // Insert A' (hermitian = conjugate transpose) in lower-left block
  aug = aug.insert (m.hermitian (), nr, 0);

  octave_quit ();

  // For complex matrices, the augmented matrix is Hermitian
  octave_idx_type k = 1;
  octave_idx_type p = -1;
  octave_idx_type info = 0;
  ComplexMatrix eig_vec;
  ComplexColumnVector eig_val;
  SparseComplexMatrix B;
  ColumnVector permB;
  ComplexColumnVector resid;

  EigsComplexNonSymmetricMatrix (aug, std::string ("LM"), k, p, info,
                                 eig_vec, eig_val, B, permB, resid,
                                 std::cout, tol, false, false, 0, maxiter);

  octave_quit ();

  if (info < 0 || eig_val.numel () == 0)
    return 0.0;

  return std::abs (eig_val (0));
}

#endif  // HAVE_ARPACK

// Unified sparse 2-norm: use ARPACK when available, fall back to power iteration
template <typename MatrixT, typename VectorT, typename R>
R
sparse_2norm (const MatrixT& m, R tol, int maxiter, VectorT)
{
  // All-zero matrix: ARPACK's dsaupd errors on a zero starting vector, so
  // short-circuit before either backend is invoked.
  if (m.nnz () == 0)
    return R (0);

#if defined (HAVE_ARPACK)
  // Use ARPACK (Lanczos) - more accurate than power iteration
  R res = sparse_2norm_arpack (m, tol, maxiter);
  if (res > R (0))
    return res;
  // Fall through to power iteration if ARPACK failed
#endif

  return sparse_2norm_power (m, tol, maxiter, VectorT ());
}

// SVD-free version for sparse matrices
template <typename MatrixT, typename VectorT, typename R>
R
matrix_norm (const MatrixT& m, R p, VectorT)
{
  R res = 0;
  if (p == 1)
    res = xcolnorms (m, static_cast<R> (1)).max ();
  else if (math::isinf (p) && p > 1)
    res = xrownorms (m, static_cast<R> (1)).max ();
  else if (p == 2)
    {
      // Use ARPACK (Lanczos) or power iteration for 2-norm (spectral norm)
      const R sqrteps = std::sqrt (std::numeric_limits<R>::epsilon ());
      res = sparse_2norm (m, sqrteps, max_norm_iter, VectorT ());
    }
  else if (p > 1)
    {
      VectorT x;
      const R sqrteps = std::sqrt (std::numeric_limits<R>::epsilon ());
      res = higham (m, p, sqrteps, max_norm_iter, x);
    }
  else
    (*current_liboctave_error_handler) ("%s", p_less1_gripe);

  return res;
}

// and finally, here's what we've promised in the header file

#define DEFINE_XNORM_FCNS(PREFIX, RTYPE)                              \
RTYPE xnorm (const PREFIX##ColumnVector& x, RTYPE p)                  \
{                                                                     \
  return vector_norm (x, p);                                          \
}                                                                     \
RTYPE xnorm (const PREFIX##RowVector& x, RTYPE p)                     \
{                                                                     \
  return vector_norm (x, p);                                          \
}                                                                     \
RTYPE xnorm (const PREFIX##Matrix& x, RTYPE p)                        \
{                                                                     \
  return svd_matrix_norm (x, p, PREFIX##Matrix ());                   \
}                                                                     \
RTYPE xfrobnorm (const PREFIX##Matrix& x)                             \
{                                                                     \
  return lange ('F', x);                                              \
}

DEFINE_XNORM_FCNS(, double)
DEFINE_XNORM_FCNS(Complex, double)
DEFINE_XNORM_FCNS(Float, float)
DEFINE_XNORM_FCNS(FloatComplex, float)

// Sparse Frobenius norm - use accumulator over nonzeros
template <typename T, typename R>
inline void array_norm_2 (const T *v, octave_idx_type n, R& res)
{
  norm_accumulator_2<R> acc;
  for (octave_idx_type i = 0; i < n; i++)
    acc.accum (v[i]);

  res = acc;
}

#define DEFINE_XNORM_SPARSE_FCNS(PREFIX, RTYPE)               \
RTYPE xnorm (const Sparse##PREFIX##Matrix& x, RTYPE p)        \
{                                                             \
  return matrix_norm (x, p, PREFIX##Matrix ());               \
}                                                             \
RTYPE xfrobnorm (const Sparse##PREFIX##Matrix& x)             \
{                                                             \
  RTYPE res;                                                  \
  array_norm_2 (x.data (), x.nnz (), res);                    \
  return res;                                                 \
}

DEFINE_XNORM_SPARSE_FCNS(, double)
DEFINE_XNORM_SPARSE_FCNS(Complex, double)

#define DEFINE_COLROW_NORM_FCNS(PREFIX, RPREFIX, RTYPE)       \
RPREFIX##RowVector                                            \
xcolnorms (const PREFIX##Matrix& m, RTYPE p)                  \
{                                                             \
  return column_norms (m, p);                                 \
}                                                             \
RPREFIX##ColumnVector                                         \
xrownorms (const PREFIX##Matrix& m, RTYPE p)                  \
{                                                             \
  return row_norms (m, p);                                    \
}                                                             \

DEFINE_COLROW_NORM_FCNS(, , double)
DEFINE_COLROW_NORM_FCNS(Complex, , double)
DEFINE_COLROW_NORM_FCNS(Float, Float, float)
DEFINE_COLROW_NORM_FCNS(FloatComplex, Float, float)

DEFINE_COLROW_NORM_FCNS(Sparse, , double)
DEFINE_COLROW_NORM_FCNS(SparseComplex, , double)

OCTAVE_END_NAMESPACE(octave)
