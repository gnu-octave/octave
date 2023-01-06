////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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
#include <limits>
#include <vector>

#include "Array.h"
#include "CColVector.h"
#include "CMatrix.h"
#include "CRowVector.h"
#include "CSparse.h"
#include "MArray.h"
#include "dColVector.h"
#include "dDiagMatrix.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "dSparse.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fDiagMatrix.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "mx-cm-s.h"
#include "mx-fcm-fs.h"
#include "mx-fs-fcm.h"
#include "mx-s-cm.h"
#include "oct-cmplx.h"
#include "oct-norm.h"
#include "quit.h"
#include "svd.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Theory: norm accumulator is an object that has an accum method able
// to handle both real and complex element, and a cast operator
// returning the intermediate norm.  Reference: Higham, N. "Estimating
// the Matrix p-Norm." Numer. Math. 62, 539-555, 1992.

// norm accumulator for the p-norm
template <typename R>
class norm_accumulator_p
{
public:
  norm_accumulator_p () { } // we need this one for Array
  norm_accumulator_p (R pp) : m_p(pp), m_scl(0), m_sum(1) { }

  template <typename U>
  void accum (U val)
  {
    octave_quit ();
    R t = std::abs (val);
    if (m_scl == t) // we need this to handle Infs properly
      m_sum += 1;
    else if (m_scl < t)
      {
        m_sum *= std::pow (m_scl/t, m_p);
        m_sum += 1;
        m_scl = t;
      }
    else if (t != 0)
      m_sum += std::pow (t/m_scl, m_p);
  }

  operator R () { return m_scl * std::pow (m_sum, 1/m_p); }

private:
  R m_p, m_scl, m_sum;
};

// norm accumulator for the minus p-pseudonorm
template <typename R>
class norm_accumulator_mp
{
public:
  norm_accumulator_mp () { } // we need this one for Array
  norm_accumulator_mp (R pp) : m_p(pp), m_scl(0), m_sum(1) { }

  template <typename U>
  void accum (U val)
  {
    octave_quit ();
    R t = 1 / std::abs (val);
    if (m_scl == t)
      m_sum += 1;
    else if (m_scl < t)
      {
        m_sum *= std::pow (m_scl/t, m_p);
        m_sum += 1;
        m_scl = t;
      }
    else if (t != 0)
      m_sum += std::pow (t/m_scl, m_p);
  }

  operator R () { return m_scl * std::pow (m_sum, -1/m_p); }

private:
  R m_p, m_scl, m_sum;
};

// norm accumulator for the 2-norm (euclidean)
template <typename R>
class norm_accumulator_2
{
public:
  norm_accumulator_2 () : m_scl(0), m_sum(1) { }

  void accum (R val)
  {
    R t = std::abs (val);
    if (m_scl == t)
      m_sum += 1;
    else if (m_scl < t)
      {
        m_sum *= pow2 (m_scl/t);
        m_sum += 1;
        m_scl = t;
      }
    else if (t != 0)
      m_sum += pow2 (t/m_scl);
  }

  void accum (std::complex<R> val)
  {
    accum (val.real ());
    accum (val.imag ());
  }

  operator R () { return m_scl * std::sqrt (m_sum); }

private:
  static inline R pow2 (R x) { return x*x; }

  //--------

  R m_scl, m_sum;
};

// norm accumulator for the 1-norm (city metric)
template <typename R>
class norm_accumulator_1
{
public:
  norm_accumulator_1 () : m_sum (0) { }
  template <typename U>
  void accum (U val)
  {
    m_sum += std::abs (val);
  }

  operator R () { return m_sum; }

private:
  R m_sum;
};

// norm accumulator for the inf-norm (max metric)
template <typename R>
class norm_accumulator_inf
{
public:
  norm_accumulator_inf () : m_max (0) { }
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
inline void vector_norm (const Array<T>& v, R& res, ACC acc)
{
  for (octave_idx_type i = 0; i < v.numel (); i++)
    acc.accum (v(i));

  res = acc;
}

// dense versions
template <typename T, typename R, typename ACC>
void column_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
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
void row_norms (const MArray<T>& m, MArray<R>& res, ACC acc)
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
void column_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
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
void row_norms (const MSparse<T>& m, MArray<R>& res, ACC acc)
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

// now the dispatchers
#define DEFINE_DISPATCHER(FCN_NAME, ARG_TYPE, RES_TYPE)         \
  template <typename T, typename R>                             \
  RES_TYPE FCN_NAME (const ARG_TYPE& v, R p)                    \
  {                                                             \
    RES_TYPE res;                                               \
    if (p == 2)                                                 \
      FCN_NAME (v, res, norm_accumulator_2<R> ());              \
    else if (p == 1)                                            \
      FCN_NAME (v, res, norm_accumulator_1<R> ());              \
    else if (lo_ieee_isinf (p))                                 \
      {                                                         \
        if (p > 0)                                              \
          FCN_NAME (v, res, norm_accumulator_inf<R> ());        \
        else                                                    \
          FCN_NAME (v, res, norm_accumulator_minf<R> ());       \
      }                                                         \
    else if (p == 0)                                            \
      FCN_NAME (v, res, norm_accumulator_0<R> ());              \
    else if (p > 0)                                             \
      FCN_NAME (v, res, norm_accumulator_p<R> (p));             \
    else                                                        \
      FCN_NAME (v, res, norm_accumulator_mp<R> (p));            \
    return res;                                                 \
  }

DEFINE_DISPATCHER (vector_norm, MArray<T>, R)
DEFINE_DISPATCHER (column_norms, MArray<T>, MArray<R>)
DEFINE_DISPATCHER (row_norms, MArray<T>, MArray<R>)
DEFINE_DISPATCHER (column_norms, MSparse<T>, MArray<R>)
DEFINE_DISPATCHER (row_norms, MSparse<T>, MArray<R>)

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
      R lmnr = std::pow (std::pow (std::abs (lambda1), p) +
                         std::pow (std::abs (mu1), p), 1/p);
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
      R lmnr = std::pow (std::pow (std::abs (lambda1), p) +
                         std::pow (std::abs (mu1), p), 1/p);
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
inline T elem_dual_p (T x, R p)
{
  return math::signum (x) * std::pow (std::abs (x), p-1);
}

// the VectorT is used for vectors, but actually it has to be
// a Matrix type to allow all the operations.  For instance SparseMatrix
// does not support multiplication with column/row vectors.
// the dual vector
template <typename VectorT, typename R>
VectorT dual_p (const VectorT& x, R p, R q)
{
  VectorT res (x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    res.xelem (i) = elem_dual_p (x(i), p);
  return res / vector_norm (res, q);
}

// Higham's hybrid method
template <typename MatrixT, typename VectorT, typename R>
R higham (const MatrixT& m, R p, R tol, int maxiter,
          VectorT& x)
{
  x.resize (m.columns (), 1);
  // the OSE part
  VectorT y(m.rows (), 1, 0), z(m.rows (), 1);
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
  R q = p/(p-1);

  R gamma = 0, gamma1;
  int iter = 0;
  while (iter < maxiter)
    {
      octave_quit ();
      y = m*x;
      gamma1 = gamma;
      gamma = vector_norm (y, p);
      z = dual_p (y, p, q);
      z = z.hermitian ();
      z = z * m;

      if (iter > 0 && (vector_norm (z, q) <= gamma
                       || (gamma - gamma1) <= tol*gamma))
        break;

      z = z.hermitian ();
      x = dual_p (z, q, p);
      iter++;
    }

  return gamma;
}

// derive column vector and SVD types

static const char *p_less1_gripe = "xnorm: p must be >= 1";

// Static constant to control the maximum number of iterations.  100 seems to
// be a good value.  Eventually, we can provide a means to change this
// constant from Octave.
static int max_norm_iter = 100;

// version with SVD for dense matrices
template <typename MatrixT, typename VectorT, typename R>
R svd_matrix_norm (const MatrixT& m, R p, VectorT)
{
  // NOTE: The octave:: namespace tags are needed for the following
  // function calls until the deprecated inline functions are removed
  // from oct-norm.h.

  R res = 0;
  if (p == 2)
    {
      math::svd<MatrixT> fact (m, math::svd<MatrixT>::Type::sigma_only);
      res = fact.singular_values () (0, 0);
    }
  else if (p == 1)
    res = octave::xcolnorms (m, static_cast<R> (1)).max ();
  else if (lo_ieee_isinf (p) && p > 1)
    res = octave::xrownorms (m, static_cast<R> (1)).max ();
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

// SVD-free version for sparse matrices
template <typename MatrixT, typename VectorT, typename R>
R matrix_norm (const MatrixT& m, R p, VectorT)
{
  // NOTE: The octave:: namespace tags are needed for the following
  // function calls until the deprecated inline functions are removed
  // from oct-norm.h.

  R res = 0;
  if (p == 1)
    res = octave::xcolnorms (m, static_cast<R> (1)).max ();
  else if (lo_ieee_isinf (p) && p > 1)
    res = octave::xrownorms (m, static_cast<R> (1)).max ();
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

#define DEFINE_XNORM_FCNS(PREFIX, RTYPE)                                \
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
    return vector_norm (x, static_cast<RTYPE> (2));                     \
  }

DEFINE_XNORM_FCNS(, double)
DEFINE_XNORM_FCNS(Complex, double)
DEFINE_XNORM_FCNS(Float, float)
DEFINE_XNORM_FCNS(FloatComplex, float)

// this is needed to avoid copying the sparse matrix for xfrobnorm
template <typename T, typename R>
inline void array_norm_2 (const T *v, octave_idx_type n, R& res)
{
  norm_accumulator_2<R> acc;
  for (octave_idx_type i = 0; i < n; i++)
    acc.accum (v[i]);

  res = acc;
}

#define DEFINE_XNORM_SPARSE_FCNS(PREFIX, RTYPE)                 \
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

#define DEFINE_COLROW_NORM_FCNS(PREFIX, RPREFIX, RTYPE)         \
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

DEFINE_COLROW_NORM_FCNS(,, double)
DEFINE_COLROW_NORM_FCNS(Complex,, double)
DEFINE_COLROW_NORM_FCNS(Float, Float, float)
DEFINE_COLROW_NORM_FCNS(FloatComplex, Float, float)

DEFINE_COLROW_NORM_FCNS(Sparse,, double)
DEFINE_COLROW_NORM_FCNS(SparseComplex,, double)

OCTAVE_END_NAMESPACE(octave)
