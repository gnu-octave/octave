////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2025 The Octave Project Developers
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

#include <algorithm>

#include "Array.h"
#include "CColVector.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "CRowVector.h"
#include "MArray.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dNDArray.h"
#include "dRowVector.h"
#include "f77-fcn.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCNDArray.h"
#include "fCRowVector.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "fNDArray.h"
#include "fRowVector.h"
#include "lo-blas-proto.h"
#include "oct-convn.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Begin AXPY specialization wrappers
// Function overloading approach for BLAS axpy operations

// double * double
inline void
blas_axpy (const F77_INT& n, const double& alpha, const double *x,
           const F77_INT& incx, double *y, const F77_INT& incy)
{
  F77_FUNC (daxpy, DAXPY) (n, alpha, x, incx, y, incy);
}

// float * float
inline void
blas_axpy (const F77_INT& n, const float& alpha, const float *x,
           const F77_INT& incx, float *y, const F77_INT& incy)
{
  F77_FUNC (saxpy, SAXPY) (n, alpha, x, incx, y, incy);
}

// complex<double> * complex<double>
inline void
blas_axpy (const F77_INT& n, const F77_DBLE_CMPLX& alpha,
           const F77_DBLE_CMPLX *x, const F77_INT& incx,
           F77_DBLE_CMPLX *y, const F77_INT& incy)
{
  F77_FUNC (zaxpy, ZAXPY) (n, alpha, x, incx, y, incy);
}

// complex<float> * complex<float>
inline void
blas_axpy (const F77_INT& n, const F77_CMPLX& alpha,
           const F77_CMPLX *x, const F77_INT& incx,
           F77_CMPLX *y, const F77_INT& incy)
{
  F77_FUNC (caxpy, CAXPY) (n, alpha, x, incx, y, incy);
}

// Overloaded versions for complex*real combinations

// complex<double> * double  - by promoting to complex
inline void
blas_axpy (const F77_INT& n, const F77_DBLE_CMPLX& alpha, const double *x,
           const F77_INT& incx, F77_DBLE_CMPLX *y, const F77_INT& incy)
{
  // Create a temporary complex array from x
  OCTAVE_LOCAL_BUFFER (F77_DBLE_CMPLX, cx, n);
  for (F77_INT i = 0; i < n; i++)
    cx[i] = F77_DBLE_CMPLX (x[i * incx]);

  // Use zaxpy with the complex temporary
  F77_FUNC (zaxpy, ZAXPY) (n, alpha, cx, 1, y, incy);
}

// complex<float> * float  - by promoting to complex
inline void
blas_axpy (const F77_INT& n, const F77_CMPLX& alpha, const float *x,
           const F77_INT& incx, F77_CMPLX *y, const F77_INT& incy)
{
  // Create a temporary complex array from x
  OCTAVE_LOCAL_BUFFER (F77_CMPLX, cx, n);
  for (F77_INT i = 0; i < n; i++)
    cx[i] = F77_CMPLX (x[i * incx]);

  // Use caxpy with the complex temporary
  F77_FUNC (caxpy, CAXPY) (n, alpha, cx, 1, y, incy);
}

// Generic fallback for types without BLAS support
// Just use loops
template <typename T>
inline void
blas_axpy (const F77_INT& n, const T& alpha, const T *x,
           const F77_INT& incx, T *y, const F77_INT& incy)
{
  for (F77_INT i = 0; i < n; i++)
    y[i * incy] += alpha * x[i * incx];
}
// End AXPY specialization wrappers

// 2d convolution with a matrix kernel.
template <typename T, typename R>
static void
convolve_2d (const T *a, F77_INT ma, F77_INT na,
             const R *b, F77_INT mb, F77_INT nb,
             T *c, bool inner)
{
  if (inner)
    {
      // Inner convolution ("valid")
      const F77_INT len = ma - mb + 1;  // Pre-calculate length
      for (F77_INT k = 0; k < na - nb + 1; k++)
        for (F77_INT j = 0; j < nb; j++)
          for (F77_INT i = 0; i < mb; i++)
            {
              // Create a T value from R
              T b_val = static_cast<T>(b[i + j*mb]);

              // Call the appropriate blas_axpy function based on type T
              blas_axpy (len, b_val, &a[mb-i-1 + (k+nb-j-1)*ma], 1,
                         &c[k*len], 1);
            }
    }
	else
    {
      // Outer convolution ("full")
      const F77_INT len = ma + mb - 1;  // Pre-calculate length
      for (F77_INT k = 0; k < na; k++)
        for (F77_INT j = 0; j < nb; j++)
          for (F77_INT i = 0; i < mb; i++)
            {
              // Create a T value from R
              T b_val = static_cast<T>(b[i + j*mb]);

              // Call the appropriate blas_axpy function based on type T
              blas_axpy (ma, b_val, &a[k*ma], 1, &c[i + (j+k)*len], 1);
            }
    }
}

template <typename T, typename R>
void
convolve_nd (const T *a, const dim_vector& ad, const dim_vector& acd,
             const R *b, const dim_vector& bd, const dim_vector& bcd,
             T *c, const dim_vector& ccd, int nd, bool inner)
{
  if (nd == 2)
    {
      F77_INT ad0 = to_f77_int (ad(0));
      F77_INT ad1 = to_f77_int (ad(1));

      F77_INT bd0 = to_f77_int (bd(0));
      F77_INT bd1 = to_f77_int (bd(1));

      convolve_2d<T, R> (a, ad0, ad1, b, bd0, bd1, c, inner);
    }
  else
    {
      octave_idx_type ma = acd(nd-2);
      octave_idx_type na = ad(nd-1);
      octave_idx_type mb = bcd(nd-2);
      octave_idx_type nb = bd(nd-1);
      octave_idx_type ldc = ccd(nd-2);

      if (inner)
        {
          for (octave_idx_type ja = 0; ja < na - nb + 1; ja++)
            for (octave_idx_type jb = 0; jb < nb; jb++)
              convolve_nd<T, R> (a + ma*(ja+jb), ad, acd,
                                 b + mb*(nb-jb-1), bd, bcd,
                                 c + ldc*ja, ccd, nd-1, inner);
        }
      else
        {
          for (octave_idx_type ja = 0; ja < na; ja++)
            for (octave_idx_type jb = 0; jb < nb; jb++)
              convolve_nd<T, R> (a + ma*ja, ad, acd, b + mb*jb, bd, bcd,
                                 c + ldc*(ja+jb), ccd, nd-1, inner);
        }
    }
}

// Arbitrary convolutor.
template <typename T, typename R>
static MArray<T>
convolve (const MArray<T>& a, const MArray<R>& b, convn_type ct)
{
  if (a.isempty () || b.isempty ())
    return MArray<T> ();

  const int nd = std::max (a.ndims (), b.ndims ());
  const dim_vector adims = a.dims ().redim (nd);
  dim_vector apdims = a.dims ().redim (nd);         // permuted adims
  const dim_vector bdims = b.dims ().redim (nd);
  dim_vector cdims = dim_vector::alloc (nd);

  for (int i = 0; i < nd; i++)
    {
      if (ct == convn_valid)
        cdims(i) = std::max (adims(i) - bdims(i) + 1,
                             static_cast<octave_idx_type> (0));
      else
        cdims(i) = std::max (adims(i) + bdims(i) - 1,
                             static_cast<octave_idx_type> (0));
    }

  // "valid" shape can sometimes result in empty matrices which must avoid
  // calling Fortran code which does not expect this (bug #52067)
  if (cdims.numel () == 0)
    return MArray<T> (cdims);

  // Permute dimensions of a/b/c such that the dimensions of a are ordered
  // by decreasing number of elements (for efficiency in Fortran loops).
  Array<octave_idx_type> order (dim_vector (1, nd));
  for (int i = 0; i < nd; i++)
    order(i) = i;

  // Since the number of dimensions is nearly always small, it is faster
  // to sort them inline instead of calling octave_sort::sort ().
  bool reordered = false;
  for (int i = 0; i < nd; i++)
    for (int j = (i+1); j < nd; j++)
      if (apdims(i) < apdims(j))
        {
          std::swap (apdims(i), apdims(j));
          std::swap (cdims(i), cdims(j));
          std::swap (order(i), order(j));
          reordered = true;
        }

  // Initialize output based on the current order of cdims.
  MArray<T> c (cdims, T ());

  if (reordered)  // adims was reordered, so the inputs must be as well.
    {
      // Permute the inputs
      const MArray<T> ap = a.permute (order);
      const MArray<R> bp = b.permute (order);
      const dim_vector bpdims = bp.dims ().redim (nd);

      // Do convolution on the permuted arrays.
      convolve_nd<T, R> (ap.data (), apdims, apdims.cumulative (),
                         bp.data (), bpdims, bpdims.cumulative (),
                         c.rwdata (), cdims.cumulative (),
                         nd, ct == convn_valid);

      // Permute back to original order.
      c = c.ipermute (order);
    }
  else  // No reordering ==> no need to create permuted arrays.
    {
      // Do convolution on the original arrays.
      convolve_nd<T, R> (a.data (), adims, adims.cumulative (),
                         b.data (), bdims, bdims.cumulative (),
                         c.rwdata (), cdims.cumulative (),
                         nd, ct == convn_valid);
    }

  if (ct == convn_same)
    {
      // Pick the relevant part.
      Array<idx_vector> sidx (dim_vector (nd, 1));

      for (int i = 0; i < nd; i++)
        sidx(i) = idx_vector::make_range (bdims(i)/2, 1, adims(i));
      c = c.index (sidx);
    }

  return c;
}

#define CONV_DEFS(TPREF, RPREF)                                         \
  TPREF ## NDArray                                                      \
  convn (const TPREF ## NDArray& a, const RPREF ## NDArray& b,          \
         convn_type ct)                                                 \
  {                                                                     \
    return convolve (a, b, ct);                                         \
  }                                                                     \
  TPREF ## Matrix                                                       \
  convn (const TPREF ## Matrix& a, const RPREF ## Matrix& b,            \
         convn_type ct)                                                 \
  {                                                                     \
    return convolve (a, b, ct);                                         \
  }                                                                     \
  TPREF ## Matrix                                                       \
  convn (const TPREF ## Matrix& a, const RPREF ## ColumnVector& c,      \
         const RPREF ## RowVector& r, convn_type ct)                    \
  {                                                                     \
    return convolve (a, c * r, ct);                                     \
  }

CONV_DEFS (, )
CONV_DEFS (Complex, )
CONV_DEFS (Complex, Complex)
CONV_DEFS (Float, Float)
CONV_DEFS (FloatComplex, Float)
CONV_DEFS (FloatComplex, FloatComplex)

OCTAVE_END_NAMESPACE(octave)
