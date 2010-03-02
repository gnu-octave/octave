/*

Copyright (C) 2010 VZLU Prague

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <algorithm>
#include "oct-convn.h"
#include "oct-locbuf.h"


// FIXME: Only the axpy-form accumulation is used. This is natural for outer
// convolution as it requires no boundary treatment.
// This typically requires one more memory store per operation, but as the
// memory access pattern is equivalent (switching ro and rw parts), I wouldn't
// expect a significant difference. cf. for instance sum(), where row-wise sum
// (axpy-based) is no slower than column-wise (dot-based).
// It would be nice, however, if the inner convolution was computed directly by
// dot-based accumulation.

// FIXME: Specifying the kernel as outer product should probably get special
// treatment.

// All kernels smaller than 7x5 get specialized code.
#define MAX_KERNEL_SIZE_M 7
#define MAX_KERNEL_SIZE_N 5

template <class T, class R, int mb, int nb>
static void 
convolve_2d_kernel_axpy (const T *a, octave_idx_type ma, octave_idx_type na,
                         const R *b, T *c, octave_idx_type ldc)
{
  for (octave_idx_type ja = 0; ja < na; ja++)
    for (octave_idx_type ia = 0; ia < ma; ia++)
      {
        T aij = a[ma*ja + ia];
        // The following double loop is a candidate for complete unrolling.
        for (int jb = 0; jb < nb; jb++)
          for (int ib = 0; ib < mb; ib++)
            c[(ja+jb)*ldc + (ia+ib)] += aij * b[mb*jb+ib];
      }
}

// Kernel dispatcher.
template <class T, class R>
static void
convolve_2d_axpy (const T *a, octave_idx_type ma, octave_idx_type na,
                  const R *b, octave_idx_type mb, octave_idx_type nb,
                  T *c, octave_idx_type ldc)
{
  // Table of kernels.
  static void (*table[MAX_KERNEL_SIZE_M][MAX_KERNEL_SIZE_N]) 
    (const T *, octave_idx_type, octave_idx_type, const R *, T *, octave_idx_type)
    = {
        // This must be repeated MAX_KERNEL_SIZE-times. I do not see a way to
        // automate this.
#define STORE_KERNEL_POINTER(M,N) \
        convolve_2d_kernel_axpy<T,R,M,N>
#define STORE_KERNEL_ROW(M) \
          { \
            STORE_KERNEL_POINTER(M,1), \
            STORE_KERNEL_POINTER(M,2), \
            STORE_KERNEL_POINTER(M,3), \
            STORE_KERNEL_POINTER(M,4), \
            STORE_KERNEL_POINTER(M,5), \
          }

        STORE_KERNEL_ROW(1),
        STORE_KERNEL_ROW(2),
        STORE_KERNEL_ROW(3),
        STORE_KERNEL_ROW(4),
        STORE_KERNEL_ROW(5),
        STORE_KERNEL_ROW(6),
        STORE_KERNEL_ROW(7),
    };

  if (mb != 0 && nb != 0)
    (*table[mb-1][nb-1]) (a, ma, na, b, c, ldc);
}

// 2d convolution with a matrix kernel.
template <class T, class R>
static void 
convolve_2d (const T *a, octave_idx_type ma, octave_idx_type na,
             const R *b, octave_idx_type mb, octave_idx_type nb,
             T *c)
{
  octave_idx_type ldc = ma + mb - 1;
  if (mb <= MAX_KERNEL_SIZE_M && nb <= MAX_KERNEL_SIZE_N)
    {
      // Call kernel directly on b.
      convolve_2d_axpy (a, ma, na, b, mb, nb, c, ldc);
    }
  else
    {
      // Split b to blocks.
      OCTAVE_LOCAL_BUFFER (R, b1, MAX_KERNEL_SIZE_M*MAX_KERNEL_SIZE_N);
      for (octave_idx_type jb = 0; jb < nb; jb += MAX_KERNEL_SIZE_N)
        {
          octave_idx_type nb1 = std::min (nb - jb, MAX_KERNEL_SIZE_N);
          for (octave_idx_type ib = 0; ib < mb; ib += MAX_KERNEL_SIZE_M)
            {
              octave_idx_type mb1 = std::min (mb - ib, MAX_KERNEL_SIZE_M);

              // Copy block to buffer.
              R *bf = b1;
              for (octave_idx_type j = jb; j < jb+nb1; j++)
                for (octave_idx_type i = ib; i < ib+mb1; i++)
                  *bf++ = b[j*mb + i];

              // Call kernel.
              convolve_2d_axpy (a, ma, na, b1, mb1, nb1, c + ldc*jb + ib, ldc);
            }
        }
    }

}

template <class T, class R>
void convolve_nd (const T *a, const dim_vector& ad, const dim_vector& acd,
                  const R *b, const dim_vector& bd, const dim_vector& bcd,
                  T *c, const dim_vector& ccd, int nd)
{
  if (nd == 2)
    convolve_2d<T, R> (a, ad(0), ad(1), b, bd(0), bd(1), c);
  else
    {
      octave_idx_type ma = acd(nd-2), na = ad(nd-1), mb = bcd(nd-2), nb = bd(nd-1);
      octave_idx_type ldc = ccd(nd-2);
      for (octave_idx_type jb = 0; jb < nb; jb++)
        {
          for (octave_idx_type ja = 0; ja < na; ja++)
            convolve_nd<T, R> (a + ma*ja, ad, acd, b + mb*jb, bd, bcd, 
                               c + ldc*(ja+jb), ccd, nd-1);
        }
    }
}

// Arbitrary convolutor. 
// The 2nd array is assumed to be the smaller one.
template <class T, class R>
static MArray<T>
convolve (const MArray<T>& a, const MArray<R>& b,
          convn_type ct)
{
  if (a.is_empty () || b.is_empty ())
    return MArray<T> ();

  int nd = std::max (a.ndims (), b.ndims ());
  const dim_vector adims = a.dims ().redim (nd), bdims = b.dims ().redim (nd);
  dim_vector cdims = dim_vector::alloc (nd);

  for (int i = 0; i < nd; i++)
    cdims(i) = std::max (adims(i) + bdims(i) - 1, 0);

  MArray<T> c (cdims, T());

  convolve_nd<T, R> (a.fortran_vec (), adims, adims.cumulative (),
                     b.fortran_vec (), bdims, bdims.cumulative (),
                     c.fortran_vec (), cdims.cumulative (), nd);

  // Pick the relevant part.
  Array<idx_vector> sidx (nd, 1);

  switch (ct)
    {
    case convn_valid:
        {
          for (int i = 0; i < nd; i++)
            sidx(i) = idx_vector (bdims(i)-1, adims(i));
          c = c.index (sidx);
          break;
        }
    case convn_same:
        {
          for (int i = 0; i < nd; i++)
            sidx(i) = idx_vector::make_range ((bdims(i)-1)/2, 1, adims(i));
          c = c.index (sidx);
          break;
        }
    default:
      break;
    }

  return c;
}

#define CONV_DEFS(TPREF, RPREF) \
TPREF ## NDArray \
convn (const TPREF ## NDArray& a, const RPREF ## NDArray& b, convn_type ct) \
{ \
  return convolve (a, b, ct); \
} \
TPREF ## Matrix \
convn (const TPREF ## Matrix& a, const RPREF ## Matrix& b, convn_type ct) \
{ \
  return convolve (a, b, ct); \
} \
TPREF ## Matrix \
convn (const TPREF ## Matrix& a, const RPREF ## ColumnVector& c, \
       const RPREF ## RowVector& r, convn_type ct) \
{ \
  return convolve (a, c * r, ct); \
}

CONV_DEFS ( , )
CONV_DEFS (Complex, )
CONV_DEFS (Complex, Complex)
CONV_DEFS (Float, Float)
CONV_DEFS (FloatComplex, Float)
CONV_DEFS (FloatComplex, FloatComplex)
