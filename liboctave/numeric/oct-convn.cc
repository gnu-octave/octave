////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
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
#include "oct-convn.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// 2d convolution with a matrix kernel.
template <typename T, typename R>
static void
convolve_2d (const T *a, F77_INT ma, F77_INT na,
             const R *b, F77_INT mb, F77_INT nb,
             T *c, bool inner);

// Forward instances to our Fortran implementations.
#define FORWARD_IMPL(T_CXX, R_CXX, T, R, T_CAST, T_CONST_CAST,          \
                     R_CONST_CAST, f, F)                                \
  extern "C"                                                            \
  F77_RET_T                                                             \
  F77_FUNC (f##conv2o, F##CONV2O) (const F77_INT&, const F77_INT&,      \
                                   const T*, const F77_INT&,            \
                                   const F77_INT&, const R*, T *);      \
                                                                        \
  extern "C"                                                            \
  F77_RET_T                                                             \
  F77_FUNC (f##conv2i, F##CONV2I) (const F77_INT&, const F77_INT&,      \
                                   const T*, const F77_INT&,            \
                                   const F77_INT&, const R*, T *);      \
                                                                        \
  template <> void                                                      \
  convolve_2d<T_CXX, R_CXX> (const T_CXX *a, F77_INT ma, F77_INT na,    \
                             const R_CXX *b, F77_INT mb, F77_INT nb,    \
                             T_CXX *c, bool inner)                      \
  {                                                                     \
    if (inner)                                                          \
      F77_XFCN (f##conv2i, F##CONV2I, (ma, na, T_CONST_CAST (a),        \
                                       mb, nb, R_CONST_CAST (b),        \
                                       T_CAST (c)));                    \
    else                                                                \
      F77_XFCN (f##conv2o, F##CONV2O, (ma, na, T_CONST_CAST (a),        \
                                       mb, nb, R_CONST_CAST (b),        \
                                       T_CAST (c)));                    \
  }

FORWARD_IMPL (double, double, F77_DBLE, F77_DBLE,,,, d, D)
FORWARD_IMPL (float, float, F77_REAL, F77_REAL,,,, s, S)

FORWARD_IMPL (std::complex<double>, std::complex<double>,
              F77_DBLE_CMPLX, F77_DBLE_CMPLX, F77_DBLE_CMPLX_ARG,
              F77_CONST_DBLE_CMPLX_ARG, F77_CONST_DBLE_CMPLX_ARG, z, Z)
FORWARD_IMPL (std::complex<float>, std::complex<float>,
              F77_CMPLX, F77_CMPLX, F77_CMPLX_ARG,
              F77_CONST_CMPLX_ARG, F77_CONST_CMPLX_ARG, c, C)

FORWARD_IMPL (std::complex<double>, double,
              F77_DBLE_CMPLX, F77_DBLE, F77_DBLE_CMPLX_ARG,
              F77_CONST_DBLE_CMPLX_ARG,, zd, ZD)
FORWARD_IMPL (std::complex<float>, float, F77_CMPLX, F77_REAL, F77_CMPLX_ARG,
              F77_CONST_CMPLX_ARG,, cs, CS)

template <typename T, typename R>
void convolve_nd (const T *a, const dim_vector& ad, const dim_vector& acd,
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
// The 2nd array is assumed to be the smaller one.
template <typename T, typename R>
static MArray<T>
convolve (const MArray<T>& a, const MArray<R>& b,
          convn_type ct)
{
  if (a.isempty () || b.isempty ())
    return MArray<T> ();

  int nd = std::max (a.ndims (), b.ndims ());
  const dim_vector adims = a.dims ().redim (nd);
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

  MArray<T> c (cdims, T ());

  // "valid" shape can sometimes result in empty matrices which must avoid
  // calling Fortran code which does not expect this (bug #52067)
  if (c.isempty ())
    return c;

  convolve_nd<T, R> (a.data (), adims, adims.cumulative (),
                     b.data (), bdims, bdims.cumulative (),
                     c.fortran_vec (), cdims.cumulative (),
                     nd, ct == convn_valid);

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
