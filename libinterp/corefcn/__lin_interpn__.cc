////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include "lo-ieee.h"
#include "dNDArray.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"

#include <type_traits>

OCTAVE_BEGIN_NAMESPACE(octave)

// equivalent to isvector.m

template <typename T>
bool
isvector (const T& array)
{
  const dim_vector dv = array.dims ();
  return dv.ndims () == 2 && (dv(0) == 1 || dv(1) == 1);
}

// lookup a value in a sorted table (lookup.m)
template <typename T>
octave_idx_type
lookup (const T *x, octave_idx_type n, T y)
{
  octave_idx_type j;

  if (x[0] < x[n-1])
    {
      // increasing x

      if (y > x[n-1] || y < x[0])
        return -1;

#if defined (EXHAUSTIF)
      for (j = 0; j < n - 1; j++)
        {
          if (x[j] <= y && y <= x[j+1])
            return j;
        }
#else
      octave_idx_type j0 = 0;
      octave_idx_type j1 = n - 1;

      while (true)
        {
          j = (j0+j1)/2;

          if (y <= x[j+1])
            {
              if (x[j] <= y)
                return j;

              j1 = j;
            }

          if (x[j] <= y)
            j0 = j;
        }
#endif
    }
  else
    {
      // decreasing x
      // previous code with x -> -x and y -> -y

      if (y > x[0] || y < x[n-1])
        return -1;

#if defined (EXHAUSTIF)
      for (j = 0; j < n - 1; j++)
        {
          if (x[j+1] <= y && y <= x[j])
            return j;
        }
#else
      octave_idx_type j0 = 0;
      octave_idx_type j1 = n - 1;

      while (true)
        {
          j = (j0+j1)/2;

          if (y >= x[j+1])
            {
              if (x[j] >= y)
                return j;

              j1 = j;
            }

          if (x[j] >= y)
            j0 = j;
        }
#endif
    }
}

// n-dimensional linear interpolation

template <typename T, typename DT>
void
lin_interpn (int n, const octave_idx_type *size, const octave_idx_type *scale,
             octave_idx_type Ni, DT extrapval, const T **x,
             const DT *v, const T **y, DT *vi)
{
  bool out = false;
  int bit;

  OCTAVE_LOCAL_BUFFER (T, coef, 2*n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, index, n);

  // loop over all points
  for (octave_idx_type m = 0; m < Ni; m++)
    {
      // loop over all dimensions
      for (int i = 0; i < n; i++)
        {
          index[i] = lookup (x[i], size[i], y[i][m]);
          out = index[i] == -1;

          if (out)
            break;
          else
            {
              octave_idx_type j = index[i];
              coef[2*i+1] = (y[i][m] - x[i][j])/(x[i][j+1] - x[i][j]);
              coef[2*i] = 1 - coef[2*i+1];
            }
        }

      if (out)
        vi[m] = extrapval;
      else
        {
          vi[m] = 0;

          // loop over all corners of hypercube (1<<n = 2^n)
          for (int i = 0; i < (1 << n); i++)
            {
              T c = 1;
              octave_idx_type l = 0;

              // loop over all dimensions
              for (int j = 0; j < n; j++)
                {
                  // test if the jth bit in i is set
                  bit = i >> j & 1;
                  l += scale[j] * (index[j] + bit);
                  c *= coef[2*j+bit];
                }

              vi[m] += c * v[l];
            }
        }
    }
}

template <typename MT, typename DMT, typename DT>
octave_value
lin_interpn (int n, MT *X, const DMT V, MT *Y, DT extrapval)
{
  static_assert(std::is_same<DT, typename DMT::element_type>::value,
                "Type DMT must be an ArrayType with elements of type DT.");
  using T = typename MT::element_type;

  octave_value retval;

  DMT Vi = DMT (Y[0].dims ());

  OCTAVE_LOCAL_BUFFER (const T *, y, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, size, n);

  for (int i = 0; i < n; i++)
    {
      y[i] = Y[i].data ();
      size[i] = V.dims ()(i);
    }

  OCTAVE_LOCAL_BUFFER (const T *, x, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, scale, n);

  const DT *v = V.data ();
  DT *vi = Vi.fortran_vec ();
  octave_idx_type Ni = Vi.numel ();

  // offset in memory of each dimension

  scale[0] = 1;

  for (int i = 1; i < n; i++)
    scale[i] = scale[i-1] * size[i-1];

  // tests if X[0] is a vector, if yes, assume that all elements of X are
  // in the ndgrid format.

  if (! isvector (X[0]))
    {
      for (int i = 0; i < n; i++)
        {
          if (X[i].dims () != V.dims ())
            error ("interpn: incompatible size of argument number %d", i+1);

          MT tmp = MT (dim_vector (size[i], 1));

          for (octave_idx_type j = 0; j < size[i]; j++)
            tmp(j) = X[i](scale[i]*j);

          X[i] = tmp;
        }
    }

  for (int i = 0; i < n; i++)
    {
      if (! isvector (X[i]) && X[i].numel () != size[i])
        error ("interpn: incompatible size of argument number %d", i+1);

      x[i] = X[i].data ();
    }

  lin_interpn (n, size, scale, Ni, extrapval, x, v, y, vi);

  retval = Vi;

  return retval;
}

// Perform @var{n}-dimensional interpolation.  Each element of then
// @var{n}-dimensional array @var{v} represents a value at a location
// given by the parameters @var{x1}, @var{x2},...,@var{xn}.  The parameters
// @var{x1}, @var{x2}, @dots{}, @var{xn} are either @var{n}-dimensional
// arrays of the same size as the array @var{v} in the "ndgrid" format
// or vectors.  The parameters @var{y1}, @var{y2}, @dots{}, @var{yn} are
// all @var{n}-dimensional arrays of the same size and represent the
// points at which the array @var{vi} is interpolated.
//
//This function only performs linear interpolation.

DEFUN (__lin_interpn__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{vi} =} __lin_interpn__ (@var{x1}, @var{x2}, @dots{}, @var{xn}, @var{v}, @var{y1}, @var{y2}, @dots{}, @var{yn})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin % 2 == 0)
    print_usage ();

  octave_value retval;

  // dimension of the problem
  int n = (nargin-1)/2;

  if (args(n).is_single_type ())
    {
      OCTAVE_LOCAL_BUFFER (FloatNDArray, X, n);
      OCTAVE_LOCAL_BUFFER (FloatNDArray, Y, n);

      for (int i = 0; i < n; i++)
        {
          X[i] = args(i).float_array_value ();
          Y[i] = args(n+i+1).float_array_value ();

          if (Y[0].dims () != Y[i].dims ())
            error ("interpn: incompatible size of argument number %d", n+i+2);
        }

      if (args(n).iscomplex ())
        {
          const FloatComplexNDArray V = args(n).float_complex_array_value ();
          FloatComplex extrapval (octave_NA, octave_NA);
          retval = lin_interpn (n, X, V, Y, extrapval);
        }
      else
        {
          const FloatNDArray V = args(n).float_array_value ();
          float extrapval = octave_NA;
          retval = lin_interpn (n, X, V, Y, extrapval);
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (NDArray, X, n);
      OCTAVE_LOCAL_BUFFER (NDArray, Y, n);

      for (int i = 0; i < n; i++)
        {
          X[i] = args(i).array_value ();
          Y[i] = args(n+i+1).array_value ();

          if (Y[0].dims () != Y[i].dims ())
            error ("interpn: incompatible size of argument number %d", n+i+2);
        }

      if (args(n).iscomplex ())
        {
          const ComplexNDArray V = args(n).complex_array_value ();
          Complex extrapval (octave_NA, octave_NA);
          retval = lin_interpn (n, X, V, Y, extrapval);
        }
      else
        {
          const NDArray V = args(n).array_value ();
          double extrapval = octave_NA;
          retval = lin_interpn (n, X, V, Y, extrapval);
        }
    }

  return retval;
}

/*
## Test that real and imaginary parts are interpolated the same way
## and outer points are set to NA + 1i*NA
%!test <*61907>
%! x1 = 1:3;
%! x2 = 1:4;
%! v = repmat(1:4, 3, 1) + 1i * repmat((1:3)', 1, 4);
%! [XI2, XI1] = meshgrid(1.5:3.5, 1.5:3.5);
%! vi_complex = __lin_interpn__ (x1, x2, v, XI1, XI2);
%! vi_real = __lin_interpn__ (x1, x2, real (v), XI1, XI2);
%! vi_imag = __lin_interpn__ (x1, x2, imag (v), XI1, XI2);
%! assert (real (vi_complex), vi_real);
%! assert (imag (vi_complex), vi_imag);
*/

OCTAVE_END_NAMESPACE(octave)
