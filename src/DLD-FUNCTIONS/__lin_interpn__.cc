/*

Copyright (C) 2007 Alexander Barth

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

#include "dNDArray.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

// equivalent to isvector.m

bool
isvector (const NDArray& array)
{
  const dim_vector dv = array.dims ();
  return dv.length () == 2 && (dv(0) == 1 || dv(1) == 1);
}

// lookup a value in a sorted table (lookup.m)
octave_idx_type
lookup (const double *x, octave_idx_type n, double y)
{
  octave_idx_type j, j0, j1;

  if (y > x[n-1] || y < x[0])
    return -1;

#ifdef EXHAUSTIF
  for (j = 0; j < n - 1; j++)
    {
      if (x[j] <= y && y <= x[j+1])
	return j;
    }
#else
  j0 = 0;
  j1 = n - 1;

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

// n-dimensional linear interpolation

void
lin_interpn (int n, const octave_idx_type *size, const octave_idx_type *scale,
	     octave_idx_type Ni, double extrapval, const double **x,
	     const double *v, const double **y, double *vi)
{
  bool out = false;
  int bit;

  OCTAVE_LOCAL_BUFFER (double, coef, 2*n);
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
	      double c = 1;
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

// Perform @var{n}-dimensional interpolation.  Each element of then
// @var{n}-dimensional array @var{v} represents a value at a location
// given by the parameters @var{x1}, @var{x2},...,@var{xn}. The parameters
// @var{x1}, @var{x2}, @dots{}, @var{xn} are either @var{n}-dimensional
// arrays of the same size as the array @var{v} in the \"ndgrid\" format
// or vectors.  The parameters @var{y1}, @var{y2}, @dots{}, @var{yn} are
// all @var{n}-dimensional arrays of the same size and represent the
// points at which the array @var{vi} is interpolated.
//
//This function only performs linear interpolation.

DEFUN_DLD (__lin_interpn__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{vi} =} __lin_interpn__ (@var{x1}, @var{x2}, @dots{}, @var{xn}, @var{v}, @var{y1}, @var{y2}, @dots{}, @var{yn})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 2 ||  nargin % 2 == 0)
    {
      print_usage ();
      return retval;
    }

  // dimension of the problem
  int n = (nargin-1)/2;

  OCTAVE_LOCAL_BUFFER (NDArray, X, n);
  OCTAVE_LOCAL_BUFFER (NDArray, Y, n);

  OCTAVE_LOCAL_BUFFER (const double *, x, n);
  OCTAVE_LOCAL_BUFFER (const double *, y, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, scale, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, size, n);

  const NDArray V = args(n).array_value ();
  NDArray Vi = NDArray (args(n+1).dims ());

  if (error_state)
    {
      print_usage ();
      return retval;
    }

  const double *v = V.data ();
  double *vi = Vi.fortran_vec ();
  octave_idx_type Ni = Vi.numel ();

  double extrapval = octave_NA;

  for (int i = 0; i < n; i++)
    {
      X[i] = args(i).array_value ();
      Y[i] = args(n+i+1).array_value ();

      if (error_state)
	{
	  print_usage ();
	  return retval;
	}

      y[i] = Y[i].data ();
      size[i] =  V.dims()(i);

      if (Y[0].dims () != Y[i].dims ())
	{
	  error ("interpn: incompatible size of argument number %d", n+i+2);
	  return retval;
	}
    }

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
	    {
	      error ("interpn: incompatible size of argument number %d", i+1);
	      return retval;
	    }
	  else
	    {
              NDArray tmp = NDArray (dim_vector (size[i], 1));

	      for (octave_idx_type j = 0; j < size[i]; j++)
		tmp(j) =  X[i](scale[i]*j);

              X[i] = tmp;
	    }
	}
    }

  for (int i = 0; i < n; i++)
    {
      if (! isvector (X[i]) && X[i].numel () != size[i])
	{
	  error ("interpn: incompatible size of argument number %d", i+1);
	  return retval;
	}
      else
	x[i] = X[i].data ();
    }

  lin_interpn (n, size, scale, Ni, extrapval, x, v, y, vi);

  retval = Vi;

  return retval;
}
