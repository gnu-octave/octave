// N-D Array  manipulations.
/*

Copyright (C) 1996, 1997, 2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include <cfloat>

#include <vector>

#include "Array-util.h"
#include "fNDArray.h"
#include "functor.h"
#include "mx-base.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "oct-locbuf.h"

#if defined (HAVE_FFTW3)
#include "oct-fftw.h"

FloatComplexNDArray
FloatNDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv (dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv (dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  const float *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::fft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv (dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv (dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  FloatComplexNDArray retval (*this);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::ifft (out + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const float *in = fortran_vec ();
  FloatComplexNDArray retval (dv);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  FloatComplexNDArray retval (*this);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::ifftNd (out + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const float *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::fftNd (in, out, rank, dv);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  FloatComplexNDArray tmp (*this);
  FloatComplex *in (tmp.fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else

extern "C"
{
  // Note that the original complex fft routines were not written for
  // float complex arguments.  They have been modified by adding an
  // implicit float precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const octave_idx_type&, FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const octave_idx_type&, FloatComplex*, FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const octave_idx_type&, FloatComplex*, FloatComplex*);
}

FloatComplexNDArray
FloatNDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();

  FloatComplexNDArray retval (dv);
  octave_idx_type npts = dv(dim);
  octave_idx_type nn = 4*npts+15;
  Array<FloatComplex> wsave (nn);
  FloatComplex *pwsave = wsave.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (FloatComplex, tmp, npts);

  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / npts;
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
  octave_idx_type dist = (stride == 1 ? npts : 1);

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type k = 0; k < nloop; k++)
    {
      for (octave_idx_type j = 0; j < howmany; j++)
	{
	  OCTAVE_QUIT;

	  for (octave_idx_type i = 0; i < npts; i++)
	    tmp[i] = elem((i + k*npts)*stride + j*dist);

	  F77_FUNC (cfftf, CFFTF) (npts, tmp, pwsave);

	  for (octave_idx_type i = 0; i < npts; i++)
	    retval ((i + k*npts)*stride + j*dist) = tmp[i];
	}
    }

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();

  FloatComplexNDArray retval (dv);
  octave_idx_type npts = dv(dim);
  octave_idx_type nn = 4*npts+15;
  Array<FloatComplex> wsave (nn);
  FloatComplex *pwsave = wsave.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (FloatComplex, tmp, npts);

  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / npts;
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
  octave_idx_type dist = (stride == 1 ? npts : 1);

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type k = 0; k < nloop; k++)
    {
      for (octave_idx_type j = 0; j < howmany; j++)
	{
	  OCTAVE_QUIT;

	  for (octave_idx_type i = 0; i < npts; i++)
	    tmp[i] = elem((i + k*npts)*stride + j*dist);

	  F77_FUNC (cfftb, CFFTB) (npts, tmp, pwsave);

	  for (octave_idx_type i = 0; i < npts; i++)
	    retval ((i + k*npts)*stride + j*dist) = tmp[i] / 
	      static_cast<float> (npts);
	}
    }

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourier2d (void) const
{
  dim_vector dv = dims();
  dim_vector dv2 (dv(0), dv(1));
  int rank = 2;
  FloatComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv2(i);
      octave_idx_type nn = 4*npts+15;
      Array<FloatComplex> wsave (nn);
      FloatComplex *pwsave = wsave.fortran_vec ();
      Array<FloatComplex> row (npts);
      FloatComplex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (cffti, CFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (cfftf, CFFTF) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l];
	    }
	}

      stride *= dv2(i);
    }

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  dim_vector dv2 (dv(0), dv(1));
  int rank = 2;
  FloatComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv2(i);
      octave_idx_type nn = 4*npts+15;
      Array<FloatComplex> wsave (nn);
      FloatComplex *pwsave = wsave.fortran_vec ();
      Array<FloatComplex> row (npts);
      FloatComplex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (cffti, CFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (cfftb, CFFTB) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l] / 
		  static_cast<float> (npts);
	    }
	}

      stride *= dv2(i);
    }

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();
  FloatComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv(i);
      octave_idx_type nn = 4*npts+15;
      Array<FloatComplex> wsave (nn);
      FloatComplex *pwsave = wsave.fortran_vec ();
      Array<FloatComplex> row (npts);
      FloatComplex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (cffti, CFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (cfftf, CFFTF) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l];
	    }
	}

      stride *= dv(i);
    }

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();
  FloatComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv(i);
      octave_idx_type nn = 4*npts+15;
      Array<FloatComplex> wsave (nn);
      FloatComplex *pwsave = wsave.fortran_vec ();
      Array<FloatComplex> row (npts);
      FloatComplex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (cffti, CFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (cfftb, CFFTB) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l] /
		  static_cast<float> (npts);
	    }
	}

      stride *= dv(i);
    }

  return retval;
}

#endif

// unary operations

boolNDArray
FloatNDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (octave_idx_type i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

bool
FloatNDArray::any_element_is_negative (bool neg_zero) const
{
  octave_idx_type nel = nelem ();

  if (neg_zero)
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (lo_ieee_signbit (elem (i)))
	  return true;
    }
  else
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (elem (i) < 0)
	  return true;
    }

  return false;
}

bool
FloatNDArray::any_element_is_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);
      if (xisnan (val))
	return true;
    }

  return false;
}

bool
FloatNDArray::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
FloatNDArray::any_element_not_one_or_zero (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);
      if (val != 0 && val != 1)
	return true;
    }

  return false;
}

bool
FloatNDArray::all_elements_are_zero (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    if (elem (i) != 0)
      return false;

  return true;
}

bool
FloatNDArray::all_elements_are_int_or_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);
      if (xisnan (val) || D_NINT (val) == val)
	continue;
      else
	return false;
    }

  return true;
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
FloatNDArray::all_integers (float& max_val, float& min_val) const
{
  octave_idx_type nel = nelem ();

  if (nel > 0)
    {
      max_val = elem (0);
      min_val = elem (0);
    }
  else
    return false;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);

      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}

bool
FloatNDArray::too_large_for_float (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float val = elem (i);

      if (! (xisnan (val) || xisinf (val))
	  && fabs (val) > FLT_MAX)
	return true;
    }

  return false;
}

// FIXME -- this is not quite the right thing.

boolNDArray
FloatNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
FloatNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ANY_EVAL (elem (iter_idx) != 0
		     && ! lo_ieee_isnan (elem (iter_idx))), false);
}

FloatNDArray
FloatNDArray::cumprod (int dim) const
{
  MX_ND_CUMULATIVE_OP (FloatNDArray, float, 1, *);
}

FloatNDArray
FloatNDArray::cumsum (int dim) const
{
  MX_ND_CUMULATIVE_OP (FloatNDArray, float, 0, +);
}

FloatNDArray
FloatNDArray::prod (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) *= elem (iter_idx), 1, FloatNDArray);
}

FloatNDArray
FloatNDArray::sumsq (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) += std::pow (elem (iter_idx), 2), 0, FloatNDArray);
}

FloatNDArray 
FloatNDArray::sum (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) += elem (iter_idx), 0, FloatNDArray);
}

FloatNDArray
FloatNDArray::max (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

FloatNDArray
FloatNDArray::max (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return FloatNDArray ();
  
  dr(dim) = 1;

  FloatNDArray result (dr);
  idx_arg.resize (dr);

  octave_idx_type x_stride = 1;
  octave_idx_type x_len = dv(dim);
  for (int i = 0; i < dim; i++)
    x_stride *= dv(i);

  for (octave_idx_type i = 0; i < dr.numel (); i++)
    {
      octave_idx_type x_offset;
      if (x_stride == 1)
	x_offset = i * x_len;
      else
	{
	  octave_idx_type x_offset2 = 0;
	  x_offset = i;
	  while (x_offset >= x_stride)
	    {
	      x_offset -= x_stride;
	      x_offset2++;
	    }
	  x_offset += x_offset2 * x_stride * x_len;
	}

      octave_idx_type idx_j;

      float tmp_max = octave_Float_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_max = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_max))
	    break;
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  float tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;
	  else if (tmp > tmp_max)
	    {
	      idx_j = j;
	      tmp_max = tmp;
	    }
	}

      result.elem (i) = tmp_max;
      idx_arg.elem (i) = xisnan (tmp_max) ? 0 : idx_j;
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

FloatNDArray
FloatNDArray::min (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

FloatNDArray
FloatNDArray::min (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return FloatNDArray ();
  
  dr(dim) = 1;

  FloatNDArray result (dr);
  idx_arg.resize (dr);

  octave_idx_type x_stride = 1;
  octave_idx_type x_len = dv(dim);
  for (int i = 0; i < dim; i++)
    x_stride *= dv(i);

  for (octave_idx_type i = 0; i < dr.numel (); i++)
    {
      octave_idx_type x_offset;
      if (x_stride == 1)
	x_offset = i * x_len;
      else
	{
	  octave_idx_type x_offset2 = 0;
	  x_offset = i;
	  while (x_offset >= x_stride)
	    {
	      x_offset -= x_stride;
	      x_offset2++;
	    }
	  x_offset += x_offset2 * x_stride * x_len;
	}

      octave_idx_type idx_j;

      float tmp_min = octave_Float_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_min = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_min))
	    break;
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  float tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;
	  else if (tmp < tmp_min)
	    {
	      idx_j = j;
	      tmp_min = tmp;
	    }
	}

      result.elem (i) = tmp_min;
      idx_arg.elem (i) = xisnan (tmp_min) ? 0 : idx_j;
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

FloatNDArray
FloatNDArray::concat (const FloatNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

FloatComplexNDArray
FloatNDArray::concat (const FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  FloatComplexNDArray retval (*this);
  if (rb.numel () > 0)
    retval.insert (rb, ra_idx);
  return retval;
}

charNDArray
FloatNDArray::concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  charNDArray retval (dims ());
  octave_idx_type nel = numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float d = elem (i);

      if (xisnan (d))
	{
	  (*current_liboctave_error_handler)
	    ("invalid conversion from NaN to character");
	  return retval;
	}
      else
	{
	  octave_idx_type ival = NINTbig (d);

	  if (ival < 0 || ival > UCHAR_MAX)
	    // FIXME -- is there something
	    // better we could do? Should we warn the user?
	    ival = 0;

	  retval.elem (i) = static_cast<char>(ival);
	}
    }

  if (rb.numel () == 0)
    return retval;

  retval.insert (rb, ra_idx);
  return retval;
}

FloatNDArray
real (const FloatComplexNDArray& a)
{
  return FloatNDArray (mx_inline_real_dup (a.data (), a.length ()),
                       a.dims ());
}

FloatNDArray
imag (const FloatComplexNDArray& a)
{
  return FloatNDArray (mx_inline_imag_dup (a.data (), a.length ()),
                       a.dims ());
}

FloatNDArray&
FloatNDArray::insert (const FloatNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<float>::insert (a, r, c);
  return *this;
}

FloatNDArray&
FloatNDArray::insert (const FloatNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<float>::insert (a, ra_idx);
  return *this;
}

FloatNDArray
FloatNDArray::abs (void) const
{
  return FloatNDArray (mx_inline_fabs_dup (data (), length ()),
                       dims ());
}

Matrix
FloatNDArray::matrix_value (void) const
{
  Matrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = Matrix (Array2<float> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = Matrix (Array2<float> (*this, dimensions(0), dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of FloatNDArray to Matrix");
      break;
    }

  return retval;
}

void
FloatNDArray::increment_index (Array<octave_idx_type>& ra_idx,
			  const dim_vector& dimensions,
			  int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type
FloatNDArray::compute_index (Array<octave_idx_type>& ra_idx,
			const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

FloatNDArray
FloatNDArray::diag (octave_idx_type k) const
{
  return MArrayN<float>::diag (k);
}

FloatNDArray
FloatNDArray::map (dmapper fcn) const
{
  return MArrayN<float>::map<float> (func_ptr (fcn));
}

FloatComplexNDArray
FloatNDArray::map (cmapper fcn) const
{
  return MArrayN<float>::map<FloatComplex> (func_ptr (fcn));
}

boolNDArray
FloatNDArray::map (bmapper fcn) const
{
  return MArrayN<float>::map<bool> (func_ptr (fcn));
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const FloatNDArray& a)
{
  octave_idx_type nel = a.nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      os << " ";
      octave_write_float (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, FloatNDArray& a)
{
  octave_idx_type nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      float tmp;
      for (octave_idx_type i = 0; i < nel; i++)
	  {
	    tmp = octave_read_float (is);
	    if (is)
	      a.elem (i) = tmp;
	    else
	      goto done;
	  }
    }

 done:

  return is;
}

// FIXME -- it would be nice to share code among the min/max
// functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nel == 0)	\
    return T (dv);

FloatNDArray
min (float d, const FloatNDArray& m)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (d, m (i));
    }

  return result;
}

FloatNDArray
min (const FloatNDArray& m, float d)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (d, m (i));
    }

  return result;
}

FloatNDArray
min (const FloatNDArray& a, const FloatNDArray& b)
{
  dim_vector dv = a.dims ();
  octave_idx_type nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return FloatNDArray ();
    }

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (a (i), b (i));
    }

  return result;
}

FloatNDArray
max (float d, const FloatNDArray& m)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (d, m (i));
    }

  return result;
}

FloatNDArray
max (const FloatNDArray& m, float d)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (d, m (i));
    }

  return result;
}

FloatNDArray
max (const FloatNDArray& a, const FloatNDArray& b)
{
  dim_vector dv = a.dims ();
  octave_idx_type nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return FloatNDArray ();
    }

  EMPTY_RETURN_CHECK (FloatNDArray);

  FloatNDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (a (i), b (i));
    }

  return result;
}

NDS_CMP_OPS(FloatNDArray, , float, )
NDS_BOOL_OPS(FloatNDArray, float, static_cast<float> (0.0))

SND_CMP_OPS(float, , FloatNDArray, )
SND_BOOL_OPS(float, FloatNDArray, static_cast<float> (0.0))

NDND_CMP_OPS(FloatNDArray, , FloatNDArray, )
NDND_BOOL_OPS(FloatNDArray, FloatNDArray, static_cast<float> (0.0))

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
