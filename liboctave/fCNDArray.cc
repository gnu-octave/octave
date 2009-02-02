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
#include "fCNDArray.h"
#include "mx-base.h"
#include "f77-fcn.h"
#include "functor.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "oct-locbuf.h"

#if defined (HAVE_FFTW3)
#include "oct-fftw.h"
#else
extern "C"
{
  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const octave_idx_type&, FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const octave_idx_type&, FloatComplex*, FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const octave_idx_type&, FloatComplex*, FloatComplex*);
}
#endif

#if defined (HAVE_FFTW3)
FloatComplexNDArray
FloatComplexNDArray::fourier (int dim) const
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

  const FloatComplex *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::fft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatComplexNDArray::ifourier (int dim) const
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

  const FloatComplex *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::ifft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatComplexNDArray::fourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const FloatComplex *in = fortran_vec ();
  FloatComplexNDArray retval (dv);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatComplexNDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const FloatComplex *in = fortran_vec ();
  FloatComplexNDArray retval (dv);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::ifftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatComplexNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const FloatComplex *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::fftNd (in, out, rank, dv);

  return retval;
}

FloatComplexNDArray
FloatComplexNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const FloatComplex *in (fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else
FloatComplexNDArray
FloatComplexNDArray::fourier (int dim) const
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
FloatComplexNDArray::ifourier (int dim) const
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
FloatComplexNDArray::fourier2d (void) const
{
  dim_vector dv = dims ();
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
FloatComplexNDArray::ifourier2d (void) const
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
FloatComplexNDArray::fourierNd (void) const
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
FloatComplexNDArray::ifourierNd (void) const
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
FloatComplexNDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (octave_idx_type i = 0; i < length (); i++)
    b.elem (i) = elem (i) == static_cast<float> (0.0);

  return b;
}

// FIXME -- this is not quite the right thing.

bool
FloatComplexNDArray::any_element_is_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      FloatComplex val = elem (i);
      if (xisnan (val))
	return true;
    }
  return false;
}

bool
FloatComplexNDArray::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      FloatComplex val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }
  return false;
}

// Return true if no elements have imaginary components.

bool
FloatComplexNDArray::all_elements_are_real (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float ip = std::imag (elem (i));

      if (ip != 0.0 || lo_ieee_signbit (ip))
	return false;
    }

  return true;
}

// Return nonzero if any element of CM has a non-integer real or
// imaginary part.  Also extract the largest and smallest (real or
// imaginary) values and return them in MAX_VAL and MIN_VAL. 

bool
FloatComplexNDArray::all_integers (float& max_val, float& min_val) const
{
  octave_idx_type nel = nelem ();

  if (nel > 0)
    {
      FloatComplex val = elem (0);

      float r_val = std::real (val);
      float i_val = std::imag (val);
      
      max_val = r_val;
      min_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (i_val < max_val)
	min_val = i_val;
    }
  else
    return false;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      FloatComplex val = elem (i);

      float r_val = std::real (val);
      float i_val = std::imag (val);

      if (r_val > max_val)
	max_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (r_val < min_val)
	min_val = r_val;

      if (i_val < min_val)
	min_val = i_val;

      if (D_NINT (r_val) != r_val || D_NINT (i_val) != i_val)
	return false;
    }

  return true;
}

bool
FloatComplexNDArray::too_large_for_float (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      FloatComplex val = elem (i);

      float r_val = std::real (val);
      float i_val = std::imag (val);

      if ((! (xisnan (r_val) || xisinf (r_val))
	   && fabs (r_val) > FLT_MAX)
	  || (! (xisnan (i_val) || xisinf (i_val))
	      && fabs (i_val) > FLT_MAX))
	return true;
    }

  return false;
}

boolNDArray
FloatComplexNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ALL_EVAL (elem (iter_idx) == FloatComplex (0, 0)), true);
}

boolNDArray
FloatComplexNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ANY_EVAL (elem (iter_idx) != FloatComplex (0, 0)
		     && ! (lo_ieee_isnan (std::real (elem (iter_idx)))
			   || lo_ieee_isnan (std::imag (elem (iter_idx))))),
		     false);
}

FloatComplexNDArray
FloatComplexNDArray::cumprod (int dim) const
{
  MX_ND_CUMULATIVE_OP (FloatComplexNDArray, FloatComplex, FloatComplex (1, 0), *);
}

FloatComplexNDArray
FloatComplexNDArray::cumsum (int dim) const
{
  MX_ND_CUMULATIVE_OP (FloatComplexNDArray, FloatComplex, FloatComplex (0, 0), +);
}

FloatComplexNDArray
FloatComplexNDArray::prod (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) *= elem (iter_idx), FloatComplex (1, 0), FloatComplexNDArray);
}

FloatComplexNDArray
FloatComplexNDArray::sumsq (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) += std::imag (elem (iter_idx))
     ? elem (iter_idx) * conj (elem (iter_idx))
     : std::pow (elem (iter_idx), 2), FloatComplex (0, 0), FloatComplexNDArray);
}

FloatComplexNDArray 
FloatComplexNDArray::sum (int dim) const
{
  MX_ND_REDUCTION (retval(result_idx) += elem (iter_idx), FloatComplex (0, 0), FloatComplexNDArray);
}

FloatComplexNDArray
FloatComplexNDArray::concat (const FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

FloatComplexNDArray
FloatComplexNDArray::concat (const FloatNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  FloatComplexNDArray tmp (rb);
  if (rb.numel () > 0)
    insert (tmp, ra_idx);
  return *this;
}

FloatComplexNDArray
concat (NDArray& ra, FloatComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  FloatComplexNDArray retval (ra);
  if (rb.numel () > 0)
    retval.insert (rb, ra_idx);
  return retval;
}

static const FloatComplex FloatComplex_NaN_result (octave_Float_NaN, octave_Float_NaN);

FloatComplexNDArray
FloatComplexNDArray::max (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

FloatComplexNDArray
FloatComplexNDArray::max (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();
  
  dr(dim) = 1;

  FloatComplexNDArray result (dr);
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

      FloatComplex tmp_max;

      float abs_max = octave_Float_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_max = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_max))
	    {
	      abs_max = std::abs(tmp_max);
	      break;
	    }
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  FloatComplex tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;

	  float abs_tmp = std::abs (tmp);

	  if (abs_tmp > abs_max)
	    {
	      idx_j = j;
	      tmp_max = tmp;
	      abs_max = abs_tmp;
	    }
	}

      if (xisnan (tmp_max))
	{
	  result.elem (i) = FloatComplex_NaN_result;
	  idx_arg.elem (i) = 0;
	}
      else
	{
	  result.elem (i) = tmp_max;
	  idx_arg.elem (i) = idx_j;
	}
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

FloatComplexNDArray
FloatComplexNDArray::min (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

FloatComplexNDArray
FloatComplexNDArray::min (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return FloatComplexNDArray ();
  
  dr(dim) = 1;

  FloatComplexNDArray result (dr);
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

      FloatComplex tmp_min;

      float abs_min = octave_Float_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_min = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_min))
	    {
	      abs_min = std::abs(tmp_min);
	      break;
	    }
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  FloatComplex tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;

	  float abs_tmp = std::abs (tmp);

	  if (abs_tmp < abs_min)
	    {
	      idx_j = j;
	      tmp_min = tmp;
	      abs_min = abs_tmp;
	    }
	}

      if (xisnan (tmp_min))
	{
	  result.elem (i) = FloatComplex_NaN_result;
	  idx_arg.elem (i) = 0;
	}
      else
	{
	  result.elem (i) = tmp_min;
	  idx_arg.elem (i) = idx_j;
	}
    }

  result.chop_trailing_singletons ();
  idx_arg.chop_trailing_singletons ();

  return result;
}

FloatNDArray
FloatComplexNDArray::abs (void) const
{
  return FloatNDArray (mx_inline_cabs_dup (data (), length ()),
                       dims ());
}

FloatComplexNDArray
conj (const FloatComplexNDArray& a)
{
  return FloatComplexNDArray (mx_inline_conj_dup (a.data (), a.length ()),
                              a.dims ());
}

FloatComplexNDArray&
FloatComplexNDArray::insert (const NDArray& a, octave_idx_type r, octave_idx_type c)
{
  dim_vector a_dv = a.dims ();
  
  int n = a_dv.length ();
  
  if (n == dimensions.length ())
    {
      Array<octave_idx_type> a_ra_idx (a_dv.length (), 0);
      
      a_ra_idx.elem (0) = r;
      a_ra_idx.elem (1) = c;
      
      for (int i = 0; i < n; i++)
	{
	  if (a_ra_idx (i) < 0 || (a_ra_idx (i) + a_dv (i)) > dimensions (i))
	    {
	      (*current_liboctave_error_handler)
		("Array<T>::insert: range error for insert");
	      return *this;
	    }
	}
      
      a_ra_idx.elem (0) = 0;
      a_ra_idx.elem (1) = 0;
      
      octave_idx_type n_elt = a.numel ();
      
      // IS make_unique () NECCESSARY HERE??

      for (octave_idx_type i = 0; i < n_elt; i++)
	{
	  Array<octave_idx_type> ra_idx = a_ra_idx;
	  
	  ra_idx.elem (0) = a_ra_idx (0) + r;
	  ra_idx.elem (1) = a_ra_idx (1) + c;
	  
	  elem (ra_idx) = a.elem (a_ra_idx);

	  increment_index (a_ra_idx, a_dv);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("Array<T>::insert: invalid indexing operation");

  return *this;
}

FloatComplexNDArray&
FloatComplexNDArray::insert (const FloatComplexNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<FloatComplex>::insert (a, r, c);
  return *this;
}

FloatComplexNDArray&
FloatComplexNDArray::insert (const FloatComplexNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<FloatComplex>::insert (a, ra_idx);
  return *this;
}

FloatComplexMatrix
FloatComplexNDArray::matrix_value (void) const
{
  FloatComplexMatrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = FloatComplexMatrix (Array2<FloatComplex> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = FloatComplexMatrix (Array2<FloatComplex> (*this, dimensions(0),
					       dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of FloatComplexNDArray to FloatComplexMatrix");
      break;
    }

  return retval;
}

void
FloatComplexNDArray::increment_index (Array<octave_idx_type>& ra_idx,
				 const dim_vector& dimensions,
				 int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type 
FloatComplexNDArray::compute_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

FloatComplexNDArray
FloatComplexNDArray::diag (octave_idx_type k) const
{
  return MArrayN<FloatComplex>::diag (k);
}

FloatNDArray
FloatComplexNDArray::map (dmapper fcn) const
{
  return MArrayN<FloatComplex>::map<float> (func_ptr (fcn));
}

FloatComplexNDArray
FloatComplexNDArray::map (cmapper fcn) const
{
  return MArrayN<FloatComplex>::map<FloatComplex> (func_ptr (fcn));
}

boolNDArray
FloatComplexNDArray::map (bmapper fcn) const
{
  return MArrayN<FloatComplex>::map<bool> (func_ptr (fcn));
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const FloatComplexNDArray& a)
{
  octave_idx_type nel = a.nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      os << " ";
      octave_write_complex (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, FloatComplexNDArray& a)
{
  octave_idx_type nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      FloatComplex tmp;
      for (octave_idx_type i = 0; i < nel; i++)
	  {
	    tmp = octave_read_complex (is);
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

FloatComplexNDArray
min (const FloatComplex& c, const FloatComplexNDArray& m)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (c, m (i));
    }

  return result;
}

FloatComplexNDArray
min (const FloatComplexNDArray& m, const FloatComplex& c)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (c, m (i));
    }

  return result;
}

FloatComplexNDArray
min (const FloatComplexNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return FloatComplexNDArray ();
    }

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (a (i), b (i));
    }

  return result;
}

FloatComplexNDArray
max (const FloatComplex& c, const FloatComplexNDArray& m)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (c, m (i));
    }

  return result;
}

FloatComplexNDArray
max (const FloatComplexNDArray& m, const FloatComplex& c)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (c, m (i));
    }

  return result;
}

FloatComplexNDArray
max (const FloatComplexNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return FloatComplexNDArray ();
    }

  EMPTY_RETURN_CHECK (FloatComplexNDArray);

  FloatComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (a (i), b (i));
    }

  return result;
}

NDS_CMP_OPS(FloatComplexNDArray, std::real, FloatComplex, std::real)
NDS_BOOL_OPS(FloatComplexNDArray, FloatComplex, static_cast<float> (0.0))

SND_CMP_OPS(FloatComplex, std::real, FloatComplexNDArray, std::real)
SND_BOOL_OPS(FloatComplex, FloatComplexNDArray, static_cast<float> (0.0))

NDND_CMP_OPS(FloatComplexNDArray, std::real, FloatComplexNDArray, std::real)
NDND_BOOL_OPS(FloatComplexNDArray, FloatComplexNDArray, static_cast<float> (0.0))

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
