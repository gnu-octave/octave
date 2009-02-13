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
#include "CNDArray.h"
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
  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (zffti, ZFFTI) (const octave_idx_type&, Complex*);

  F77_RET_T
  F77_FUNC (zfftf, ZFFTF) (const octave_idx_type&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (zfftb, ZFFTB) (const octave_idx_type&, Complex*, Complex*);
}
#endif

#if defined (HAVE_FFTW3)
ComplexNDArray
ComplexNDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return ComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv (dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv (dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  const Complex *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::fft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return ComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv (dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv (dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  const Complex *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::ifft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
ComplexNDArray::fourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return ComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const Complex *in = fortran_vec ();
  ComplexNDArray retval (dv);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return ComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const Complex *in = fortran_vec ();
  ComplexNDArray retval (dv);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::ifftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
ComplexNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const Complex *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave_fftw::fftNd (in, out, rank, dv);

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const Complex *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else
ComplexNDArray
ComplexNDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return ComplexNDArray ();

  ComplexNDArray retval (dv);
  octave_idx_type npts = dv(dim);
  octave_idx_type nn = 4*npts+15;
  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, tmp, npts);

  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / npts;
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
  octave_idx_type dist = (stride == 1 ? npts : 1);

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type k = 0; k < nloop; k++)
    {
      for (octave_idx_type j = 0; j < howmany; j++)
	{
	  OCTAVE_QUIT;

	  for (octave_idx_type i = 0; i < npts; i++)
	    tmp[i] = elem((i + k*npts)*stride + j*dist);

	  F77_FUNC (zfftf, ZFFTF) (npts, tmp, pwsave);

	  for (octave_idx_type i = 0; i < npts; i++)
	    retval ((i + k*npts)*stride + j*dist) = tmp[i];
	}
    }

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.length () || dim < 0)
    return ComplexNDArray ();

  ComplexNDArray retval (dv);
  octave_idx_type npts = dv(dim);
  octave_idx_type nn = 4*npts+15;
  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (Complex, tmp, npts);

  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / npts;
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
  octave_idx_type dist = (stride == 1 ? npts : 1);

  F77_FUNC (zffti, ZFFTI) (npts, pwsave);

  for (octave_idx_type k = 0; k < nloop; k++)
    {
      for (octave_idx_type j = 0; j < howmany; j++)
	{
	  OCTAVE_QUIT;

	  for (octave_idx_type i = 0; i < npts; i++)
	    tmp[i] = elem((i + k*npts)*stride + j*dist);

	  F77_FUNC (zfftb, ZFFTB) (npts, tmp, pwsave);

	  for (octave_idx_type i = 0; i < npts; i++)
	    retval ((i + k*npts)*stride + j*dist) = tmp[i] /
	      static_cast<double> (npts);
	}
    }

  return retval;
}

ComplexNDArray
ComplexNDArray::fourier2d (void) const
{
  dim_vector dv = dims ();
  dim_vector dv2 (dv(0), dv(1));
  int rank = 2;
  ComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv2(i);
      octave_idx_type nn = 4*npts+15;
      Array<Complex> wsave (nn);
      Complex *pwsave = wsave.fortran_vec ();
      Array<Complex> row (npts);
      Complex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (zffti, ZFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (zfftf, ZFFTF) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l];
	    }
	}

      stride *= dv2(i);
    }

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  dim_vector dv2 (dv(0), dv(1));
  int rank = 2;
  ComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv2(i);
      octave_idx_type nn = 4*npts+15;
      Array<Complex> wsave (nn);
      Complex *pwsave = wsave.fortran_vec ();
      Array<Complex> row (npts);
      Complex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (zffti, ZFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (zfftb, ZFFTB) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l] /
		  static_cast<double> (npts);
	    }
	}

      stride *= dv2(i);
    }

  return retval;
}

ComplexNDArray
ComplexNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();
  ComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv(i);
      octave_idx_type nn = 4*npts+15;
      Array<Complex> wsave (nn);
      Complex *pwsave = wsave.fortran_vec ();
      Array<Complex> row (npts);
      Complex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (zffti, ZFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (zfftf, ZFFTF) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l];
	    }
	}

      stride *= dv(i);
    }

  return retval;
}

ComplexNDArray
ComplexNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();
  ComplexNDArray retval (*this);
  octave_idx_type stride = 1;

  for (int i = 0; i < rank; i++)
    {
      octave_idx_type npts = dv(i);
      octave_idx_type nn = 4*npts+15;
      Array<Complex> wsave (nn);
      Complex *pwsave = wsave.fortran_vec ();
      Array<Complex> row (npts);
      Complex *prow = row.fortran_vec ();

      octave_idx_type howmany = numel () / npts;
      howmany = (stride == 1 ? howmany : 
		 (howmany > stride ? stride : howmany));
      octave_idx_type nloop = (stride == 1 ? 1 : numel () / npts / stride);
      octave_idx_type dist = (stride == 1 ? npts : 1);

      F77_FUNC (zffti, ZFFTI) (npts, pwsave);

      for (octave_idx_type k = 0; k < nloop; k++)
	{
	  for (octave_idx_type j = 0; j < howmany; j++)
	    {
	      OCTAVE_QUIT;

	      for (octave_idx_type l = 0; l < npts; l++)
		prow[l] = retval ((l + k*npts)*stride + j*dist);

	      F77_FUNC (zfftb, ZFFTB) (npts, prow, pwsave);

	      for (octave_idx_type l = 0; l < npts; l++)
		retval ((l + k*npts)*stride + j*dist) = prow[l] /
		  static_cast<double> (npts);
	    }
	}

      stride *= dv(i);
    }

  return retval;
}

#endif

// unary operations

boolNDArray
ComplexNDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (octave_idx_type i = 0; i < length (); i++)
    b.elem (i) = elem (i) == 0.0;

  return b;
}

// FIXME -- this is not quite the right thing.

bool
ComplexNDArray::any_element_is_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      Complex val = elem (i);
      if (xisnan (val))
	return true;
    }
  return false;
}

bool
ComplexNDArray::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      Complex val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }
  return false;
}

// Return true if no elements have imaginary components.

bool
ComplexNDArray::all_elements_are_real (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double ip = std::imag (elem (i));

      if (ip != 0.0 || lo_ieee_signbit (ip))
	return false;
    }

  return true;
}

// Return nonzero if any element of CM has a non-integer real or
// imaginary part.  Also extract the largest and smallest (real or
// imaginary) values and return them in MAX_VAL and MIN_VAL. 

bool
ComplexNDArray::all_integers (double& max_val, double& min_val) const
{
  octave_idx_type nel = nelem ();

  if (nel > 0)
    {
      Complex val = elem (0);

      double r_val = std::real (val);
      double i_val = std::imag (val);
      
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
      Complex val = elem (i);

      double r_val = std::real (val);
      double i_val = std::imag (val);

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
ComplexNDArray::too_large_for_float (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      Complex val = elem (i);

      double r_val = std::real (val);
      double i_val = std::imag (val);

      if ((! (xisnan (r_val) || xisinf (r_val))
	   && fabs (r_val) > FLT_MAX)
	  || (! (xisnan (i_val) || xisinf (i_val))
	      && fabs (i_val) > FLT_MAX))
	return true;
    }

  return false;
}

boolNDArray
ComplexNDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ALL_EVAL (elem (iter_idx) == Complex (0, 0)), true);
}

boolNDArray
ComplexNDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ANY_EVAL (elem (iter_idx) != Complex (0, 0)
		     && ! (lo_ieee_isnan (std::real (elem (iter_idx)))
			   || lo_ieee_isnan (std::imag (elem (iter_idx))))),
		     false);
}

ComplexNDArray
ComplexNDArray::cumprod (int dim) const
{
  return do_mx_cum_op<ComplexNDArray> (*this, dim, mx_inline_cumprod);
}

ComplexNDArray
ComplexNDArray::cumsum (int dim) const
{
  return do_mx_cum_op<ComplexNDArray> (*this, dim, mx_inline_cumsum);
}

ComplexNDArray
ComplexNDArray::prod (int dim) const
{
  return do_mx_red_op<ComplexNDArray> (*this, dim, mx_inline_prod);
}

ComplexNDArray
ComplexNDArray::sum (int dim) const
{
  return do_mx_red_op<ComplexNDArray> (*this, dim, mx_inline_sum);
}

ComplexNDArray
ComplexNDArray::sumsq (int dim) const
{
  return do_mx_red_op<NDArray> (*this, dim, mx_inline_sumsq);
}

ComplexNDArray
ComplexNDArray::concat (const ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

ComplexNDArray
ComplexNDArray::concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  ComplexNDArray tmp (rb);
  if (rb.numel () > 0)
    insert (tmp, ra_idx);
  return *this;
}

ComplexNDArray
concat (NDArray& ra, ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  ComplexNDArray retval (ra);
  if (rb.numel () > 0)
    retval.insert (rb, ra_idx);
  return retval;
}

static const Complex Complex_NaN_result (octave_NaN, octave_NaN);

ComplexNDArray
ComplexNDArray::max (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

ComplexNDArray
ComplexNDArray::max (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return ComplexNDArray ();
  
  dr(dim) = 1;

  ComplexNDArray result (dr);
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

      Complex tmp_max;

      double abs_max = octave_NaN;

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
	  Complex tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;

	  double abs_tmp = std::abs (tmp);

	  if (abs_tmp > abs_max)
	    {
	      idx_j = j;
	      tmp_max = tmp;
	      abs_max = abs_tmp;
	    }
	}

      if (xisnan (tmp_max))
	{
	  result.elem (i) = Complex_NaN_result;
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

ComplexNDArray
ComplexNDArray::min (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

ComplexNDArray
ComplexNDArray::min (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return ComplexNDArray ();
  
  dr(dim) = 1;

  ComplexNDArray result (dr);
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

      Complex tmp_min;

      double abs_min = octave_NaN;

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
	  Complex tmp = elem (j * x_stride + x_offset);

	  if (xisnan (tmp))
	    continue;

	  double abs_tmp = std::abs (tmp);

	  if (abs_tmp < abs_min)
	    {
	      idx_j = j;
	      tmp_min = tmp;
	      abs_min = abs_tmp;
	    }
	}

      if (xisnan (tmp_min))
	{
	  result.elem (i) = Complex_NaN_result;
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

NDArray
ComplexNDArray::abs (void) const
{
  return NDArray (mx_inline_cabs_dup (data (), length ()),
                  dims ());
}

ComplexNDArray
conj (const ComplexNDArray& a)
{
  return ComplexNDArray (mx_inline_conj_dup (a.data (), a.length ()),
                         a.dims ());
}

ComplexNDArray&
ComplexNDArray::insert (const NDArray& a, octave_idx_type r, octave_idx_type c)
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

ComplexNDArray&
ComplexNDArray::insert (const ComplexNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<Complex>::insert (a, r, c);
  return *this;
}

ComplexNDArray&
ComplexNDArray::insert (const ComplexNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<Complex>::insert (a, ra_idx);
  return *this;
}

ComplexMatrix
ComplexNDArray::matrix_value (void) const
{
  ComplexMatrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = ComplexMatrix (Array2<Complex> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = ComplexMatrix (Array2<Complex> (*this, dimensions(0),
					       dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of ComplexNDArray to ComplexMatrix");
      break;
    }

  return retval;
}

void
ComplexNDArray::increment_index (Array<octave_idx_type>& ra_idx,
				 const dim_vector& dimensions,
				 int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type 
ComplexNDArray::compute_index (Array<octave_idx_type>& ra_idx,
			       const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

ComplexNDArray
ComplexNDArray::diag (octave_idx_type k) const
{
  return MArrayN<Complex>::diag (k);
}

NDArray
ComplexNDArray::map (dmapper fcn) const
{
  return MArrayN<Complex>::map<double> (func_ptr (fcn));
}

ComplexNDArray
ComplexNDArray::map (cmapper fcn) const
{
  return MArrayN<Complex>::map<Complex> (func_ptr (fcn));
}

boolNDArray
ComplexNDArray::map (bmapper fcn) const
{
  return MArrayN<Complex>::map<bool> (func_ptr (fcn));
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const ComplexNDArray& a)
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
operator >> (std::istream& is, ComplexNDArray& a)
{
  octave_idx_type nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      Complex tmp;
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

ComplexNDArray
min (const Complex& c, const ComplexNDArray& m)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (c, m (i));
    }

  return result;
}

ComplexNDArray
min (const ComplexNDArray& m, const Complex& c)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (c, m (i));
    }

  return result;
}

ComplexNDArray
min (const ComplexNDArray& a, const ComplexNDArray& b)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return ComplexNDArray ();
    }

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (a (i), b (i));
    }

  return result;
}

ComplexNDArray
max (const Complex& c, const ComplexNDArray& m)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (c, m (i));
    }

  return result;
}

ComplexNDArray
max (const ComplexNDArray& m, const Complex& c)
{
  dim_vector dv = m.dims ();
  int nel = dv.numel ();

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (c, m (i));
    }

  return result;
}

ComplexNDArray
max (const ComplexNDArray& a, const ComplexNDArray& b)
{
  dim_vector dv = a.dims ();
  int nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return ComplexNDArray ();
    }

  EMPTY_RETURN_CHECK (ComplexNDArray);

  ComplexNDArray result (dv);

  for (int i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (a (i), b (i));
    }

  return result;
}

NDS_CMP_OPS(ComplexNDArray, std::real, Complex, std::real)
NDS_BOOL_OPS(ComplexNDArray, Complex, 0.0)

SND_CMP_OPS(Complex, std::real, ComplexNDArray, std::real)
SND_BOOL_OPS(Complex, ComplexNDArray, 0.0)

NDND_CMP_OPS(ComplexNDArray, std::real, ComplexNDArray, std::real)
NDND_BOOL_OPS(ComplexNDArray, ComplexNDArray, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
