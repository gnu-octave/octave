// N-D Array  manipulations.
/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include <vector>

#include "Array-util.h"
#include "dNDArray.h"
#include "mx-base.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

#if defined (HAVE_FFTW3)
#include "oct-fftw.h"

ComplexNDArray
NDArray::fourier (int dim) const
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

  const double *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::fft (in + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
NDArray::ifourier (int dim) const
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

  ComplexNDArray retval (*this);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave_fftw::ifft (out + k * stride * n, out + k * stride * n, 
		      n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
NDArray::fourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return ComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  const double *in = fortran_vec ();
  ComplexNDArray retval (dv);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
NDArray::ifourier2d (void) const
{
  dim_vector dv = dims();
  if (dv.length () < 2)
    return ComplexNDArray ();

  dim_vector dv2(dv(0), dv(1));
  ComplexNDArray retval (*this);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel() / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave_fftw::ifftNd (out + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
NDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  const double *in (fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave_fftw::fftNd (in, out, rank, dv);

  return retval;
}

ComplexNDArray
NDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.length ();

  ComplexNDArray tmp (*this);
  Complex *in (tmp.fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else

extern "C"
{
  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const octave_idx_type&, Complex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const octave_idx_type&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const octave_idx_type&, Complex*, Complex*);
}

ComplexNDArray
NDArray::fourier (int dim) const
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

ComplexNDArray
NDArray::ifourier (int dim) const
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
	      static_cast<double> (npts);
	}
    }

  return retval;
}

ComplexNDArray
NDArray::fourier2d (void) const
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

ComplexNDArray
NDArray::ifourier2d (void) const
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
		  static_cast<double> (npts);
	    }
	}

      stride *= dv2(i);
    }

  return retval;
}

ComplexNDArray
NDArray::fourierNd (void) const
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

ComplexNDArray
NDArray::ifourierNd (void) const
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
NDArray::operator ! (void) const
{
  boolNDArray b (dims ());

  for (octave_idx_type i = 0; i < length (); i++)
    b.elem (i) = ! elem (i);

  return b;
}

bool
NDArray::any_element_is_negative (bool neg_zero) const
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
NDArray::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
NDArray::any_element_not_one_or_zero (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (val != 0 && val != 1)
	return true;
    }

  return false;
}

bool
NDArray::all_elements_are_zero (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    if (elem (i) != 0)
      return false;

  return true;
}

bool
NDArray::all_elements_are_int_or_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);
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
NDArray::all_integers (double& max_val, double& min_val) const
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
      double val = elem (i);

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
NDArray::too_large_for_float (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (! (xisnan (val) || xisinf (val))
	  && fabs (val) > FLT_MAX)
	return true;
    }

  return false;
}

// FIXME -- this is not quite the right thing.

boolNDArray
NDArray::all (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION (MX_ND_ALL_EVAL (MX_ND_ALL_EXPR), true);
}

boolNDArray
NDArray::any (int dim) const
{
  MX_ND_ANY_ALL_REDUCTION
    (MX_ND_ANY_EVAL (elem (iter_idx) != 0
		     && ! lo_ieee_isnan (elem (iter_idx))), false);
}

NDArray
NDArray::cumprod (int dim) const
{
  MX_ND_CUMULATIVE_OP (NDArray, double, 1, *);
}

NDArray
NDArray::cumsum (int dim) const
{
  MX_ND_CUMULATIVE_OP (NDArray, double, 0, +);
}

NDArray
NDArray::prod (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (*= elem (iter_idx), 1);
}

NDArray
NDArray::sumsq (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (+= std::pow (elem (iter_idx), 2), 0);
}

NDArray 
NDArray::sum (int dim) const
{
  MX_ND_REAL_OP_REDUCTION (+= elem (iter_idx), 0);
}

NDArray
NDArray::max (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return max (dummy_idx, dim);
}

NDArray
NDArray::max (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return NDArray ();
  
  dr(dim) = 1;

  NDArray result (dr);
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

      double tmp_max = octave_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_max = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_max))
	    break;
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  double tmp = elem (j * x_stride + x_offset);

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

  return result;
}

NDArray
NDArray::min (int dim) const
{
  ArrayN<octave_idx_type> dummy_idx;
  return min (dummy_idx, dim);
}

NDArray
NDArray::min (ArrayN<octave_idx_type>& idx_arg, int dim) const
{
  dim_vector dv = dims ();
  dim_vector dr = dims ();

  if (dv.numel () == 0 || dim > dv.length () || dim < 0)
    return NDArray ();
  
  dr(dim) = 1;

  NDArray result (dr);
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

      double tmp_min = octave_NaN;

      for (idx_j = 0; idx_j < x_len; idx_j++)
	{
	  tmp_min = elem (idx_j * x_stride + x_offset);
	  
	  if (! xisnan (tmp_min))
	    break;
	}

      for (octave_idx_type j = idx_j+1; j < x_len; j++)
	{
	  double tmp = elem (j * x_stride + x_offset);

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

  return result;
}

NDArray
NDArray::concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

ComplexNDArray
NDArray::concat (const ComplexNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  ComplexNDArray retval (*this);
  if (rb.numel () > 0)
    retval.insert (rb, ra_idx);
  return retval;
}

charNDArray
NDArray::concat (const charNDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  charNDArray retval (dims ());
  octave_idx_type nel = numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double d = elem (i);

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

NDArray
real (const ComplexNDArray& a)
{
  octave_idx_type a_len = a.length ();
  NDArray retval;
  if (a_len > 0)
    retval = NDArray (mx_inline_real_dup (a.data (), a_len), a.dims ());
  return retval;
}

NDArray
imag (const ComplexNDArray& a)
{
  octave_idx_type a_len = a.length ();
  NDArray retval;
  if (a_len > 0)
    retval = NDArray (mx_inline_imag_dup (a.data (), a_len), a.dims ());
  return retval;
}

NDArray&
NDArray::insert (const NDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<double>::insert (a, r, c);
  return *this;
}

NDArray&
NDArray::insert (const NDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<double>::insert (a, ra_idx);
  return *this;
}

NDArray
NDArray::abs (void) const
{
  NDArray retval (dims ());

  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = fabs (elem (i));

  return retval;
}

Matrix
NDArray::matrix_value (void) const
{
  Matrix retval;

  int nd = ndims ();

  switch (nd)
    {
    case 1:
      retval = Matrix (Array2<double> (*this, dimensions(0), 1));
      break;

    case 2:
      retval = Matrix (Array2<double> (*this, dimensions(0), dimensions(1)));
      break;

    default:
      (*current_liboctave_error_handler)
	("invalid conversion of NDArray to Matrix");
      break;
    }

  return retval;
}

void
NDArray::increment_index (Array<octave_idx_type>& ra_idx,
			  const dim_vector& dimensions,
			  int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type
NDArray::compute_index (Array<octave_idx_type>& ra_idx,
			const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const NDArray& a)
{
  octave_idx_type nel = a.nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      os << " ";
      octave_write_double (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, NDArray& a)
{
  octave_idx_type nel = a.nelem ();

  if (nel < 1 )
    is.clear (std::ios::badbit);
  else
    {
      double tmp;
      for (octave_idx_type i = 0; i < nel; i++)
	  {
	    tmp = octave_read_double (is);
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

NDArray
min (double d, const NDArray& m)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (d, m (i));
    }

  return result;
}

NDArray
min (const NDArray& m, double d)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (d, m (i));
    }

  return result;
}

NDArray
min (const NDArray& a, const NDArray& b)
{
  dim_vector dv = a.dims ();
  octave_idx_type nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return NDArray ();
    }

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmin (a (i), b (i));
    }

  return result;
}

NDArray
max (double d, const NDArray& m)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (d, m (i));
    }

  return result;
}

NDArray
max (const NDArray& m, double d)
{
  dim_vector dv = m.dims ();
  octave_idx_type nel = dv.numel ();

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (d, m (i));
    }

  return result;
}

NDArray
max (const NDArray& a, const NDArray& b)
{
  dim_vector dv = a.dims ();
  octave_idx_type nel = dv.numel ();

  if (dv != b.dims ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return NDArray ();
    }

  EMPTY_RETURN_CHECK (NDArray);

  NDArray result (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      OCTAVE_QUIT;
      result (i) = xmax (a (i), b (i));
    }

  return result;
}

NDS_CMP_OPS(NDArray, , double, )
NDS_BOOL_OPS(NDArray, double, 0.0)

SND_CMP_OPS(double, , NDArray, )
SND_BOOL_OPS(double, NDArray, 0.0)

NDND_CMP_OPS(NDArray, , NDArray, )
NDND_BOOL_OPS(NDArray, NDArray, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
