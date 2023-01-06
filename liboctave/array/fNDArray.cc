////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <istream>
#include <limits>
#include <ostream>

#include "Array-util.h"
#include "f77-fcn.h"
#include "fNDArray.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "mx-base.h"
#include "mx-op-defs.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"

#include "bsxfun-defs.cc"

FloatNDArray::FloatNDArray (const charNDArray& a)
  : MArray<float> (a.dims ())
{
  octave_idx_type n = a.numel ();
  for (octave_idx_type i = 0; i < n; i++)
    xelem (i) = static_cast<unsigned char> (a(i));
}

#if defined (HAVE_FFTW)

FloatComplexNDArray
FloatNDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.ndims () || dim < 0)
    return FloatComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv(dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv(dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  const float *in (data ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave::fftw::fft (in + k * stride * n, out + k * stride * n,
                       n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.ndims () || dim < 0)
    return FloatComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv(dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv(dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  FloatComplexNDArray retval (*this);
  FloatComplex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave::fftw::ifft (out + k * stride * n, out + k * stride * n,
                        n, howmany, stride, dist);

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourier2d (void) const
{
  dim_vector dv = dims ();
  if (dv.ndims () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2 (dv(0), dv(1));
  const float *in = data ();
  FloatComplexNDArray retval (dv);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel () / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave::fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourier2d (void) const
{
  dim_vector dv = dims ();
  if (dv.ndims () < 2)
    return FloatComplexNDArray ();

  dim_vector dv2 (dv(0), dv(1));
  FloatComplexNDArray retval (*this);
  FloatComplex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel () / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave::fftw::ifftNd (out + i*dist, out + i*dist, 2, dv2);

  return retval;
}

FloatComplexNDArray
FloatNDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.ndims ();

  const float *in (data ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave::fftw::fftNd (in, out, rank, dv);

  return retval;
}

FloatComplexNDArray
FloatNDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.ndims ();

  FloatComplexNDArray tmp (*this);
  FloatComplex *in (tmp.fortran_vec ());
  FloatComplexNDArray retval (dv);
  FloatComplex *out (retval.fortran_vec ());

  octave::fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else

FloatComplexNDArray
FloatNDArray::fourier (int dim) const
{
  octave_unused_parameter (dim);

  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexNDArray ();
}

FloatComplexNDArray
FloatNDArray::ifourier (int dim) const
{
  octave_unused_parameter (dim);

  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");
  return FloatComplexNDArray ();
}

FloatComplexNDArray
FloatNDArray::fourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexNDArray ();
}

FloatComplexNDArray
FloatNDArray::ifourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexNDArray ();
}

FloatComplexNDArray
FloatNDArray::fourierNd (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexNDArray ();
}

FloatComplexNDArray
FloatNDArray::ifourierNd (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return FloatComplexNDArray ();
}

#endif

// unary operations

boolNDArray
FloatNDArray::operator ! (void) const
{
  if (any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();

  return do_mx_unary_op<bool, float> (*this, mx_inline_not);
}

bool
FloatNDArray::any_element_is_negative (bool neg_zero) const
{
  return (neg_zero ? test_all (octave::math::negative_sign)
          : do_mx_check<float> (*this, mx_inline_any_negative));
}

bool
FloatNDArray::any_element_is_positive (bool neg_zero) const
{
  return (neg_zero ? test_all (octave::math::positive_sign)
          : do_mx_check<float> (*this, mx_inline_any_positive));
}

bool
FloatNDArray::any_element_is_nan (void) const
{
  return do_mx_check<float> (*this, mx_inline_any_nan);
}

bool
FloatNDArray::any_element_is_inf_or_nan (void) const
{
  return ! do_mx_check<float> (*this, mx_inline_all_finite);
}

bool
FloatNDArray::any_element_not_one_or_zero (void) const
{
  return ! test_all (octave::is_one_or_zero);
}

bool
FloatNDArray::all_elements_are_zero (void) const
{
  return test_all (octave::is_zero);
}

bool
FloatNDArray::all_elements_are_int_or_inf_or_nan (void) const
{
  return test_all (octave::is_int_or_inf_or_nan);
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
FloatNDArray::all_integers (float& max_val, float& min_val) const
{
  octave_idx_type nel = numel ();

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

      if (! octave::math::isinteger (val))
        return false;
    }

  return true;
}

bool
FloatNDArray::all_integers (void) const
{
  return test_all (octave::math::isinteger);
}

bool
FloatNDArray::too_large_for_float (void) const
{
  return false;
}

// FIXME: this is not quite the right thing.

boolNDArray
FloatNDArray::all (int dim) const
{
  return do_mx_red_op<bool, float> (*this, dim, mx_inline_all);
}

boolNDArray
FloatNDArray::any (int dim) const
{
  return do_mx_red_op<bool, float> (*this, dim, mx_inline_any);
}

FloatNDArray
FloatNDArray::cumprod (int dim) const
{
  return do_mx_cum_op<float, float> (*this, dim, mx_inline_cumprod);
}

FloatNDArray
FloatNDArray::cumsum (int dim) const
{
  return do_mx_cum_op<float, float> (*this, dim, mx_inline_cumsum);
}

FloatNDArray
FloatNDArray::prod (int dim) const
{
  return do_mx_red_op<float, float> (*this, dim, mx_inline_prod);
}

NDArray
FloatNDArray::dprod (int dim) const
{
  return do_mx_red_op<double, float> (*this, dim, mx_inline_dprod);
}

FloatNDArray
FloatNDArray::sum (int dim) const
{
  return do_mx_red_op<float, float> (*this, dim, mx_inline_sum);
}

NDArray
FloatNDArray::dsum (int dim) const
{
  return do_mx_red_op<double, float> (*this, dim, mx_inline_dsum);
}

FloatNDArray
FloatNDArray::sumsq (int dim) const
{
  return do_mx_red_op<float, float> (*this, dim, mx_inline_sumsq);
}

FloatNDArray
FloatNDArray::max (int dim) const
{
  return do_mx_minmax_op<float> (*this, dim, mx_inline_max);
}

FloatNDArray
FloatNDArray::max (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<float> (*this, idx_arg, dim, mx_inline_max);
}

FloatNDArray
FloatNDArray::min (int dim) const
{
  return do_mx_minmax_op<float> (*this, dim, mx_inline_min);
}

FloatNDArray
FloatNDArray::min (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<float> (*this, idx_arg, dim, mx_inline_min);
}

FloatNDArray
FloatNDArray::cummax (int dim) const
{
  return do_mx_cumminmax_op<float> (*this, dim, mx_inline_cummax);
}

FloatNDArray
FloatNDArray::cummax (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_cumminmax_op<float> (*this, idx_arg, dim, mx_inline_cummax);
}

FloatNDArray
FloatNDArray::cummin (int dim) const
{
  return do_mx_cumminmax_op<float> (*this, dim, mx_inline_cummin);
}

FloatNDArray
FloatNDArray::cummin (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_cumminmax_op<float> (*this, idx_arg, dim, mx_inline_cummin);
}

FloatNDArray
FloatNDArray::diff (octave_idx_type order, int dim) const
{
  return do_mx_diff_op<float> (*this, dim, order, mx_inline_diff);
}

FloatNDArray
FloatNDArray::concat (const FloatNDArray& rb,
                      const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

FloatComplexNDArray
FloatNDArray::concat (const FloatComplexNDArray& rb,
                      const Array<octave_idx_type>& ra_idx)
{
  FloatComplexNDArray retval (*this);
  if (rb.numel () > 0)
    retval.insert (rb, ra_idx);
  return retval;
}

charNDArray
FloatNDArray::concat (const charNDArray& rb,
                      const Array<octave_idx_type>& ra_idx)
{
  charNDArray retval (dims ());
  octave_idx_type nel = numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      float d = elem (i);

      if (octave::math::isnan (d))
        (*current_liboctave_error_handler)
          ("invalid conversion from NaN to character");

      octave_idx_type ival = octave::math::nint_big (d);

      if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
        // FIXME: is there something better to do?  Should we warn the user?
        ival = 0;

      retval.elem (i) = static_cast<char> (ival);
    }

  if (rb.isempty ())
    return retval;

  retval.insert (rb, ra_idx);
  return retval;
}

FloatNDArray
real (const FloatComplexNDArray& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_real);
}

FloatNDArray
imag (const FloatComplexNDArray& a)
{
  return do_mx_unary_op<float, FloatComplex> (a, mx_inline_imag);
}

FloatNDArray&
FloatNDArray::insert (const FloatNDArray& a,
                      octave_idx_type r, octave_idx_type c)
{
  Array<float>::insert (a, r, c);
  return *this;
}

FloatNDArray&
FloatNDArray::insert (const FloatNDArray& a,
                      const Array<octave_idx_type>& ra_idx)
{
  Array<float>::insert (a, ra_idx);
  return *this;
}

FloatNDArray
FloatNDArray::abs (void) const
{
  return do_mx_unary_map<float, float, std::abs> (*this);
}

boolNDArray
FloatNDArray::isnan (void) const
{
  return do_mx_unary_map<bool, float, octave::math::isnan> (*this);
}

boolNDArray
FloatNDArray::isinf (void) const
{
  return do_mx_unary_map<bool, float, octave::math::isinf> (*this);
}

boolNDArray
FloatNDArray::isfinite (void) const
{
  return do_mx_unary_map<bool, float, octave::math::isfinite> (*this);
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
  return MArray<float>::diag (k);
}

FloatNDArray
FloatNDArray::diag (octave_idx_type m, octave_idx_type n) const
{
  return MArray<float>::diag (m, n);
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const FloatNDArray& a)
{
  octave_idx_type nel = a.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      os << ' ';
      octave::write_value<float> (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, FloatNDArray& a)
{
  octave_idx_type nel = a.numel ();

  if (nel > 0)
    {
      float tmp;
      for (octave_idx_type i = 0; i < nel; i++)
        {
          tmp = octave::read_value<float> (is);
          if (is)
            a.elem (i) = tmp;
          else
            return is;
        }
    }

  return is;
}

MINMAX_FCNS (FloatNDArray, float)

NDS_CMP_OPS (FloatNDArray, float)
NDS_BOOL_OPS (FloatNDArray, float)

SND_CMP_OPS (float, FloatNDArray)
SND_BOOL_OPS (float, FloatNDArray)

NDND_CMP_OPS (FloatNDArray, FloatNDArray)
NDND_BOOL_OPS (FloatNDArray, FloatNDArray)

BSXFUN_STDOP_DEFS_MXLOOP (FloatNDArray)
BSXFUN_STDREL_DEFS_MXLOOP (FloatNDArray)

BSXFUN_OP_DEF_MXLOOP (pow, FloatNDArray, mx_inline_pow)
BSXFUN_OP2_DEF_MXLOOP (pow, FloatComplexNDArray, FloatComplexNDArray,
                       FloatNDArray, mx_inline_pow)
BSXFUN_OP2_DEF_MXLOOP (pow, FloatComplexNDArray, FloatNDArray,
                       FloatComplexNDArray, mx_inline_pow)
