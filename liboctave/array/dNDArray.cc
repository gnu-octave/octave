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
#include "dNDArray.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "mx-base.h"
#include "mx-op-defs.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"

#include "bsxfun-defs.cc"

NDArray::NDArray (const Array<octave_idx_type>& a, bool zero_based,
                  bool negative_to_nan)
{
  const octave_idx_type *pa = a.data ();
  resize (a.dims ());
  double *ptmp = fortran_vec ();
  if (negative_to_nan)
    {
      double nan_val = lo_ieee_nan_value ();

      if (zero_based)
        for (octave_idx_type i = 0; i < a.numel (); i++)
          {
            double val = static_cast<double>
                         (pa[i] + static_cast<octave_idx_type> (1));
            if (val <= 0)
              ptmp[i] = nan_val;
            else
              ptmp[i] = val;
          }
      else
        for (octave_idx_type i = 0; i < a.numel (); i++)
          {
            double val = static_cast<double> (pa[i]);
            if (val <= 0)
              ptmp[i] = nan_val;
            else
              ptmp[i] = val;
          }
    }
  else
    {
      if (zero_based)
        for (octave_idx_type i = 0; i < a.numel (); i++)
          ptmp[i] = static_cast<double>
                    (pa[i] + static_cast<octave_idx_type> (1));
      else
        for (octave_idx_type i = 0; i < a.numel (); i++)
          ptmp[i] = static_cast<double> (pa[i]);
    }
}

NDArray::NDArray (const charNDArray& a)
  : MArray<double> (a.dims ())
{
  octave_idx_type n = a.numel ();
  for (octave_idx_type i = 0; i < n; i++)
    xelem (i) = static_cast<unsigned char> (a(i));
}

#if defined (HAVE_FFTW)

ComplexNDArray
NDArray::fourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.ndims () || dim < 0)
    return ComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv(dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv(dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  const double *in (data ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave::fftw::fft (in + k * stride * n, out + k * stride * n,
                       n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
NDArray::ifourier (int dim) const
{
  dim_vector dv = dims ();

  if (dim > dv.ndims () || dim < 0)
    return ComplexNDArray ();

  octave_idx_type stride = 1;
  octave_idx_type n = dv(dim);

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  octave_idx_type howmany = numel () / dv(dim);
  howmany = (stride == 1 ? howmany : (howmany > stride ? stride : howmany));
  octave_idx_type nloop = (stride == 1 ? 1 : numel () / dv(dim) / stride);
  octave_idx_type dist = (stride == 1 ? n : 1);

  ComplexNDArray retval (*this);
  Complex *out (retval.fortran_vec ());

  // Need to be careful here about the distance between fft's
  for (octave_idx_type k = 0; k < nloop; k++)
    octave::fftw::ifft (out + k * stride * n, out + k * stride * n,
                        n, howmany, stride, dist);

  return retval;
}

ComplexNDArray
NDArray::fourier2d (void) const
{
  dim_vector dv = dims ();
  if (dv.ndims () < 2)
    return ComplexNDArray ();

  dim_vector dv2 (dv(0), dv(1));
  const double *in = data ();
  ComplexNDArray retval (dv);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel () / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave::fftw::fftNd (in + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
NDArray::ifourier2d (void) const
{
  dim_vector dv = dims ();
  if (dv.ndims () < 2)
    return ComplexNDArray ();

  dim_vector dv2 (dv(0), dv(1));
  ComplexNDArray retval (*this);
  Complex *out = retval.fortran_vec ();
  octave_idx_type howmany = numel () / dv(0) / dv(1);
  octave_idx_type dist = dv(0) * dv(1);

  for (octave_idx_type i=0; i < howmany; i++)
    octave::fftw::ifftNd (out + i*dist, out + i*dist, 2, dv2);

  return retval;
}

ComplexNDArray
NDArray::fourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.ndims ();

  const double *in (data ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave::fftw::fftNd (in, out, rank, dv);

  return retval;
}

ComplexNDArray
NDArray::ifourierNd (void) const
{
  dim_vector dv = dims ();
  int rank = dv.ndims ();

  ComplexNDArray tmp (*this);
  Complex *in (tmp.fortran_vec ());
  ComplexNDArray retval (dv);
  Complex *out (retval.fortran_vec ());

  octave::fftw::ifftNd (in, out, rank, dv);

  return retval;
}

#else

ComplexNDArray
NDArray::fourier (int dim) const
{
  octave_unused_parameter (dim);

  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

ComplexNDArray
NDArray::ifourier (int dim) const
{
  octave_unused_parameter (dim);

  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

ComplexNDArray
NDArray::fourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

ComplexNDArray
NDArray::ifourier2d (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

ComplexNDArray
NDArray::fourierNd (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

ComplexNDArray
NDArray::ifourierNd (void) const
{
  (*current_liboctave_error_handler)
    ("support for FFTW was unavailable or disabled when liboctave was built");

  return ComplexNDArray ();
}

#endif

// unary operations

boolNDArray
NDArray::operator ! (void) const
{
  if (any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();

  return do_mx_unary_op<bool, double> (*this, mx_inline_not);
}

bool
NDArray::any_element_is_negative (bool neg_zero) const
{
  return (neg_zero ? test_all (octave::math::negative_sign)
          : do_mx_check<double> (*this, mx_inline_any_negative));
}

bool
NDArray::any_element_is_positive (bool neg_zero) const
{
  return (neg_zero ? test_all (octave::math::positive_sign)
          : do_mx_check<double> (*this, mx_inline_any_positive));
}

bool
NDArray::any_element_is_nan (void) const
{
  return do_mx_check<double> (*this, mx_inline_any_nan);
}

bool
NDArray::any_element_is_inf_or_nan (void) const
{
  return ! do_mx_check<double> (*this, mx_inline_all_finite);
}

bool
NDArray::any_element_not_one_or_zero (void) const
{
  return ! test_all (octave::is_one_or_zero);
}

bool
NDArray::all_elements_are_zero (void) const
{
  return test_all (octave::is_zero);
}

bool
NDArray::all_elements_are_int_or_inf_or_nan (void) const
{
  return test_all (octave::is_int_or_inf_or_nan);
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
NDArray::all_integers (double& max_val, double& min_val) const
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
      double val = elem (i);

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
NDArray::all_integers (void) const
{
  return test_all (octave::math::isinteger);
}

bool
NDArray::too_large_for_float (void) const
{
  return test_any (octave::too_large_for_float);
}

// FIXME: this is not quite the right thing.

boolNDArray
NDArray::all (int dim) const
{
  return do_mx_red_op<bool, double> (*this, dim, mx_inline_all);
}

boolNDArray
NDArray::any (int dim) const
{
  return do_mx_red_op<bool, double> (*this, dim, mx_inline_any);
}

NDArray
NDArray::cumprod (int dim) const
{
  return do_mx_cum_op<double, double> (*this, dim, mx_inline_cumprod);
}

NDArray
NDArray::cumsum (int dim) const
{
  return do_mx_cum_op<double, double> (*this, dim, mx_inline_cumsum);
}

NDArray
NDArray::prod (int dim) const
{
  return do_mx_red_op<double, double> (*this, dim, mx_inline_prod);
}

NDArray
NDArray::sum (int dim) const
{
  return do_mx_red_op<double, double> (*this, dim, mx_inline_sum);
}

NDArray
NDArray::xsum (int dim) const
{
  return do_mx_red_op<double, double> (*this, dim, mx_inline_xsum);
}

NDArray
NDArray::sumsq (int dim) const
{
  return do_mx_red_op<double, double> (*this, dim, mx_inline_sumsq);
}

NDArray
NDArray::max (int dim) const
{
  return do_mx_minmax_op<double> (*this, dim, mx_inline_max);
}

NDArray
NDArray::max (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<double> (*this, idx_arg, dim, mx_inline_max);
}

NDArray
NDArray::min (int dim) const
{
  return do_mx_minmax_op<double> (*this, dim, mx_inline_min);
}

NDArray
NDArray::min (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<double> (*this, idx_arg, dim, mx_inline_min);
}

NDArray
NDArray::cummax (int dim) const
{
  return do_mx_cumminmax_op<double> (*this, dim, mx_inline_cummax);
}

NDArray
NDArray::cummax (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_cumminmax_op<double> (*this, idx_arg, dim, mx_inline_cummax);
}

NDArray
NDArray::cummin (int dim) const
{
  return do_mx_cumminmax_op<double> (*this, dim, mx_inline_cummin);
}

NDArray
NDArray::cummin (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_cumminmax_op<double> (*this, idx_arg, dim, mx_inline_cummin);
}

NDArray
NDArray::diff (octave_idx_type order, int dim) const
{
  return do_mx_diff_op<double> (*this, dim, order, mx_inline_diff);
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

      if (octave::math::isnan (d))
        (*current_liboctave_error_handler)
          ("invalid conversion from NaN to character");

      octave_idx_type ival = octave::math::nint_big (d);

      if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
        // FIXME: is there something better to do? Should we warn the user?
        ival = 0;

      retval.elem (i) = static_cast<char> (ival);
    }

  if (rb.isempty ())
    return retval;

  retval.insert (rb, ra_idx);
  return retval;
}

NDArray
real (const ComplexNDArray& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_real);
}

NDArray
imag (const ComplexNDArray& a)
{
  return do_mx_unary_op<double, Complex> (a, mx_inline_imag);
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
  return do_mx_unary_map<double, double, std::abs> (*this);
}

boolNDArray
NDArray::isnan (void) const
{
  return do_mx_unary_map<bool, double, octave::math::isnan> (*this);
}

boolNDArray
NDArray::isinf (void) const
{
  return do_mx_unary_map<bool, double, octave::math::isinf> (*this);
}

boolNDArray
NDArray::isfinite (void) const
{
  return do_mx_unary_map<bool, double, octave::math::isfinite> (*this);
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

NDArray
NDArray::diag (octave_idx_type k) const
{
  return MArray<double>::diag (k);
}

NDArray
NDArray::diag (octave_idx_type m, octave_idx_type n) const
{
  return MArray<double>::diag (m, n);
}

// This contains no information on the array structure !!!
std::ostream&
operator << (std::ostream& os, const NDArray& a)
{
  octave_idx_type nel = a.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      os << ' ';
      octave::write_value<double> (os, a.elem (i));
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, NDArray& a)
{
  octave_idx_type nel = a.numel ();

  if (nel > 0)
    {
      double tmp;
      for (octave_idx_type i = 0; i < nel; i++)
        {
          tmp = octave::read_value<double> (is);
          if (is)
            a.elem (i) = tmp;
          else
            return is;
        }
    }

  return is;
}

MINMAX_FCNS (NDArray, double)

NDS_CMP_OPS (NDArray, double)
NDS_BOOL_OPS (NDArray, double)

SND_CMP_OPS (double, NDArray)
SND_BOOL_OPS (double, NDArray)

NDND_CMP_OPS (NDArray, NDArray)
NDND_BOOL_OPS (NDArray, NDArray)

BSXFUN_STDOP_DEFS_MXLOOP (NDArray)
BSXFUN_STDREL_DEFS_MXLOOP (NDArray)

BSXFUN_OP_DEF_MXLOOP (pow, NDArray, mx_inline_pow)
BSXFUN_OP2_DEF_MXLOOP (pow, ComplexNDArray, ComplexNDArray,
                       NDArray, mx_inline_pow)
BSXFUN_OP2_DEF_MXLOOP (pow, ComplexNDArray, NDArray,
                       ComplexNDArray, mx_inline_pow)
