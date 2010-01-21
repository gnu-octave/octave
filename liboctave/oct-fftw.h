/*

Copyright (C) 2001, 2004, 2005, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_oct_fftw_h)
#define octave_oct_fftw_h 1

#include <cstddef>

#if defined (HAVE_FFTW3_H)
#include <fftw3.h>
#endif

#include "oct-cmplx.h"
#include "dim-vector.h"

#if defined (HAVE_FFTW)

class
OCTAVE_API
octave_fftw_planner
{
protected:

  octave_fftw_planner (void);

public:

  ~octave_fftw_planner (void) { }

  enum FftwMethod
  {
    UNKNOWN = -1,
    ESTIMATE,
    MEASURE,
    PATIENT,
    EXHAUSTIVE,
    HYBRID
  };

  static bool instance_ok (void);

  static fftw_plan
  create_plan (int dir, const int rank, const dim_vector dims, 
	       octave_idx_type howmany, octave_idx_type stride,
	       octave_idx_type dist, const Complex *in,
	       Complex *out)
  {
    static fftw_plan dummy;

    return instance_ok ()
      ? instance->do_create_plan (dir, rank, dims, howmany, stride,
				  dist, in, out)
      : dummy;
  }

  static fftw_plan
  create_plan (const int rank, const dim_vector dims, 
	       octave_idx_type howmany, octave_idx_type stride,
	       octave_idx_type dist, const double *in, Complex *out)
  {
    static fftw_plan dummy;

    return instance_ok ()
      ? instance->do_create_plan (rank, dims, howmany, stride, dist, in, out)
      : dummy;
  }

  static FftwMethod method (void)
  {
    static FftwMethod dummy;

    return instance_ok () ? instance->do_method () : dummy;
  }

  static FftwMethod method (FftwMethod _meth)
  {
    static FftwMethod dummy;

    return instance_ok () ? instance->do_method (_meth) : dummy;
  }

private:

  static octave_fftw_planner *instance;

  fftw_plan
  do_create_plan (int dir, const int rank, const dim_vector dims, 
		  octave_idx_type howmany, octave_idx_type stride,
		  octave_idx_type dist, const Complex *in,
		  Complex *out);

  fftw_plan
  do_create_plan (const int rank, const dim_vector dims, 
		  octave_idx_type howmany, octave_idx_type stride,
		  octave_idx_type dist, const double *in, Complex *out);

  FftwMethod do_method (void);

  FftwMethod do_method (FftwMethod _meth);

  FftwMethod meth;

  // FIXME -- perhaps this should be split into two classes?

  // Plan for fft and ifft of complex values
  fftw_plan plan[2];

  // dist
  octave_idx_type d[2];

  // stride
  octave_idx_type s[2];

  // rank
  int r[2];

  // howmany
  octave_idx_type h[2];

  // dims
  dim_vector n[2];

  bool simd_align[2];
  bool inplace[2];

  // Plan for fft of real values
  fftw_plan rplan;

  // dist
  octave_idx_type rd;

  // stride
  octave_idx_type rs;

  // rank
  int rr;

  // howmany
  octave_idx_type rh;

  // dims
  dim_vector rn;

  bool rsimd_align;
};

class
OCTAVE_API
octave_float_fftw_planner
{
protected:

  octave_float_fftw_planner (void);

public:

  ~octave_float_fftw_planner (void) { }

  enum FftwMethod
  {
    UNKNOWN = -1,
    ESTIMATE,
    MEASURE,
    PATIENT,
    EXHAUSTIVE,
    HYBRID
  };

  static bool instance_ok (void);
  
  static fftwf_plan
  create_plan (int dir, const int rank, const dim_vector dims, 
	       octave_idx_type howmany, octave_idx_type stride,
	       octave_idx_type dist, const FloatComplex *in,
	       FloatComplex *out)
  {
    static fftwf_plan dummy;

    return instance_ok ()
      ? instance->do_create_plan (dir, rank, dims, howmany, stride,
				  dist, in, out)
      : dummy;
  }

  static fftwf_plan
  create_plan (const int rank, const dim_vector dims, 
	       octave_idx_type howmany, octave_idx_type stride,
	       octave_idx_type dist, const float *in, FloatComplex *out)
  {
    static fftwf_plan dummy;

    return instance_ok ()
      ? instance->do_create_plan (rank, dims, howmany, stride, dist, in, out)
      : dummy;
  }

  static FftwMethod method (void)
  {
    static FftwMethod dummy;

    return instance_ok () ? instance->method () : dummy;
  }

  static FftwMethod method (FftwMethod _meth)
  {
    static FftwMethod dummy;

    return instance_ok () ? instance->method (_meth) : dummy;
  }

private:

  static octave_float_fftw_planner *instance;

  fftwf_plan
  do_create_plan (int dir, const int rank, const dim_vector dims, 
		  octave_idx_type howmany, octave_idx_type stride,
		  octave_idx_type dist, const FloatComplex *in,
		  FloatComplex *out);

  fftwf_plan
  do_create_plan (const int rank, const dim_vector dims, 
		  octave_idx_type howmany, octave_idx_type stride,
		  octave_idx_type dist, const float *in, FloatComplex *out);

  FftwMethod do_method (void);

  FftwMethod do_method (FftwMethod _meth);

  FftwMethod meth;

  // FIXME -- perhaps this should be split into two classes?

  // Plan for fft and ifft of complex values
  fftwf_plan plan[2];

  // dist
  octave_idx_type d[2];

  // stride
  octave_idx_type s[2];

  // rank
  int r[2];

  // howmany
  octave_idx_type h[2];

  // dims
  dim_vector n[2];

  bool simd_align[2];
  bool inplace[2];

  // Plan for fft of real values
  fftwf_plan rplan;

  // dist
  octave_idx_type rd;

  // stride
  octave_idx_type rs;

  // rank
  int rr;

  // howmany
  octave_idx_type rh;

  // dims
  dim_vector rn;

  bool rsimd_align;
};

class
OCTAVE_API
octave_fftw
{
public:

  static int fft (const double *in, Complex *out, size_t npts, 
		  size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);
  static int fft (const Complex *in, Complex *out, size_t npts, 
		  size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);
  static int ifft (const Complex *in, Complex *out, size_t npts,
		   size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);

  static int fftNd (const double*, Complex*, const int, const dim_vector &);
  static int fftNd (const Complex*, Complex*, const int, 
		    const dim_vector &);
  static int ifftNd (const Complex*, Complex*, const int, 
		     const dim_vector &);

  static int fft (const float *in, FloatComplex *out, size_t npts, 
		  size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);
  static int fft (const FloatComplex *in, FloatComplex *out, size_t npts, 
		  size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);
  static int ifft (const FloatComplex *in, FloatComplex *out, size_t npts,
		   size_t nsamples = 1, octave_idx_type stride = 1, octave_idx_type dist = -1);

  static int fftNd (const float*, FloatComplex*, const int, const dim_vector &);
  static int fftNd (const FloatComplex*, FloatComplex*, const int, 
		    const dim_vector &);
  static int ifftNd (const FloatComplex*, FloatComplex*, const int, 
		     const dim_vector &);

private:
  octave_fftw (void);
  octave_fftw (const octave_fftw&);
  octave_fftw& operator = (const octave_fftw&);
};

#endif

#endif
