/*

Copyright (C) 2001, 2004, 2005, 2007, 2008 John W. Eaton

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
#include <fftw3.h>

#include "oct-cmplx.h"
#include "dim-vector.h"

class
OCTAVE_API
octave_fftw_planner
{
public:

  octave_fftw_planner (void);

  fftw_plan create_plan (int dir, const int rank, const dim_vector dims, 
			 octave_idx_type howmany, octave_idx_type stride, octave_idx_type dist, 
			 const Complex *in, Complex *out);

  fftw_plan create_plan (const int rank, const dim_vector dims, 
			 octave_idx_type howmany, octave_idx_type stride, octave_idx_type dist, 
			 const double *in, Complex *out);

  enum FftwMethod {
    UNKNOWN = -1,
    ESTIMATE,
    MEASURE,
    PATIENT,
    EXHAUSTIVE,
    HYBRID
  };

  FftwMethod method (void);

  FftwMethod method (FftwMethod _meth);

private:

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
public:

  octave_float_fftw_planner (void);

  fftwf_plan create_plan (int dir, const int rank, const dim_vector dims, 
			 octave_idx_type howmany, octave_idx_type stride, octave_idx_type dist, 
			 const FloatComplex *in, FloatComplex *out);

  fftwf_plan create_plan (const int rank, const dim_vector dims, 
			 octave_idx_type howmany, octave_idx_type stride, octave_idx_type dist, 
			 const float *in, FloatComplex *out);

  enum FftwMethod {
    UNKNOWN = -1,
    ESTIMATE,
    MEASURE,
    PATIENT,
    EXHAUSTIVE,
    HYBRID
  };

  FftwMethod method (void);

  FftwMethod method (FftwMethod _meth);

private:

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

// FIXME -- maybe octave_fftw_planner should be a singleton object?
extern OCTAVE_API octave_fftw_planner fftw_planner;
extern OCTAVE_API octave_float_fftw_planner float_fftw_planner;

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
  octave_fftw ();
  octave_fftw (const octave_fftw&);
  octave_fftw& operator = (const octave_fftw&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

