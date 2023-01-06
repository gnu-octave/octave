////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

#if ! defined (octave_oct_fftw_h)
#define octave_oct_fftw_h 1

#include "octave-config.h"

#include <cstddef>

#include <string>

#include "dim-vector.h"
#include "oct-cmplx.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTAVE_API
fftw_planner
{
protected:

  fftw_planner (void);

public:

  // No copying!

  fftw_planner (const fftw_planner&) = delete;

  fftw_planner& operator = (const fftw_planner&) = delete;

  ~fftw_planner (void);

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

  static void *
  create_plan (int dir, const int rank, const dim_vector& dims,
               octave_idx_type howmany, octave_idx_type stride,
               octave_idx_type dist, const Complex *in,
               Complex *out)
  {
    return instance_ok ()
           ? s_instance->do_create_plan (dir, rank, dims, howmany, stride,
                                         dist, in, out)
           : nullptr;
  }

  static void *
  create_plan (const int rank, const dim_vector& dims,
               octave_idx_type howmany, octave_idx_type stride,
               octave_idx_type dist, const double *in, Complex *out)
  {
    return instance_ok ()
           ? s_instance->do_create_plan (rank, dims, howmany, stride, dist,
                                         in, out)
           : nullptr;
  }

  static FftwMethod method (void)
  {
    static FftwMethod dummy;

    return instance_ok () ? s_instance->do_method () : dummy;
  }

  static FftwMethod method (FftwMethod meth)
  {
    static FftwMethod dummy;

    return instance_ok () ? s_instance->do_method (meth) : dummy;
  }

  static void threads (int nt);

  static int threads (void)
  {
    return instance_ok () ? s_instance->m_nthreads : 0;
  }

private:

  static fftw_planner *s_instance;

  static void cleanup_instance (void)
  { delete s_instance; s_instance = nullptr; }

  void *
  do_create_plan (int dir, const int rank, const dim_vector& dims,
                  octave_idx_type howmany, octave_idx_type stride,
                  octave_idx_type dist, const Complex *in,
                  Complex *out);

  void *
  do_create_plan (const int rank, const dim_vector& dims,
                  octave_idx_type howmany, octave_idx_type stride,
                  octave_idx_type dist, const double *in, Complex *out);

  FftwMethod do_method (void);

  FftwMethod do_method (FftwMethod meth);

  FftwMethod m_meth;

  // FIXME: perhaps this should be split into two classes?

  // Plan for fft and ifft of complex values
  void *m_plan[2];

  // dist
  octave_idx_type m_d[2];

  // stride
  octave_idx_type m_s[2];

  // rank
  int m_r[2];

  // howmany
  octave_idx_type m_h[2];

  // dims
  dim_vector m_n[2];

  bool m_simd_align[2];
  bool m_inplace[2];

  // Plan for fft of real values
  void *m_rplan;

  // dist
  octave_idx_type m_rd;

  // stride
  octave_idx_type m_rs;

  // rank
  int m_rr;

  // howmany
  octave_idx_type m_rh;

  // dims
  dim_vector m_rn;

  bool m_rsimd_align;

  // number of threads.  Always 1 unless compiled with multi-threading
  // support.
  int m_nthreads;
};

class
OCTAVE_API
float_fftw_planner
{
protected:

  float_fftw_planner (void);

public:

  // No copying!

  float_fftw_planner (const float_fftw_planner&) = delete;

  float_fftw_planner&
  operator = (const float_fftw_planner&) = delete;

  ~float_fftw_planner (void);

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

  static void *
  create_plan (int dir, const int rank, const dim_vector& dims,
               octave_idx_type howmany, octave_idx_type stride,
               octave_idx_type dist, const FloatComplex *in,
               FloatComplex *out)
  {
    return instance_ok ()
           ? s_instance->do_create_plan (dir, rank, dims, howmany, stride,
                                         dist, in, out)
           : nullptr;
  }

  static void *
  create_plan (const int rank, const dim_vector& dims,
               octave_idx_type howmany, octave_idx_type stride,
               octave_idx_type dist, const float *in, FloatComplex *out)
  {
    return instance_ok ()
           ? s_instance->do_create_plan (rank, dims, howmany, stride, dist,
                                         in, out)
           : nullptr;
  }

  static FftwMethod method (void)
  {
    static FftwMethod dummy;

    return instance_ok () ? s_instance->do_method () : dummy;
  }

  static FftwMethod method (FftwMethod meth)
  {
    static FftwMethod dummy;

    return instance_ok () ? s_instance->do_method (meth) : dummy;
  }

  static void threads (int nt);

  static int threads (void)
  {
    return instance_ok () ? s_instance->m_nthreads : 0;
  }

private:

  static float_fftw_planner *s_instance;

  static void cleanup_instance (void)
  { delete s_instance; s_instance = nullptr; }

  void *
  do_create_plan (int dir, const int rank, const dim_vector& dims,
                  octave_idx_type howmany, octave_idx_type stride,
                  octave_idx_type dist, const FloatComplex *in,
                  FloatComplex *out);

  void *
  do_create_plan (const int rank, const dim_vector& dims,
                  octave_idx_type howmany, octave_idx_type stride,
                  octave_idx_type dist, const float *in, FloatComplex *out);

  FftwMethod do_method (void);

  FftwMethod do_method (FftwMethod meth);

  FftwMethod m_meth;

  // FIXME: perhaps this should be split into two classes?

  // Plan for fft and ifft of complex values
  void *m_plan[2];

  // dist
  octave_idx_type m_d[2];

  // stride
  octave_idx_type m_s[2];

  // rank
  int m_r[2];

  // howmany
  octave_idx_type m_h[2];

  // dims
  dim_vector m_n[2];

  bool m_simd_align[2];
  bool m_inplace[2];

  // Plan for fft of real values
  void *m_rplan;

  // dist
  octave_idx_type m_rd;

  // stride
  octave_idx_type m_rs;

  // rank
  int m_rr;

  // howmany
  octave_idx_type m_rh;

  // dims
  dim_vector m_rn;

  bool m_rsimd_align;

  // number of threads.  Always 1 unless compiled with multi-threading
  // support.
  int m_nthreads;
};

class
OCTAVE_API
fftw
{
public:

  fftw (void) = delete;

  // No copying.

  fftw (const fftw&) = delete;

  fftw& operator = (const fftw&) = delete;

  static int fft (const double *in, Complex *out, std::size_t npts,
                  std::size_t nsamples = 1, octave_idx_type stride = 1,
                  octave_idx_type dist = -1);
  static int fft (const Complex *in, Complex *out, std::size_t npts,
                  std::size_t nsamples = 1, octave_idx_type stride = 1,
                  octave_idx_type dist = -1);
  static int ifft (const Complex *in, Complex *out, std::size_t npts,
                   std::size_t nsamples = 1, octave_idx_type stride = 1,
                   octave_idx_type dist = -1);

  static int fftNd (const double *, Complex *, const int, const dim_vector&);
  static int fftNd (const Complex *, Complex *, const int,
                    const dim_vector&);
  static int ifftNd (const Complex *, Complex *, const int,
                     const dim_vector&);

  static int fft (const float *in, FloatComplex *out, std::size_t npts,
                  std::size_t nsamples = 1, octave_idx_type stride = 1,
                  octave_idx_type dist = -1);
  static int fft (const FloatComplex *in, FloatComplex *out, std::size_t npts,
                  std::size_t nsamples = 1, octave_idx_type stride = 1,
                  octave_idx_type dist = -1);
  static int ifft (const FloatComplex *in, FloatComplex *out, std::size_t npts,
                   std::size_t nsamples = 1, octave_idx_type stride = 1,
                   octave_idx_type dist = -1);

  static int fftNd (const float *, FloatComplex *, const int,
                    const dim_vector&);
  static int fftNd (const FloatComplex *, FloatComplex *, const int,
                    const dim_vector&);
  static int ifftNd (const FloatComplex *, FloatComplex *, const int,
                     const dim_vector&);
};

extern OCTAVE_API std::string fftw_version (void);
extern OCTAVE_API std::string fftwf_version (void);

OCTAVE_END_NAMESPACE(octave)

#endif
