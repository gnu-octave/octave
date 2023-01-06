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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_FFTW3_H)
#  include <fftw3.h>
#endif

#include "lo-error.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "singleton-cleanup.h"

#if defined (HAVE_FFTW3_THREADS) || defined (HAVE_FFTW3F_THREADS)
#  include "nproc-wrapper.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_FFTW)

fftw_planner *fftw_planner::s_instance = nullptr;

// Helper class to create and cache FFTW plans for both 1D and
// 2D.  This implementation defaults to using FFTW_ESTIMATE to create
// the plans, which in theory is suboptimal, but provides quite
// reasonable performance in practice.

// Also note that if FFTW_ESTIMATE is not used then the planner in FFTW3
// will destroy the input and output arrays.  We must, therefore, create a
// temporary input array with the same size and 16-byte alignment as
// the original array when using a different planner strategy.
// Note that we also use any wisdom that is available, either in a
// FFTW3 system wide file or as supplied by the user.

// FIXME: if we can ensure 16 byte alignment in Array<T>
// (<T> *data) the FFTW3 can use SIMD instructions for further
// acceleration.

// Note that it is profitable to store the FFTW3 plans, for small FFTs.

fftw_planner::fftw_planner (void)
  : m_meth (ESTIMATE), m_rplan (nullptr), m_rd (0), m_rs (0), m_rr (0),
    m_rh (0), m_rn (), m_rsimd_align (false), m_nthreads (1)
{
  m_plan[0] = m_plan[1] = nullptr;
  m_d[0] = m_d[1] = m_s[0] = m_s[1] = m_r[0] = m_r[1] = m_h[0] = m_h[1] = 0;
  m_simd_align[0] = m_simd_align[1] = false;
  m_inplace[0] = m_inplace[1] = false;
  m_n[0] = m_n[1] = dim_vector ();

#if defined (HAVE_FFTW3_THREADS)
  int init_ret = fftw_init_threads ();
  if (! init_ret)
    (*current_liboctave_error_handler) ("Error initializing FFTW threads");

  // Check number of processors available to the current process
  m_nthreads =
    octave_num_processors_wrapper (OCTAVE_NPROC_CURRENT_OVERRIDABLE);

  // Limit number of threads to 3 by default
  // See: https://octave.discourse.group/t/3121
  // This can be later changed with fftw ("threads", nthreads).
  if (m_nthreads > 3)
    m_nthreads = 3;

  fftw_plan_with_nthreads (m_nthreads);
#endif

  // If we have a system wide wisdom file, import it.
  fftw_import_system_wisdom ();
}

fftw_planner::~fftw_planner (void)
{
  fftw_plan *plan_p;

  plan_p = reinterpret_cast<fftw_plan *> (&m_rplan);
  if (*plan_p)
    fftw_destroy_plan (*plan_p);

  plan_p = reinterpret_cast<fftw_plan *> (&m_plan[0]);
  if (*plan_p)
    fftw_destroy_plan (*plan_p);

  plan_p = reinterpret_cast<fftw_plan *> (&m_plan[1]);
  if (*plan_p)
    fftw_destroy_plan (*plan_p);
}

bool
fftw_planner::instance_ok (void)
{
  bool retval = true;

  if (! s_instance)
    {
      s_instance = new fftw_planner ();
      singleton_cleanup_list::add (cleanup_instance);
    }

  return retval;
}

void
fftw_planner::threads (int nt)
{
#if defined (HAVE_FFTW3_THREADS)
  if (instance_ok () && nt != threads ())
    {
      s_instance->m_nthreads = nt;
      fftw_plan_with_nthreads (nt);
      // Clear the current plans.
      s_instance->m_rplan = nullptr;
      s_instance->m_plan[0] = s_instance->m_plan[1] = nullptr;
    }
#else
  octave_unused_parameter (nt);

  (*current_liboctave_warning_handler)
    ("unable to change number of threads without FFTW thread support");
#endif
}

#define CHECK_SIMD_ALIGNMENT(x)                         \
  (((reinterpret_cast<std::ptrdiff_t> (x)) & 0xF) == 0)

void *
fftw_planner::do_create_plan (int dir, const int rank,
                              const dim_vector& dims,
                              octave_idx_type howmany,
                              octave_idx_type stride,
                              octave_idx_type dist,
                              const Complex *in, Complex *out)
{
  int which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftw_plan *cur_plan_p = reinterpret_cast<fftw_plan *> (&m_plan[which]);
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);
  bool ioinplace = (in == out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (m_plan[which] == nullptr || m_d[which] != dist || m_s[which] != stride
      || m_r[which] != rank || m_h[which] != howmany
      || ioinplace != m_inplace[which]
      || ((ioalign != m_simd_align[which]) ? ! ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != m_n[which](i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      m_d[which] = dist;
      m_s[which] = stride;
      m_r[which] = rank;
      m_h[which] = howmany;
      m_simd_align[which] = ioalign;
      m_inplace[which] = ioinplace;
      m_n[which] = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (m_meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftw_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (Complex, itmp, nn * howmany + 32);
          itmp = reinterpret_cast<Complex *>
                 (((reinterpret_cast<std::ptrdiff_t> (itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<std::ptrdiff_t> (in)) & 0xF));

          *cur_plan_p
            = fftw_plan_many_dft (rank, tmp, howmany,
                                  reinterpret_cast<fftw_complex *> (itmp),
                                  nullptr, stride, dist,
                                  reinterpret_cast<fftw_complex *> (out),
                                  nullptr, stride, dist, dir, plan_flags);
        }
      else
        {
          *cur_plan_p
            = fftw_plan_many_dft (rank, tmp, howmany,
                                  reinterpret_cast<fftw_complex *> (const_cast<Complex *> (in)),
                                  nullptr, stride, dist,
                                  reinterpret_cast<fftw_complex *> (out),
                                  nullptr, stride, dist, dir, plan_flags);
        }

      if (*cur_plan_p == nullptr)
        (*current_liboctave_error_handler) ("Error creating FFTW plan");
    }

  return *cur_plan_p;
}

void *
fftw_planner::do_create_plan (const int rank, const dim_vector& dims,
                              octave_idx_type howmany,
                              octave_idx_type stride,
                              octave_idx_type dist,
                              const double *in, Complex *out)
{
  fftw_plan *cur_plan_p = reinterpret_cast<fftw_plan *> (&m_rplan);
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (m_rplan == nullptr || m_rd != dist || m_rs != stride || m_rr != rank
      || m_rh != howmany || ((ioalign != m_rsimd_align) ? ! ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != m_rn(i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      m_rd = dist;
      m_rs = stride;
      m_rr = rank;
      m_rh = howmany;
      m_rsimd_align = ioalign;
      m_rn = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (m_meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftw_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (double, itmp, nn + 32);
          itmp = reinterpret_cast<double *>
                 (((reinterpret_cast<std::ptrdiff_t> (itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<std::ptrdiff_t> (in)) & 0xF));

          *cur_plan_p
            = fftw_plan_many_dft_r2c (rank, tmp, howmany, itmp,
                                      nullptr, stride, dist,
                                      reinterpret_cast<fftw_complex *> (out),
                                      nullptr, stride, dist, plan_flags);
        }
      else
        {
          *cur_plan_p
            = fftw_plan_many_dft_r2c (rank, tmp, howmany,
                                      (const_cast<double *> (in)),
                                      nullptr, stride, dist,
                                      reinterpret_cast<fftw_complex *> (out),
                                      nullptr, stride, dist, plan_flags);
        }

      if (*cur_plan_p == nullptr)
        (*current_liboctave_error_handler) ("Error creating FFTW plan");
    }

  return *cur_plan_p;
}

fftw_planner::FftwMethod
fftw_planner::do_method (void)
{
  return m_meth;
}

fftw_planner::FftwMethod
fftw_planner::do_method (FftwMethod _meth)
{
  FftwMethod ret = m_meth;
  if (_meth == ESTIMATE || _meth == MEASURE
      || _meth == PATIENT || _meth == EXHAUSTIVE
      || _meth == HYBRID)
    {
      if (m_meth != _meth)
        {
          m_meth = _meth;
          if (m_rplan)
            fftw_destroy_plan (reinterpret_cast<fftw_plan> (m_rplan));
          if (m_plan[0])
            fftw_destroy_plan (reinterpret_cast<fftw_plan> (m_plan[0]));
          if (m_plan[1])
            fftw_destroy_plan (reinterpret_cast<fftw_plan> (m_plan[1]));
          m_rplan = m_plan[0] = m_plan[1] = nullptr;
        }
    }
  else
    ret = UNKNOWN;
  return ret;
}

float_fftw_planner *float_fftw_planner::s_instance = nullptr;

float_fftw_planner::float_fftw_planner (void)
  : m_meth (ESTIMATE), m_rplan (nullptr), m_rd (0), m_rs (0), m_rr (0),
    m_rh (0), m_rn (), m_rsimd_align (false), m_nthreads (1)
{
  m_plan[0] = m_plan[1] = nullptr;
  m_d[0] = m_d[1] = m_s[0] = m_s[1] = m_r[0] = m_r[1] = m_h[0] = m_h[1] = 0;
  m_simd_align[0] = m_simd_align[1] = false;
  m_inplace[0] = m_inplace[1] = false;
  m_n[0] = m_n[1] = dim_vector ();

#if defined (HAVE_FFTW3F_THREADS)
  int init_ret = fftwf_init_threads ();
  if (! init_ret)
    (*current_liboctave_error_handler) ("Error initializing FFTW3F threads");

  // Use number of processors available to the current process
  // This can be later changed with fftw ("threads", nthreads).
  m_nthreads =
    octave_num_processors_wrapper (OCTAVE_NPROC_CURRENT_OVERRIDABLE);

  fftwf_plan_with_nthreads (m_nthreads);
#endif

  // If we have a system wide wisdom file, import it.
  fftwf_import_system_wisdom ();
}

float_fftw_planner::~float_fftw_planner (void)
{
  fftwf_plan *plan_p;

  plan_p = reinterpret_cast<fftwf_plan *> (&m_rplan);
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);

  plan_p = reinterpret_cast<fftwf_plan *> (&m_plan[0]);
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);

  plan_p = reinterpret_cast<fftwf_plan *> (&m_plan[1]);
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);
}

bool
float_fftw_planner::instance_ok (void)
{
  bool retval = true;

  if (! s_instance)
    {
      s_instance = new float_fftw_planner ();
      singleton_cleanup_list::add (cleanup_instance);
    }

  return retval;
}

void
float_fftw_planner::threads (int nt)
{
#if defined (HAVE_FFTW3F_THREADS)
  if (instance_ok () && nt != threads ())
    {
      s_instance->m_nthreads = nt;
      fftwf_plan_with_nthreads (nt);
      // Clear the current plans.
      s_instance->m_rplan = nullptr;
      s_instance->m_plan[0] = s_instance->m_plan[1] = nullptr;
    }
#else
  octave_unused_parameter (nt);

  (*current_liboctave_warning_handler)
    ("unable to change number of threads without FFTW thread support");
#endif
}

void *
float_fftw_planner::do_create_plan (int dir, const int rank,
                                    const dim_vector& dims,
                                    octave_idx_type howmany,
                                    octave_idx_type stride,
                                    octave_idx_type dist,
                                    const FloatComplex *in,
                                    FloatComplex *out)
{
  int which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftwf_plan *cur_plan_p = reinterpret_cast<fftwf_plan *> (&m_plan[which]);
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);
  bool ioinplace = (in == out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (m_plan[which] == nullptr || m_d[which] != dist || m_s[which] != stride
      || m_r[which] != rank || m_h[which] != howmany
      || ioinplace != m_inplace[which]
      || ((ioalign != m_simd_align[which]) ? ! ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != m_n[which](i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      m_d[which] = dist;
      m_s[which] = stride;
      m_r[which] = rank;
      m_h[which] = howmany;
      m_simd_align[which] = ioalign;
      m_inplace[which] = ioinplace;
      m_n[which] = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (m_meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftwf_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (FloatComplex, itmp, nn * howmany + 32);
          itmp = reinterpret_cast<FloatComplex *>
                 (((reinterpret_cast<std::ptrdiff_t> (itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<std::ptrdiff_t> (in)) & 0xF));

          *cur_plan_p
            = fftwf_plan_many_dft (rank, tmp, howmany,
                                   reinterpret_cast<fftwf_complex *> (itmp),
                                   nullptr, stride, dist,
                                   reinterpret_cast<fftwf_complex *> (out),
                                   nullptr, stride, dist, dir, plan_flags);
        }
      else
        {
          *cur_plan_p
            = fftwf_plan_many_dft (rank, tmp, howmany,
                                   reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *> (in)),
                                   nullptr, stride, dist,
                                   reinterpret_cast<fftwf_complex *> (out),
                                   nullptr, stride, dist, dir, plan_flags);
        }

      if (*cur_plan_p == nullptr)
        (*current_liboctave_error_handler) ("Error creating FFTW plan");
    }

  return *cur_plan_p;
}

void *
float_fftw_planner::do_create_plan (const int rank, const dim_vector& dims,
                                    octave_idx_type howmany,
                                    octave_idx_type stride,
                                    octave_idx_type dist,
                                    const float *in, FloatComplex *out)
{
  fftwf_plan *cur_plan_p = reinterpret_cast<fftwf_plan *> (&m_rplan);
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (m_rplan == nullptr || m_rd != dist || m_rs != stride || m_rr != rank
      || m_rh != howmany || ((ioalign != m_rsimd_align) ? ! ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != m_rn(i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      m_rd = dist;
      m_rs = stride;
      m_rr = rank;
      m_rh = howmany;
      m_rsimd_align = ioalign;
      m_rn = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (m_meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftwf_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (float, itmp, nn + 32);
          itmp = reinterpret_cast<float *>
                 (((reinterpret_cast<std::ptrdiff_t> (itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<std::ptrdiff_t> (in)) & 0xF));

          *cur_plan_p
            = fftwf_plan_many_dft_r2c (rank, tmp, howmany, itmp,
                                       nullptr, stride, dist,
                                       reinterpret_cast<fftwf_complex *> (out),
                                       nullptr, stride, dist, plan_flags);
        }
      else
        {
          *cur_plan_p
            = fftwf_plan_many_dft_r2c (rank, tmp, howmany,
                                       (const_cast<float *> (in)),
                                       nullptr, stride, dist,
                                       reinterpret_cast<fftwf_complex *> (out),
                                       nullptr, stride, dist, plan_flags);
        }

      if (*cur_plan_p == nullptr)
        (*current_liboctave_error_handler) ("Error creating FFTW plan");
    }

  return *cur_plan_p;
}

float_fftw_planner::FftwMethod
float_fftw_planner::do_method (void)
{
  return m_meth;
}

float_fftw_planner::FftwMethod
float_fftw_planner::do_method (FftwMethod _meth)
{
  FftwMethod ret = m_meth;
  if (_meth == ESTIMATE || _meth == MEASURE
      || _meth == PATIENT || _meth == EXHAUSTIVE
      || _meth == HYBRID)
    {
      if (m_meth != _meth)
        {
          m_meth = _meth;
          if (m_rplan)
            fftwf_destroy_plan (reinterpret_cast<fftwf_plan> (m_rplan));
          if (m_plan[0])
            fftwf_destroy_plan (reinterpret_cast<fftwf_plan> (m_plan[0]));
          if (m_plan[1])
            fftwf_destroy_plan (reinterpret_cast<fftwf_plan> (m_plan[1]));
          m_rplan = m_plan[0] = m_plan[1] = nullptr;
        }
    }
  else
    ret = UNKNOWN;
  return ret;
}

template <typename T>
static inline void
convert_packcomplex_1d (T *out, std::size_t nr, std::size_t nc,
                        octave_idx_type stride, octave_idx_type dist)
{
  octave_quit ();

  // Fill in the missing data.

  for (std::size_t i = 0; i < nr; i++)
    for (std::size_t j = nc/2+1; j < nc; j++)
      out[j*stride + i*dist] = conj (out[(nc - j)*stride + i*dist]);

  octave_quit ();
}

template <typename T>
static inline void
convert_packcomplex_Nd (T *out, const dim_vector& dv)
{
  std::size_t nc = dv(0);
  std::size_t nr = dv(1);
  std::size_t np = (dv.ndims () > 2 ? dv.numel () / nc / nr : 1);
  std::size_t nrp = nr * np;
  T *ptr1, *ptr2;

  octave_quit ();

  // Create space for the missing elements.

  for (std::size_t i = 0; i < nrp; i++)
    {
      ptr1 = out + i * (nc/2 + 1) + nrp*((nc-1)/2);
      ptr2 = out + i * nc;
      for (std::size_t j = 0; j < nc/2+1; j++)
        *ptr2++ = *ptr1++;
    }

  octave_quit ();

  // Fill in the missing data for the rank = 2 case directly for speed.

  for (std::size_t i = 0; i < np; i++)
    {
      for (std::size_t j = 1; j < nr; j++)
        for (std::size_t k = nc/2+1; k < nc; k++)
          out[k + (j + i*nr)*nc] = conj (out[nc - k + ((i+1)*nr - j)*nc]);

      for (std::size_t j = nc/2+1; j < nc; j++)
        out[j + i*nr*nc] = conj (out[(i*nr+1)*nc - j]);
    }

  octave_quit ();

  // Now do the permutations needed for rank > 2 cases.

  std::size_t jstart = dv(0) * dv(1);
  std::size_t kstep = dv(0);
  std::size_t nel = dv.numel ();

  for (int inner = 2; inner < dv.ndims (); inner++)
    {
      std::size_t jmax = jstart * dv(inner);
      for (std::size_t i = 0; i < nel; i+=jmax)
        for (std::size_t j = jstart, jj = jmax-jstart; j < jj;
             j+=jstart, jj-=jstart)
          for (std::size_t k = 0; k < jstart; k+= kstep)
            for (std::size_t l = nc/2+1; l < nc; l++)
              {
                T tmp = out[i+ j + k + l];
                out[i + j + k + l] = out[i + jj + k + l];
                out[i + jj + k + l] = tmp;
              }
      jstart = jmax;
    }

  octave_quit ();
}

int
fftw::fft (const double *in, Complex *out, std::size_t npts,
           std::size_t nsamples, octave_idx_type stride,
           octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = fftw_planner::create_plan (1, dv, nsamples,
                stride, dist, in, out);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft_r2c (m_plan, (const_cast<double *>(in)),
                        reinterpret_cast<fftw_complex *> (out));

  // Need to create other half of the transform.

  convert_packcomplex_1d (out, nsamples, npts, stride, dist);

  return 0;
}

int
fftw::fft (const Complex *in, Complex *out, std::size_t npts,
           std::size_t nsamples, octave_idx_type stride,
           octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = fftw_planner::create_plan (FFTW_FORWARD, 1, dv,
                nsamples, stride,
                dist, in, out);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft (m_plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  return 0;
}

int
fftw::ifft (const Complex *in, Complex *out, std::size_t npts,
            std::size_t nsamples, octave_idx_type stride,
            octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = fftw_planner::create_plan (FFTW_BACKWARD, 1, dv, nsamples,
                stride, dist, in, out);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft (m_plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  const Complex scale = npts;
  for (std::size_t j = 0; j < nsamples; j++)
    for (std::size_t i = 0; i < npts; i++)
      out[i*stride + j*dist] /= scale;

  return 0;
}

int
fftw::fftNd (const double *in, Complex *out, const int rank,
             const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  // Fool with the position of the start of the output matrix, so that
  // creating other half of the matrix won't cause cache problems.

  octave_idx_type offset = (dv.numel () / dv(0)) * ((dv(0) - 1) / 2);

  void *vplan = fftw_planner::create_plan (rank, dv, 1, 1, dist,
                in, out + offset);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft_r2c (m_plan, (const_cast<double *>(in)),
                        reinterpret_cast<fftw_complex *> (out+ offset));

  // Need to create other half of the transform.

  convert_packcomplex_Nd (out, dv);

  return 0;
}

int
fftw::fftNd (const Complex *in, Complex *out, const int rank,
             const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  void *vplan = fftw_planner::create_plan (FFTW_FORWARD, rank, dv,
                1, 1, dist, in, out);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft (m_plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  return 0;
}

int
fftw::ifftNd (const Complex *in, Complex *out, const int rank,
              const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  void *vplan = fftw_planner::create_plan (FFTW_BACKWARD, rank, dv,
                1, 1, dist, in, out);
  fftw_plan m_plan = reinterpret_cast<fftw_plan> (vplan);

  fftw_execute_dft (m_plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  const std::size_t npts = dv.numel ();
  const Complex scale = npts;
  for (std::size_t i = 0; i < npts; i++)
    out[i] /= scale;

  return 0;
}

int
fftw::fft (const float *in, FloatComplex *out, std::size_t npts,
           std::size_t nsamples, octave_idx_type stride,
           octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = float_fftw_planner::create_plan (1, dv, nsamples, stride,
                dist, in, out);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft_r2c (m_plan, (const_cast<float *>(in)),
                         reinterpret_cast<fftwf_complex *> (out));

  // Need to create other half of the transform.

  convert_packcomplex_1d (out, nsamples, npts, stride, dist);

  return 0;
}

int
fftw::fft (const FloatComplex *in, FloatComplex *out, std::size_t npts,
           std::size_t nsamples, octave_idx_type stride,
           octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = float_fftw_planner::create_plan (FFTW_FORWARD, 1, dv,
                nsamples, stride, dist,
                in, out);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft (m_plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  return 0;
}

int
fftw::ifft (const FloatComplex *in, FloatComplex *out, std::size_t npts,
            std::size_t nsamples, octave_idx_type stride,
            octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  void *vplan = float_fftw_planner::create_plan (FFTW_BACKWARD, 1, dv,
                nsamples, stride, dist,
                in, out);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft (m_plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  const FloatComplex scale = npts;
  for (std::size_t j = 0; j < nsamples; j++)
    for (std::size_t i = 0; i < npts; i++)
      out[i*stride + j*dist] /= scale;

  return 0;
}

int
fftw::fftNd (const float *in, FloatComplex *out, const int rank,
             const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  // Fool with the position of the start of the output matrix, so that
  // creating other half of the matrix won't cause cache problems.

  octave_idx_type offset = (dv.numel () / dv(0)) * ((dv(0) - 1) / 2);

  void *vplan = float_fftw_planner::create_plan (rank, dv, 1, 1, dist,
                in, out + offset);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft_r2c (m_plan, (const_cast<float *>(in)),
                         reinterpret_cast<fftwf_complex *> (out+ offset));

  // Need to create other half of the transform.

  convert_packcomplex_Nd (out, dv);

  return 0;
}

int
fftw::fftNd (const FloatComplex *in, FloatComplex *out, const int rank,
             const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  void *vplan = float_fftw_planner::create_plan (FFTW_FORWARD, rank, dv,
                1, 1, dist, in, out);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft (m_plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  return 0;
}

int
fftw::ifftNd (const FloatComplex *in, FloatComplex *out, const int rank,
              const dim_vector& dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  void *vplan = float_fftw_planner::create_plan (FFTW_BACKWARD, rank, dv,
                1, 1, dist, in, out);
  fftwf_plan m_plan = reinterpret_cast<fftwf_plan> (vplan);

  fftwf_execute_dft (m_plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  const std::size_t npts = dv.numel ();
  const FloatComplex scale = npts;
  for (std::size_t i = 0; i < npts; i++)
    out[i] /= scale;

  return 0;
}

#endif

std::string
fftw_version (void)
{
#if defined (HAVE_FFTW)
  return ::fftw_version;
#else
  return "none";
#endif
}

std::string
fftwf_version (void)
{
#if defined (HAVE_FFTW)
  return ::fftwf_version;
#else
  return "none";
#endif
}

OCTAVE_END_NAMESPACE(octave)
