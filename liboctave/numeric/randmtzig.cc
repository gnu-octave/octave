////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

/*
   A C-program for MT19937, with initialization improved 2002/2/10.
   Coded by Takuji Nishimura and Makoto Matsumoto.
   This is a faster version by taking Shawn Cokus's optimization,
   Matthe Bellew's simplification, Isaku Wada's real version.
   David Bateman added normal and exponential distributions following
   Marsaglia and Tang's Ziggurat algorithm.

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   Copyright (C) 2004, David Bateman
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp

   * 2004-01-19 Paul Kienzle
   * * comment out main
   * add init_by_entropy, get_state, set_state
   * * converted to allow compiling by C++ compiler
   *
   * 2004-01-25 David Bateman
   * * Add Marsaglia and Tsang Ziggurat code
   *
   * 2004-07-13 Paul Kienzle
   * * make into an independent library with some docs.
   * * introduce new main and test code.
   *
   * 2004-07-28 Paul Kienzle & David Bateman
   * * add -DALLBITS flag for 32 vs. 53 bits of randomness in mantissa
   * * make the naming scheme more uniform
   * * add -DHAVE_X86 for faster support of 53 bit mantissa on x86 arch.
   *
   * 2005-02-23 Paul Kienzle
   * * fix -DHAVE_X86_32 flag and add -DUSE_X86_32=0|1 for explicit control
   *
   * 2006-04-01 David Bateman
   * * convert for use in octave, declaring static functions only used
   *   here and adding oct_ to functions visible externally
   * * inverse sense of ALLBITS
   *
   * 2012-05-18 David Bateman
   * * Remove randu64 and ALLBIT option
   * * Add the single precision generators
   */

/*
   === Build instructions ===

   Compile with -DHAVE_GETTIMEOFDAY if the gettimeofday function is
   available.  This is not necessary if your architecture has
   /dev/urandom defined.

   Uses implicit -Di386 or explicit -DHAVE_X86_32 to determine if CPU=x86.
   You can force X86 behavior with -DUSE_X86_32=1, or suppress it with
   -DUSE_X86_32=0. You should also consider -march=i686 or similar for
   extra performance. Check whether -DUSE_X86_32=0 is faster on 64-bit
   x86 architectures.

   If you want to replace the Mersenne Twister with another
   generator then redefine randi32 appropriately.

   === Usage instructions ===
   Before using any of the generators, initialize the state with one of
   the init_mersenne_twister functions.

   All generators share the same state vector.

   === Mersenne Twister ===
   random initial state:
   void init_mersenne_twister (void)

   // 32-bit initial state:
   void init_mersenne_twister (uint32_t s)

   // m*32-bit initial state:
   void init_mersenne_twister (uint32_t k[],int m)

   // saves state in array:
   void get_mersenne_twister_state (uint32_t save[MT_N+1])

   // restores state from array
   void set_mersenne_twister_state (uint32_t save[MT_N+1])

   static uint32_t randmt (void)               returns 32-bit unsigned int

   === inline generators ===
   static uint32_t randi32 (void)   returns 32-bit unsigned int
   static uint64_t randi53 (void)   returns 53-bit unsigned int
   static uint64_t randi54 (void)   returns 54-bit unsigned int
   static float randu24 (void)      returns 24-bit uniform in (0,1)
   static double randu53 (void)     returns 53-bit uniform in (0,1)

   double rand_uniform (void)       returns M-bit uniform in (0,1)
   double rand_normal (void)        returns M-bit standard normal
   double rand_exponential (void)   returns N-bit standard exponential

   === Array generators ===
   void rand_uniform (octave_idx_type, double [])
   void rand_normal (octave_idx_type, double [])
   void rand_exponential (octave_idx_type, double [])
*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>
#include <cstdio>
#include <ctime>

#include <algorithm>
#include <random>

#include "oct-syscalls.h"
#include "oct-time.h"
#include "randmtzig.h"

/* FIXME: may want to suppress X86 if sizeof(long) > 4 */
#if ! defined (USE_X86_32)
#  if defined (i386) || defined (HAVE_X86_32)
#    define USE_X86_32 1
#  else
#    define USE_X86_32 0
#  endif
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

/* ===== Mersenne Twister 32-bit generator ===== */

#define MT_M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UMASK 0x80000000UL /* most significant w-r bits */
#define LMASK 0x7fffffffUL /* least significant r bits */
#define MIXBITS(u,v) ( ((u) & UMASK) | ((v) & LMASK) )
#define TWIST(u,v) ((MIXBITS(u,v) >> 1) ^ ((v)&1UL ? MATRIX_A : 0UL))

static uint32_t *next;
static uint32_t state[MT_N]; /* the array for the state vector  */
static int left = 1;
static int initf = 0;
static int initt = 1;
static int inittf = 1;

/* initializes state[MT_N] with a seed */
void init_mersenne_twister (const uint32_t s)
{
  int j;
  state[0] = s & 0xffffffffUL;
  for (j = 1; j < MT_N; j++)
    {
      state[j] = (1812433253UL * (state[j-1] ^ (state[j-1] >> 30)) + j);
      /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
      /* In the previous versions, MSBs of the seed affect   */
      /* only MSBs of the array state[].                        */
      /* 2002/01/09 modified by Makoto Matsumoto             */
      state[j] &= 0xffffffffUL;  /* for >32 bit machines */
    }
  left = 1;
  initf = 1;
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
void init_mersenne_twister (const uint32_t *init_key, const int key_length)
{
  int i, j, k;
  init_mersenne_twister (19650218UL);
  i = 1;
  j = 0;
  k = (MT_N > key_length ? MT_N : key_length);
  for (; k; k--)
    {
      state[i] = (state[i] ^ ((state[i-1] ^ (state[i-1] >> 30)) * 1664525UL))
                 + init_key[j] + j; /* non linear */
      state[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      j++;
      if (i >= MT_N)
        {
          state[0] = state[MT_N-1];
          i = 1;
        }
      if (j >= key_length)
        j = 0;
    }
  for (k = MT_N - 1; k; k--)
    {
      state[i] = (state[i] ^ ((state[i-1] ^ (state[i-1] >> 30)) * 1566083941UL))
                 - i; /* non linear */
      state[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
      i++;
      if (i >= MT_N)
        {
          state[0] = state[MT_N-1];
          i = 1;
        }
    }

  state[0] = 0x80000000UL; /* MSB is 1; assuring nonzero initial array */
  left = 1;
  initf = 1;
}

void init_mersenne_twister (void)
{
  uint32_t entropy[MT_N];
  int n = 0;

  // Gather some entropy from various sources

  sys::time now;

  // Current time in seconds
  if (n < MT_N)
    entropy[n++] = now.unix_time ();

  // CPU time used (usec)
  if (n < MT_N)
    entropy[n++] = clock ();

  // Fractional part of current time
  if (n < MT_N)
    entropy[n++] = now.usec ();

  // Include the PID to make sure that several processes reaching here at the
  // same time use different random numbers.
  if (n < MT_N)
    entropy[n++] = sys::getpid ();

  if (n < MT_N)
    {
      try
        {
          // The standard doesn't *guarantee* that random_device provides
          // non-deterministic random numbers. So add entropy from this
          // source last to make sure we gathered at least some entropy from
          // the earlier sources.
          std::random_device rd;
          std::uniform_int_distribution<uint32_t> dist;
          // Add 1024 bit of "true" entropy
          int n_max = std::min (n + 32, MT_N);
          while (n < n_max)
            entropy[n++] = dist (rd);
        }
      catch (const std::exception&)
        {
          // Just ignore any exception and skip that source of entropy.
        }
    }

  // Send all the entropy into the initial state vector
  init_mersenne_twister (entropy, n);
}

void set_mersenne_twister_state (const uint32_t *save)
{
  std::copy_n (save, MT_N, state);
  left = save[MT_N];
  next = state + (MT_N - left + 1);
}

void get_mersenne_twister_state (uint32_t *save)
{
  std::copy_n (state, MT_N, save);
  save[MT_N] = left;
}

static void next_state (void)
{
  uint32_t *p = state;
  int j;

  /* if init_by_int() has not been called, */
  /* a default initial seed is used         */
  /* if (initf==0) init_by_int(5489UL); */
  /* Or better yet, a random seed! */
  if (initf == 0)
    init_mersenne_twister ();

  left = MT_N;
  next = state;

  for (j = MT_N - MT_M + 1; --j; p++)
    *p = p[MT_M] ^ TWIST(p[0], p[1]);

  for (j = MT_M; --j; p++)
    *p = p[MT_M-MT_N] ^ TWIST(p[0], p[1]);

  *p = p[MT_M-MT_N] ^ TWIST(p[0], state[0]);
}

/* generates a random number on [0,0xffffffff]-interval */
static uint32_t randmt (void)
{
  uint32_t y;

  if (--left == 0)
    next_state ();
  y = *next++;

  /* Tempering */
  y ^= (y >> 11);
  y ^= (y << 7) & 0x9d2c5680UL;
  y ^= (y << 15) & 0xefc60000UL;
  return (y ^ (y >> 18));
}

/* ===== Uniform generators ===== */

/* Select which 32 bit generator to use */
#define randi32 randmt

static uint64_t randi53 (void)
{
  const uint32_t lo = randi32 ();
  const uint32_t hi = randi32 () & 0x1FFFFF;
#if defined (HAVE_X86_32)
  uint64_t u;
  uint32_t *p = (uint32_t *)&u;
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return ((static_cast<uint64_t> (hi) << 32) | lo);
#endif
}

static uint64_t randi54 (void)
{
  const uint32_t lo = randi32 ();
  const uint32_t hi = randi32 () & 0x3FFFFF;
#if defined (HAVE_X86_32)
  uint64_t u;
  uint32_t *p = static_cast<uint32_t *> (&u);
  p[0] = lo;
  p[1] = hi;
  return u;
#else
  return ((static_cast<uint64_t> (hi) << 32) | lo);
#endif
}

/* generates a random number on (0,1)-real-interval */
static float randu24 (void)
{
  uint32_t i;

  do
    {
      i = randi32 () & static_cast<uint32_t> (0xFFFFFF);
    }
  while (i == 0);

  return i * (1.0f / 16777216.0f);
}

/* generates a random number on (0,1) with 53-bit resolution */
static double randu53 (void)
{
  int32_t a, b;

  do
    {
      a = randi32 () >> 5;
      b = randi32 () >> 6;
    }
  while (a == 0 && b == 0);

  return (a*67108864.0 + b) * (1.0/9007199254740992.0);
}

/* Determine mantissa for uniform doubles */
template <>
OCTAVE_API double
rand_uniform<double> (void)
{
  return randu53 ();
}

/* Determine mantissa for uniform floats */
template <>
OCTAVE_API float
rand_uniform<float> (void)
{
  return randu24 ();
}

/* ===== Ziggurat normal and exponential generators ===== */

#define ZIGGURAT_TABLE_SIZE 256

#define ZIGGURAT_NOR_R 3.6541528853610088
#define ZIGGURAT_NOR_INV_R 0.27366123732975828
#define NOR_SECTION_AREA 0.00492867323399

#define ZIGGURAT_EXP_R 7.69711747013104972
#define ZIGGURAT_EXP_INV_R 0.129918765548341586
#define EXP_SECTION_AREA 0.0039496598225815571993

#define ZIGINT uint64_t
#define EMANTISSA 9007199254740992.0  /* 53 bit mantissa */
#define ERANDI randi53() /* 53 bits for mantissa */
#define NMANTISSA EMANTISSA
#define NRANDI randi54() /* 53 bits for mantissa + 1 bit sign */
#define RANDU randu53()

static ZIGINT ki[ZIGGURAT_TABLE_SIZE];
static double wi[ZIGGURAT_TABLE_SIZE], fi[ZIGGURAT_TABLE_SIZE];
static ZIGINT ke[ZIGGURAT_TABLE_SIZE];
static double we[ZIGGURAT_TABLE_SIZE], fe[ZIGGURAT_TABLE_SIZE];

/*
  This code is based on the paper Marsaglia and Tsang, "The ziggurat method
  for generating random variables", Journ. Statistical Software. Code was
  presented in this paper for a Ziggurat of 127 levels and using a 32 bit
  integer random number generator. This version of the code, uses the
  Mersenne Twister as the integer generator and uses 256 levels in the
  Ziggurat. This has several advantages.

  1) As Marsaglia and Tsang themselves states, the more levels the few
  times the expensive tail algorithm must be called
  2) The cycle time of the generator is determined by the integer
  generator, thus the use of a Mersenne Twister for the core random
  generator makes this cycle extremely long.
  3) The license on the original code was unclear, thus rewriting the code
  from the article means we are free of copyright issues.
  4) Compile flag for full 53-bit random mantissa.

  It should be stated that the authors made my life easier, by the fact that
  the algorithm developed in the text of the article is for a 256 level
  ziggurat, even if the code itself isn't...

  One modification to the algorithm developed in the article, is that it is
  assumed that 0 <= x < Inf, and "unsigned long"s are used, thus resulting in
  terms like 2^32 in the code. As the normal distribution is defined between
  -Inf < x < Inf, we effectively only have 31 bit integers plus a sign. Thus
  in Marsaglia and Tsang, terms like 2^32 become 2^31. We use NMANTISSA for
  this term.  The exponential distribution is one sided so we use the
  full 32 bits.  We use EMANTISSA for this term.

  It appears that I'm slightly slower than the code in the article, this
  is partially due to a better generator of random integers than they
  use. But might also be that the case of rapid return was optimized by
  inlining the relevant code with a #define. As the basic Mersenne
  Twister is only 25% faster than this code I suspect that the main
  reason is just the use of the Mersenne Twister and not the inlining,
  so I'm not going to try and optimize further.
*/

void create_ziggurat_tables (void)
{
  int i;
  double x, x1;

  /* Ziggurat tables for the normal distribution */
  x1 = ZIGGURAT_NOR_R;
  wi[255] = x1 / NMANTISSA;
  fi[255] = exp (-0.5 * x1 * x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^31 * r * f(r) / v, w_0 = 0.5^31 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  ki[0] = static_cast<ZIGINT> (x1 * fi[255] / NOR_SECTION_AREA * NMANTISSA);
  wi[0] = NOR_SECTION_AREA / fi[255] / NMANTISSA;
  fi[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-0.5*x*x) -> x = sqrt(-2*ln(y))
       */
      x = std::sqrt (-2. * std::log (NOR_SECTION_AREA / x1 + fi[i+1]));
      ki[i+1] = static_cast<ZIGINT> (x / x1 * NMANTISSA);
      wi[i] = x / NMANTISSA;
      fi[i] = exp (-0.5 * x * x);
      x1 = x;
    }

  ki[1] = 0;

  /* Zigurrat tables for the exponential distribution */
  x1 = ZIGGURAT_EXP_R;
  we[255] = x1 / EMANTISSA;
  fe[255] = exp (-x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^32 * r * f(r) / v, w_0 = 0.5^32 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  ke[0] = static_cast<ZIGINT> (x1 * fe[255] / EXP_SECTION_AREA * EMANTISSA);
  we[0] = EXP_SECTION_AREA / fe[255] / EMANTISSA;
  fe[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-x) -> x = -ln(y)
       */
      x = - std::log (EXP_SECTION_AREA / x1 + fe[i+1]);
      ke[i+1] = static_cast<ZIGINT> (x / x1 * EMANTISSA);
      we[i] = x / EMANTISSA;
      fe[i] = exp (-x);
      x1 = x;
    }
  ke[1] = 0;

  initt = 0;
}

/*
 * Here is the guts of the algorithm. As Marsaglia and Tsang state the
 * algorithm in their paper
 *
 * 1) Calculate a random signed integer j and let i be the index
 *     provided by the rightmost 8-bits of j
 * 2) Set x = j * w_i. If j < k_i return x
 * 3) If i = 0, then return x from the tail
 * 4) If [f(x_{i-1}) - f(x_i)] * U < f(x) - f(x_i), return x
 * 5) goto step 1
 *
 * Where f is the functional form of the distribution, which for a normal
 * distribution is exp(-0.5*x*x)
 */


template <> OCTAVE_API double rand_normal<double> (void)
{
                                                   if (initt)
                                                   create_ziggurat_tables ();

  while (1)
    {
      /* The following code is specialized for 32-bit mantissa.
       * Compared to the arbitrary mantissa code, there is a performance
       * gain for 32-bits:  PPC: 2%, MIPS: 8%, x86: 40%
       * There is a bigger performance gain compared to using a full
       * 53-bit mantissa:  PPC: 60%, MIPS: 65%, x86: 240%
       * Of course, different compilers and operating systems may
       * have something to do with this.
       */
# if defined (HAVE_X86_32)
      /* 53-bit mantissa, 1-bit sign, x86 32-bit architecture */
      double x;
      int si, idx;
      uint32_t lo, hi;
      int64_t rabs;
      uint32_t *p = (uint32_t *)&rabs;
      lo = randi32 ();
      idx = lo & 0xFF;
      hi = randi32 ();
      si = hi & UMASK;
      p[0] = lo;
      p[1] = hi & 0x1FFFFF;
      x = ( si ? -rabs : rabs ) * wi[idx];
# else
      /* arbitrary mantissa (selected by NRANDI, with 1 bit for sign) */
      const uint64_t r = NRANDI;
      const int64_t rabs = r >> 1;
      const int idx = static_cast<int> (rabs & 0xFF);
      const double x = ( (r & 1) ? -rabs : rabs) * wi[idx];
# endif
      if (rabs < static_cast<int64_t> (ki[idx]))
        return x;        /* 99.3% of the time we return here 1st try */
      else if (idx == 0)
        {
          /* As stated in Marsaglia and Tsang
           *
           * For the normal tail, the method of Marsaglia[5] provides:
           * generate x = -ln(U_1)/r, y = -ln(U_2), until y+y > x*x,
           * then return r+x. Except that r+x is always in the positive
           * tail!!!! Any thing random might be used to determine the
           * sign, but as we already have r we might as well use it
           *
           * [PAK] but not the bottom 8 bits, since they are all 0 here!
           */
          double xx, yy;
          do
            {
              xx = - ZIGGURAT_NOR_INV_R * std::log (RANDU);
              yy = - std::log (RANDU);
            }
          while ( yy+yy <= xx*xx);
          return ((rabs & 0x100) ? -ZIGGURAT_NOR_R-xx : ZIGGURAT_NOR_R+xx);
        }
      else if ((fi[idx-1] - fi[idx]) * RANDU + fi[idx] < exp (-0.5*x*x))
        return x;
    }
}

template <> OCTAVE_API double rand_exponential<double> (void)
{
                                                        if (initt)
                                                        create_ziggurat_tables ();

  while (1)
    {
      ZIGINT ri = ERANDI;
      const int idx = static_cast<int> (ri & 0xFF);
      const double x = ri * we[idx];
      if (ri < ke[idx])
        return x;               /* 98.9% of the time we return here 1st try */
      else if (idx == 0)
        {
          /* As stated in Marsaglia and Tsang
           *
           * For the exponential tail, the method of Marsaglia[5] provides:
           * x = r - ln(U);
           */
          return ZIGGURAT_EXP_R - std::log (RANDU);
        }
      else if ((fe[idx-1] - fe[idx]) * RANDU + fe[idx] < exp (-x))
        return x;
    }
}

template <> OCTAVE_API void rand_uniform<double> (octave_idx_type n, double *p)
{
                                                  std::generate_n (p, n, [](void) { return rand_uniform<double> (); });
}

template <> OCTAVE_API void rand_normal (octave_idx_type n, double *p)
{
                                         std::generate_n (p, n, [](void) { return rand_normal<double> (); });
}

template <> OCTAVE_API void rand_exponential (octave_idx_type n, double *p)
{
                                              std::generate_n (p, n, [](void) { return rand_exponential<double> (); });
}

#undef ZIGINT
#undef EMANTISSA
#undef ERANDI
#undef NMANTISSA
#undef NRANDI
#undef RANDU

#define ZIGINT uint32_t
#define EMANTISSA 4294967296.0 /* 32 bit mantissa */
#define ERANDI randi32() /* 32 bits for mantissa */
#define NMANTISSA 2147483648.0 /* 31 bit mantissa */
#define NRANDI randi32() /* 31 bits for mantissa + 1 bit sign */
#define RANDU randu24()

static ZIGINT fki[ZIGGURAT_TABLE_SIZE];
static float fwi[ZIGGURAT_TABLE_SIZE], ffi[ZIGGURAT_TABLE_SIZE];
static ZIGINT fke[ZIGGURAT_TABLE_SIZE];
static float fwe[ZIGGURAT_TABLE_SIZE], ffe[ZIGGURAT_TABLE_SIZE];

static void create_ziggurat_float_tables (void)
{
  int i;
  float x, x1;

  /* Ziggurat tables for the normal distribution */
  x1 = ZIGGURAT_NOR_R;
  fwi[255] = x1 / NMANTISSA;
  ffi[255] = exp (-0.5 * x1 * x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^31 * r * f(r) / v, w_0 = 0.5^31 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  fki[0] = static_cast<ZIGINT> (x1 * ffi[255] / NOR_SECTION_AREA * NMANTISSA);
  fwi[0] = NOR_SECTION_AREA / ffi[255] / NMANTISSA;
  ffi[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-0.5*x*x) -> x = sqrt(-2*ln(y))
       */
      x = std::sqrt (-2. * std::log (NOR_SECTION_AREA / x1 + ffi[i+1]));
      fki[i+1] = static_cast<ZIGINT> (x / x1 * NMANTISSA);
      fwi[i] = x / NMANTISSA;
      ffi[i] = exp (-0.5 * x * x);
      x1 = x;
    }

  fki[1] = 0;

  /* Zigurrat tables for the exponential distribution */
  x1 = ZIGGURAT_EXP_R;
  fwe[255] = x1 / EMANTISSA;
  ffe[255] = exp (-x1);

  /* Index zero is special for tail strip, where Marsaglia and Tsang
   * defines this as
   * k_0 = 2^32 * r * f(r) / v, w_0 = 0.5^32 * v / f(r), f_0 = 1,
   * where v is the area of each strip of the ziggurat.
   */
  fke[0] = static_cast<ZIGINT> (x1 * ffe[255] / EXP_SECTION_AREA * EMANTISSA);
  fwe[0] = EXP_SECTION_AREA / ffe[255] / EMANTISSA;
  ffe[0] = 1.;

  for (i = 254; i > 0; i--)
    {
      /* New x is given by x = f^{-1}(v/x_{i+1} + f(x_{i+1})), thus
       * need inverse operator of y = exp(-x) -> x = -ln(y)
       */
      x = - std::log (EXP_SECTION_AREA / x1 + ffe[i+1]);
      fke[i+1] = static_cast<ZIGINT> (x / x1 * EMANTISSA);
      fwe[i] = x / EMANTISSA;
      ffe[i] = exp (-x);
      x1 = x;
    }
  fke[1] = 0;

  inittf = 0;
}

/*
 * Here is the guts of the algorithm. As Marsaglia and Tsang state the
 * algorithm in their paper
 *
 * 1) Calculate a random signed integer j and let i be the index
 *     provided by the rightmost 8-bits of j
 * 2) Set x = j * w_i. If j < k_i return x
 * 3) If i = 0, then return x from the tail
 * 4) If [f(x_{i-1}) - f(x_i)] * U < f(x) - f(x_i), return x
 * 5) goto step 1
 *
 * Where f is the functional form of the distribution, which for a normal
 * distribution is exp(-0.5*x*x)
 */

template <> OCTAVE_API float rand_normal<float> (void)
{
                                                 if (inittf)
                                                 create_ziggurat_float_tables ();

  while (1)
    {
      /* 32-bit mantissa */
      const uint32_t r = randi32 ();
      const uint32_t rabs = r & LMASK;
      const int idx = static_cast<int> (r & 0xFF);
      const float x = static_cast<int32_t> (r) * fwi[idx];
      if (rabs < fki[idx])
        return x;        /* 99.3% of the time we return here 1st try */
      else if (idx == 0)
        {
          /* As stated in Marsaglia and Tsang
           *
           * For the normal tail, the method of Marsaglia[5] provides:
           * generate x = -ln(U_1)/r, y = -ln(U_2), until y+y > x*x,
           * then return r+x. Except that r+x is always in the positive
           * tail!!!! Any thing random might be used to determine the
           * sign, but as we already have r we might as well use it
           *
           * [PAK] but not the bottom 8 bits, since they are all 0 here!
           */
          float xx, yy;
          do
            {
              xx = - ZIGGURAT_NOR_INV_R * std::log (RANDU);
              yy = - std::log (RANDU);
            }
          while ( yy+yy <= xx*xx);
          return ((rabs & 0x100) ? -ZIGGURAT_NOR_R-xx : ZIGGURAT_NOR_R+xx);
        }
      else if ((ffi[idx-1] - ffi[idx]) * RANDU + ffi[idx] < exp (-0.5*x*x))
        return x;
    }
}

template <> OCTAVE_API float rand_exponential<float> (void)
{
                                                      if (inittf)
                                                      create_ziggurat_float_tables ();

  while (1)
    {
      ZIGINT ri = ERANDI;
      const int idx = static_cast<int> (ri & 0xFF);
      const float x = ri * fwe[idx];
      if (ri < fke[idx])
        return x;               /* 98.9% of the time we return here 1st try */
      else if (idx == 0)
        {
          /* As stated in Marsaglia and Tsang
           *
           * For the exponential tail, the method of Marsaglia[5] provides:
           * x = r - ln(U);
           */
          return ZIGGURAT_EXP_R - std::log (RANDU);
        }
      else if ((ffe[idx-1] - ffe[idx]) * RANDU + ffe[idx] < exp (-x))
        return x;
    }
}

template <> OCTAVE_API void rand_uniform (octave_idx_type n, float *p)
{
                                          std::generate_n (p, n, [](void) { return rand_uniform<float> (); });
}

template <> OCTAVE_API void rand_normal (octave_idx_type n, float *p)
{
                                         std::generate_n (p, n, [](void) { return rand_normal<float> (); });
}

template <> OCTAVE_API void rand_exponential (octave_idx_type n, float *p)
{
                                              std::generate_n (p, n, [](void) { return rand_exponential<float> (); });
}

OCTAVE_END_NAMESPACE(octave)

