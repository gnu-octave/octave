////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2020 The Octave Project Developers
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

*/

#if ! defined (octave_randmtzig_h)
#define octave_randmtzig_h 1

#include "octave-config.h"

#define MT_N 624

namespace octave
{
  // Mersenne Twister.

  extern void init_mersenne_twister (void);
  extern void init_mersenne_twister (const uint32_t seed);
  extern void init_mersenne_twister (const uint32_t *init_key,
                                     const int key_length);

  extern void set_mersenne_twister_state (const uint32_t *save);
  extern void get_mersenne_twister_state (uint32_t *save);

  template <typename T> T rand_uniform (void);
  template <typename T> T rand_normal (void);
  template <typename T> T rand_exponential (void);

  template <typename T> void rand_uniform (octave_idx_type n, T *p);
  template <typename T> void rand_normal (octave_idx_type n, T *p);
  template <typename T> void rand_exponential (octave_idx_type n, T *p);

  template <> double rand_uniform<double> (void);
  template <> double rand_normal<double> (void);
  template <> double rand_exponential<double> (void);

  template <> float rand_uniform<float> (void);
  template <> float rand_normal<float> (void);
  template <> float rand_exponential<float> (void);

  template <> void
  rand_uniform<double> (octave_idx_type n, double *p);

  template <> void
  rand_normal<double> (octave_idx_type n, double *p);

  template <> void
  rand_exponential<double> (octave_idx_type n, double *p);

  template <> void
  rand_uniform<float> (octave_idx_type n, float *p);

  template <> void
  rand_normal<float> (octave_idx_type n, float *p);

  template <> void
  rand_exponential<float> (octave_idx_type n, float *p);
}

#endif
