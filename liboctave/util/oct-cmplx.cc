////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#include <complex>

// For complex-complex and complex-real comparisons, Octave uses the following
// ordering: compare absolute values first; if they match, compare phase
// angles.  This is partially inconsistent with M*b, which compares complex
// numbers only by their real parts; OTOH, it uses the same definition for
// max/min and sort.  The abs/arg comparison is definitely more useful (the
// other one is emulated rather trivially), so let's be consistent and use that
// all over.

// The standard C library function arg() returns [-pi,pi], which creates a
// non-unique representation for numbers along the negative real axis branch
// cut.  Change this to principal value (-pi,pi] by mapping -pi to pi.

// General templated code for all (double, float) complex operators
#define DEF_COMPLEXR_COMP(OP, OPS)                                           \
   template <typename T>                                                     \
   bool operator OP (const std::complex<T>& a, const std::complex<T>& b)     \
   {                                                                         \
     OCTAVE_FLOAT_TRUNCATE const T ax = std::abs (a);                        \
     OCTAVE_FLOAT_TRUNCATE const T bx = std::abs (b);                        \
     if (ax == bx)                                                           \
       {                                                                     \
         OCTAVE_FLOAT_TRUNCATE const T ay = std::arg (a);                    \
         OCTAVE_FLOAT_TRUNCATE const T by = std::arg (b);                    \
         if (ay == static_cast<T> (-M_PI))                                   \
           {                                                                 \
             if (by != static_cast<T> (-M_PI))                               \
               return static_cast<T> (M_PI) OP by;                           \
           }                                                                 \
         else if (by == static_cast<T> (-M_PI))                              \
           {                                                                 \
             return ay OP static_cast<T> (M_PI);                             \
           }                                                                 \
         return ay OP by;                                                    \
       }                                                                     \
     else                                                                    \
       return ax OPS bx;                                                     \
   }                                                                         \
   template <typename T>                                                     \
   bool operator OP (const std::complex<T>& a, T b)                          \
   {                                                                         \
     OCTAVE_FLOAT_TRUNCATE const T ax = std::abs (a);                        \
     OCTAVE_FLOAT_TRUNCATE const T bx = std::abs (b);                        \
     if (ax == bx)                                                           \
       {                                                                     \
         OCTAVE_FLOAT_TRUNCATE const T ay = std::arg (a);                    \
         if (ay == static_cast<T> (-M_PI))                                   \
           return static_cast<T> (M_PI) OP 0;                                \
         return ay OP 0;                                                     \
       }                                                                     \
     else                                                                    \
       return ax OPS bx;                                                     \
   }                                                                         \
   template <typename T>                                                     \
   bool operator OP (T a, const std::complex<T>& b)                          \
   {                                                                         \
     OCTAVE_FLOAT_TRUNCATE const T ax = std::abs (a);                        \
     OCTAVE_FLOAT_TRUNCATE const T bx = std::abs (b);                        \
     if (ax == bx)                                                           \
       {                                                                     \
         OCTAVE_FLOAT_TRUNCATE const T by = std::arg (b);                    \
         if (by == static_cast<T> (-M_PI))                                   \
           return 0 OP static_cast<T> (M_PI);                                \
         return 0 OP by;                                                     \
       }                                                                     \
     else                                                                    \
       return ax OPS bx;                                                     \
   }

#if defined (__APPLE__)
   // Apple specializations

   // FIXME: Apple's math library chooses to return a different value for
   // std::arg with floats than the constant static_cast<float> (M_PI).
   // Work around this obtuse behavior by providing the constant A_PI which
   // is Apple's definition of the constant PI for float variables.
   // The template code for doubles is the same as that for UNIX platforms.
   // Use C++ template specialization to add specific functions for float
   // values that make use of A_PI.

   // Apple version of PI in single precision
#  define A_PI 3.1415925025939941f

#  define INST_SPECIAL_COMPLEXR_COMP(OP, OPS)                                \
     template <> OCTAVE_API                                                  \
     bool operator OP<float> (const std::complex<float>& a,                  \
                              const std::complex<float>& b)                  \
     {                                                                       \
       OCTAVE_FLOAT_TRUNCATE const float ax = std::abs (a);                  \
       OCTAVE_FLOAT_TRUNCATE const float bx = std::abs (b);                  \
       if (ax == bx)                                                         \
         {                                                                   \
           OCTAVE_FLOAT_TRUNCATE const float ay = std::arg (a);              \
           OCTAVE_FLOAT_TRUNCATE const float by = std::arg (b);              \
           if (ay == -A_PI)                                                  \
             {                                                               \
               if (by != -A_PI)                                              \
                 return static_cast<float> (M_PI) OP by;                     \
             }                                                               \
           else if (by == -A_PI)                                             \
             {                                                               \
               return ay OP static_cast<float> (M_PI);                       \
             }                                                               \
           return ay OP by;                                                  \
         }                                                                   \
       else                                                                  \
         return ax OPS bx;                                                   \
     }                                                                       \
     template <> OCTAVE_API                                                  \
     bool operator OP<float> (const std::complex<float>& a, float b)         \
     {                                                                       \
       OCTAVE_FLOAT_TRUNCATE const float ax = std::abs (a);                  \
       OCTAVE_FLOAT_TRUNCATE const float bx = std::abs (b);                  \
       if (ax == bx)                                                         \
         {                                                                   \
           OCTAVE_FLOAT_TRUNCATE const float ay = std::arg (a);              \
           if (ay == -A_PI)                                                  \
             return static_cast<float> (M_PI) OP 0;                          \
           return ay OP 0;                                                   \
         }                                                                   \
       else                                                                  \
         return ax OPS bx;                                                   \
     }                                                                       \
     template <> OCTAVE_API                                                  \
     bool operator OP<float> (float a, const std::complex<float>& b)         \
     {                                                                       \
       OCTAVE_FLOAT_TRUNCATE const float ax = std::abs (a);                  \
       OCTAVE_FLOAT_TRUNCATE const float bx = std::abs (b);                  \
       if (ax == bx)                                                         \
         {                                                                   \
           OCTAVE_FLOAT_TRUNCATE const float by = std::arg (b);              \
           if (by == -A_PI)                                                  \
             return 0 OP static_cast<float> (M_PI);                          \
           return 0 OP by;                                                   \
         }                                                                   \
       else                                                                  \
         return ax OPS bx;                                                   \
     }

#endif

DEF_COMPLEXR_COMP (>, >)
DEF_COMPLEXR_COMP (<, <)
DEF_COMPLEXR_COMP (>=, >)
DEF_COMPLEXR_COMP (<=, <)


// Instantiate templates

# define INST_COMPLEXR_COMP(OP, T)                                           \
    template OCTAVE_API bool operator OP (const std::complex<T>& a,          \
                                          const std::complex<T>& b);         \
    template OCTAVE_API bool operator OP (const std::complex<T>& a, T b);    \
    template OCTAVE_API bool operator OP (T a, const std::complex<T>& b);

INST_COMPLEXR_COMP (>, double)
INST_COMPLEXR_COMP (<, double)
INST_COMPLEXR_COMP (>=, double)
INST_COMPLEXR_COMP (<=, double)

#if defined (__APPLE__)
INST_SPECIAL_COMPLEXR_COMP (>, >)
INST_SPECIAL_COMPLEXR_COMP (<, <)
INST_SPECIAL_COMPLEXR_COMP (>=, >)
INST_SPECIAL_COMPLEXR_COMP (<=, <)
#else
INST_COMPLEXR_COMP (>, float)
INST_COMPLEXR_COMP (<, float)
INST_COMPLEXR_COMP (>=, float)
INST_COMPLEXR_COMP (<=, float)
#endif
