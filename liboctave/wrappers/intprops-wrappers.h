////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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

#if ! defined (octave_localcharset_wrapper_h)
#define octave_localcharset_wrapper_h 1

#include "octave-config.h"

#if defined __cplusplus
extern "C" {
#endif

// These functions return 1 if the operation between the input arguments would
// overflow.

extern OCTAVE_API int
octave_i_multiply_overflow_wrapper (int a, int b, int *r);

extern OCTAVE_API int
octave_li_multiply_overflow_wrapper (long int a, long int b, long int *r);

#  if defined (OCTAVE_HAVE_LONG_LONG_INT)
extern OCTAVE_API int
octave_lli_multiply_overflow_wrapper (long long int a, long long int b,
                                      long long int *r);
#  endif

extern OCTAVE_API int
octave_ui_multiply_overflow_wrapper (unsigned int a, unsigned int b,
                                     unsigned int *r);

extern OCTAVE_API int
octave_uli_multiply_overflow_wrapper (unsigned long int a, unsigned long int b,
                                      unsigned long int *r);

#  if defined (OCTAVE_HAVE_UNSIGNED_LONG_LONG_INT)
extern OCTAVE_API int
octave_ulli_multiply_overflow_wrapper (unsigned long long int a,
                                       unsigned long long int b,
                                       unsigned long long int *r);
#  endif

#if defined __cplusplus
}
#endif

#endif
