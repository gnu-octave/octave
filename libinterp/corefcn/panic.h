////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#if ! defined (octave_panic_h)
#define octave_panic_h 1

#include "octave-config.h"

#include <cstdarg>

OCTAVE_NORETURN
extern OCTINTERP_API void vpanic (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN
extern OCTINTERP_API void panic (const char *fmt, ...);

// To allow the __FILE__ and __LINE__ macros to work as expected, the
// panic_impossible, panic_if, panic_unless, error_impossible, error_if,
// and error_unless symbols must be defined as macros.

#define panic_impossible()                                              \
  ::panic ("impossible state reached in file '%s' at line %d", __FILE__, __LINE__)

#if defined (NDEBUG)
#  define panic_if(cond)
#else
#  define panic_if(cond) do { if (cond) panic_impossible (); } while (0)
#endif

#if defined (NDEBUG)
#  define panic_unless(cond)
#else
#  define panic_unless(cond) panic_if (! (cond))
#endif

#endif
