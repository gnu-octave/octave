////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if ! defined (octave_lo_error_h)
#define octave_lo_error_h 1

#include "octave-config.h"

#if defined (__cplusplus)
extern "C" {
#endif

OCTAVE_NORETURN extern OCTAVE_API void
liboctave_fatal (const char *fmt, ...);

OCTAVE_NORETURN extern OCTAVE_API void
liboctave_fatal_with_id (const char *id, const char *fmt, ...);

extern OCTAVE_API void
liboctave_warning (const char *fmt, ...);

extern OCTAVE_API void
liboctave_warning_with_id (const char *id, const char *fmt, ...);

typedef void (*liboctave_error_handler) (const char *, ...);

typedef void (*liboctave_error_with_id_handler) (const char *, const char *, ...);

typedef void (*liboctave_warning_handler) (const char *, ...);

typedef void (*liboctave_warning_with_id_handler) (const char *, const char *, ...);

/* Would be nice to make these pointers private, but we want to share
   them among all the liboctave classes. */
OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN extern OCTAVE_API liboctave_error_handler
  current_liboctave_error_handler;

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN extern OCTAVE_API liboctave_error_with_id_handler
  current_liboctave_error_with_id_handler;

OCTAVE_FORMAT_PRINTF (1, 2)
extern OCTAVE_API liboctave_warning_handler current_liboctave_warning_handler;

OCTAVE_FORMAT_PRINTF (2, 3)
extern OCTAVE_API liboctave_warning_with_id_handler
  current_liboctave_warning_with_id_handler;

extern OCTAVE_API void
set_liboctave_error_handler (OCTAVE_NORETURN liboctave_error_handler f);

extern OCTAVE_API void
set_liboctave_error_with_id_handler (OCTAVE_NORETURN liboctave_error_with_id_handler f);

extern OCTAVE_API void
set_liboctave_warning_handler (liboctave_warning_handler f);

extern OCTAVE_API void
set_liboctave_warning_with_id_handler (liboctave_warning_with_id_handler f);

// To allow the __FILE__ and __LINE__ macros to work as expected, the
// liboctave_panic_impossible, liboctave_panic_if, and
// liboctave_panic_unless symbols must be defined as macros.

#if defined (NDEBUG)

#  define liboctave_panic_impossible() do { } while (0)

#  define liboctave_panic_if(cond) do { } while (0)

#  define liboctave_panic_unless(cond) do { } while (0)

#else

#  define liboctave_panic_impossible() (*current_liboctave_error_handler) ("impossible state reached in file '%s' at line %d", __FILE__, __LINE__)

#  define liboctave_panic_if(cond) do { if (cond) liboctave_panic_impossible (); } while (0)

#  define liboctave_panic_unless(cond) liboctave_panic_if (! (cond))

#endif

#if defined (__cplusplus)
}
#endif

#endif
