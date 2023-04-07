////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (HAVE_DEV_T)
typedef short dev_t;
#endif

#if ! defined (HAVE_INO_T)
typedef unsigned long ino_t;
#endif

#if defined (_MSC_VER)
#  define __WIN32__ 1
#  define WIN32 1
   /* missing parameters in macros */
#  pragma warning (disable: 4003)
   /* missing implementations in template instantiation */
#  pragma warning (disable: 4996)
   /* deprecated function names (FIXME: ???) */
#  pragma warning (disable: 4661)
#endif

#if defined (__APPLE__) && defined (__MACH__)
#  define OCTAVE_USE_OS_X_API 1
#endif

/* Silence deprecated API warning from Apple OS > 10.14 */
#if defined (__APPLE__) && defined (__MACH__) && defined (HAVE_OPENGL)
#  define GL_SILENCE_DEPRECATION 1
#endif

/* Define to 1 if we expect to have <windows.h>, Sleep, etc. */
#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  define OCTAVE_USE_WINDOWS_API 1
#endif

#if defined (OCTAVE_USE_WINDOWS_API)
#  define OCTAVE_HAVE_WINDOWS_FILESYSTEM 1
#elif defined (__CYGWIN__)
#  define OCTAVE_HAVE_WINDOWS_FILESYSTEM 1
#  define OCTAVE_HAVE_POSIX_FILESYSTEM 1
#else
#  define OCTAVE_HAVE_POSIX_FILESYSTEM 1
#endif

#if defined (__MINGW32__)
  /* We need to include this header or __MSVCRT_VERSION__ might not be defined
     to the correct value */
#  include <_mingw.h>
#endif
/* assume that Windows will support UTF-8 locales when using UCRT */
#if defined (__MSVCRT_VERSION__) && __MSVCRT_VERSION__ == 0x0E00
#  define OCTAVE_HAVE_WINDOWS_UTF8_LOCALE 1
#endif

/* sigsetjmp is a macro, not a function. */
#if defined (sigsetjmp) && defined (HAVE_SIGLONGJMP)
#  define OCTAVE_HAVE_SIG_JUMP 1
#endif

/* To be able to use long doubles for 64-bit mixed arithmetics, we need
   them at least 80 bits wide and we need roundl declared in math.h.
   FIXME: Maybe substitute this by a more precise check in the future?  */
#if (SIZEOF_LONG_DOUBLE >= 10) && defined (HAVE_ROUNDL)
#  define OCTAVE_INT_USE_LONG_DOUBLE
#  if (SIZEOF_LONG_DOUBLE < 16                                          \
       && (defined __i386__ || defined __x86_64__) && defined __GNUC__)
#    define OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED 1
#  endif
#endif

#if defined (HAVE_LLVM_LIBCXX)
/* The stream encoding facet from libc++ is stricter than libstdc++ when
   it comes to reverting the stream.  Disable encoding conversion for file
   streams with libc++.
   FIXME: Maybe use a more specific test.  */
#  define OCTAVE_HAVE_STRICT_ENCODING_FACET 1
#endif

/* Make all .oct file interpreter functions and methods static.  */
#define OCTAVE_USE_STATIC_DEFUN

/* This macro is intended to be used only to enable inline functions or
   typedefs that provide access to symbols that have been moved to the
   octave namespace.  It may be temporarily useful to define this macro
   when moving a symbol to the octave namespace but it should not be
   defined when building released versions of Octave, as building those
   should not require deprecated symbols.  It is defined in
   octave-config.h, so users of Octave may continue to access symbols
   using the deprecated names.  */
/* #undef OCTAVE_PROVIDE_DEPRECATED_SYMBOLS */

/* Tag indicating Octave's autoconf-generated config.h has been
   included.  This symbol is provided because autoconf-generated
   config.h files do not define a multiple-inclusion guard.  See also
   the notes at the top of the generated octave-config.h file.  */

#define OCTAVE_AUTOCONFIG_H_INCLUDED 1
