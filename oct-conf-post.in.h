/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

/* The C++ standard is evolving to allow attribute hints in a
   compiler-independent manner.  In C++ 2011 support for noreturn was
   added.  In C++ 2014 support for deprecated was added.  The Octave
   code base has been future-proofed by using macros of the form
   OCTAVE_ATTRIBUTE_NAME in place of vendor specific attribute
   mechanisms.  As compilers evolve, the underlying implementation can
   be changed with the macro definitions below.  FIXME: Update macros
   to use C++ standard attribute syntax when Octave moves to C++ 2011
   standard.  */

#if defined (__GNUC__)
   /* The following attributes are used with gcc and clang compilers.  */
#  if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
#    define OCTAVE_DEPRECATED(ver, msg) __attribute__ ((__deprecated__ ("[" #ver "]: " msg)))
#  else
#    define OCTAVE_DEPRECATED(ver, msg) __attribute__ ((__deprecated__))
#  endif
#  define HAVE_OCTAVE_DEPRECATED_ATTR 1

#  define OCTAVE_NORETURN __attribute__ ((__noreturn__))
#  define HAVE_OCTAVE_NORETURN_ATTR 1

#  define OCTAVE_UNUSED __attribute__ ((__unused__))
#  define HAVE_OCTAVE_UNUSED_ATTR 1
#else
#  define OCTAVE_DEPRECATED(ver, msg)
#  define OCTAVE_NORETURN
#  define OCTAVE_UNUSED
#endif

#if defined (__MINGW32__)
  /* MinGW requires special handling due to different format specifiers
   * on different platforms.  The macro __MINGW_PRINTF_FORMAT maps to
   * either gnu_printf or ms_printf depending on where we are compiling
   * to avoid warnings on format specifiers that are legal.
   * See: https://bugzilla.mozilla.org/show_bug.cgi?id=1331349  */
#  define OCTAVE_FORMAT_PRINTF(stringIndex, firstToCheck) \
     __attribute__ ((format (__MINGW_PRINTF_FORMAT, stringIndex, firstToCheck)))

#  define HAVE_OCTAVE_FORMAT_PRINTF_ATTR 1
#elif defined (__GNUC__)
   /* The following attributes are used with gcc and clang compilers.  */
#  define OCTAVE_FORMAT_PRINTF(index, first) \
     __attribute__ ((__format__(printf, index, first)))

#  define HAVE_OCTAVE_FORMAT_PRINTF_ATTR 1
#else
#  define OCTAVE_FORMAT_PRINTF(index, first)

/* #  undef HAVE_OCTAVE_FORMAT_PRINTF_ATTR */
#endif

#if ! defined (OCTAVE_FALLTHROUGH)
#  if defined (__cplusplus) && __cplusplus > 201402L
#    define OCTAVE_FALLTHROUGH [[fallthrough]]
#  elif defined (__GNUC__) && __GNUC__ < 7
#    define OCTAVE_FALLTHROUGH ((void) 0)
#  else
#    define OCTAVE_FALLTHROUGH __attribute__ ((__fallthrough__))
#  endif
#endif

/* This macro could have a better name...  It is intended to be used
   only to enable inline functions or typedefs that provide access to
   symbols that have been moved to the octave namespace.  It may be
   temporarily useful to define this macro when moving a symbol to the
   octave namespace but it should not be defined when building
   released versions of Octave, as building those should not require
   deprecated symbols.  It is defined in octave-config.h, so users of
   Octave may continue to access symbols using the deprecated names.  */
/* #undef OCTAVE_USE_DEPRECATED_FUNCTIONS */

#if defined (__cplusplus)
template <typename T>
static inline void
octave_unused_parameter (const T&)
{ }
#else
#  define octave_unused_parameter(param) (void) param;
#endif

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

/* oct-dlldefs.h */

/* FIXME: GCC supports visibility attributes as well, even using the
   same __declspec declaration if desired.  The build system should be
   extended to support GCC and visibility attributes.  */
#if defined (_MSC_VER)
#  define OCTAVE_EXPORT __declspec(dllexport)
#  define OCTAVE_IMPORT __declspec(dllimport)
#else
   /* All other compilers, at least for now. */
#  define OCTAVE_EXPORT
#  define OCTAVE_IMPORT
#endif

/* API macro for liboctave */
#if defined (OCTAVE_DLL)
#  define OCTAVE_API OCTAVE_EXPORT
#else
#  define OCTAVE_API OCTAVE_IMPORT
#endif

/* API macro for libinterp */
#if defined (OCTINTERP_DLL)
#  define OCTINTERP_API OCTAVE_EXPORT
#else
#  define OCTINTERP_API OCTAVE_IMPORT
#endif

/* API macro for libinterp/graphics */
#if defined (OCTGRAPHICS_DLL)
#  define OCTGRAPHICS_API OCTAVE_EXPORT
#else
#  define OCTGRAPHICS_API OCTAVE_IMPORT
#endif

/* API macro for libgui */
#if defined (OCTGUI_DLL)
#  define OCTGUI_API OCTAVE_EXPORT
#else
#  define OCTGUI_API OCTAVE_IMPORT
#endif

/* Backward compatibility */

#if defined (OCTAVE_ENABLE_ATOMIC_REFCOUNT)
#  define USE_ATOMIC_REFCOUNT 1
#endif

#if defined (OCTAVE_ENABLE_64)
#  define USE_64_BIT_IDX_T 1
#endif

#if defined (OCTAVE_ENABLE_OPENMP)
#  define HAVE_OPENMP 1
#endif

#if defined (OCTAVE_ENABLE_FLOAT_TRUNCATE)
#  define OCTAVE_FLOAT_TRUNCATE volatile
#else
#  define OCTAVE_FLOAT_TRUNCATE
#endif

#if defined (__cplusplus)
#  include <cstdint>
#else
#  include <stdint.h>
#endif

typedef OCTAVE_IDX_TYPE octave_idx_type;

#if defined (OCTAVE_ENABLE_64)
#  define OCTAVE_IDX_TYPE_FORMAT PRId64
#else
#  define OCTAVE_IDX_TYPE_FORMAT PRId32
#endif

typedef OCTAVE_F77_INT_TYPE octave_f77_int_type;

#define OCTAVE_HAVE_F77_INT_TYPE 1

#if defined (__cplusplus) && ! defined (OCTAVE_THREAD_LOCAL)
#  define OCTAVE_THREAD_LOCAL
#endif

/* Tag indicating Octave's autoconf-generated config.h has been
   included.  This symbol is provided because autoconf-generated
   config.h files do not define a multiple-inclusion guard.  See also
   the notes at the top of the generated octave-config.h file.  */

#define OCTAVE_AUTOCONFIG_H_INCLUDED 1
