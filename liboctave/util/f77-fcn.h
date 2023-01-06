////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_f77_fcn_h)
#define octave_f77_fcn_h 1

#include "octave-config.h"

#include "lo-error.h"
#include "quit.h"

#if defined (__cplusplus)
#  include <cstddef>
#  include <limits>
using std::size_t;
extern "C" {
#else
#  include <stddef.h>
#endif

/* This macro is obsolete.  */

#define F77_XFCN(f, F, args)                    \
  F77_FUNC (f, F) args

#if ! defined (F77_FCN)
#  define F77_FCN(f, F) F77_FUNC (f, F)
#endif

/*

The following macros are used for handling Fortran <-> C calling
conventions.  They are defined below for three different types of
systems, Cray (possibly now obsolete), Visual Fortran, and gfortran.
Note that we don't attempt to handle Fortran functions, we always use
subroutine wrappers for them and pass the return value as an extra
argument.

Use these macros to pass character strings from C to Fortran:

  F77_CHAR_ARG(x)
  F77_CONST_CHAR_ARG(x)
  F77_CXX_STRING_ARG(x)
  F77_CHAR_ARG_LEN(l)
  F77_CHAR_ARG_DECL
  F77_CONST_CHAR_ARG_DECL
  F77_CHAR_ARG_LEN_DECL

Use these macros to write C-language functions that accept
Fortran-style character strings:

  F77_CHAR_ARG_DEF(s, len)
  F77_CONST_CHAR_ARG_DEF(s, len)
  F77_CHAR_ARG_LEN_DEF(len)
  F77_CHAR_ARG_USE(s)
  F77_CHAR_ARG_LEN_USE(s, len)

Use these macros for C++ code

  F77_INT         Equivalent to Fortran INTEGER type
  F77_INT4        Equivalent to Fortran INTEGER*4 type
  F77_DBLE        Equivalent to Fortran DOUBLE PRECISION type
  F77_REAL        Equivalent to Fortran REAL type
  F77_CMPLX       Equivalent to Fortran COMPLEX type
  F77_DBLE_CMPLX  Equivalent to Fortran DOUBLE COMPLEX type
  F77_LOGICAL     Equivalent to Fortran LOGICAL type
  F77_RET_T       Return type of a C++ function that acts like a
                  Fortran subroutine.

Use these macros to return from C-language functions that are supposed
to act like Fortran subroutines.  F77_NORETURN is intended to be used
as the last statement of such a function that has been tagged with a
"noreturn" attribute.  If the compiler supports the "noreturn"
attribute or if F77_RET_T is void, then it should expand to nothing so
that we avoid warnings about functions tagged as "noreturn"
containing a return statement.  Otherwise, it should expand to a
statement that returns the given value so that we avoid warnings about
not returning a value from a function declared to return something.

  F77_RETURN(retval)
  F77_NORETURN(retval)

*/

#if defined (F77_USES_CRAY_CALLING_CONVENTION)

#include <fortran.h>

/* Use these macros to pass character strings from C to Fortran.  Cray
   Fortran uses a descriptor structure to pass a pointer to the string
   and the length in a single argument.  */

#define F77_CHAR_ARG(x) octave_make_cray_ftn_ch_dsc (x, strlen (x))
#define F77_CONST_CHAR_ARG(x)                           \
  octave_make_cray_const_ftn_ch_dsc (x, strlen (x))
#define F77_CHAR_ARG2(x, l) octave_make_cray_ftn_ch_dsc (x, l)
#define F77_CONST_CHAR_ARG2(x, l) octave_make_cray_const_ftn_ch_dsc (x, l)
#define F77_CXX_STRING_ARG(x)                                   \
  octave_make_cray_const_ftn_ch_dsc (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l)
#define F77_CHAR_ARG_LEN_TYPE
#define F77_CHAR_ARG_LEN_DECL
#define F77_CHAR_ARG_DECL octave_cray_ftn_ch_dsc
#define F77_CONST_CHAR_ARG_DECL octave_cray_ftn_ch_dsc

/* Use these macros to write C-language functions that accept
   Fortran-style character strings.  */
#define F77_CHAR_ARG_DEF(s, len) octave_cray_ftn_ch_dsc s
#define F77_CONST_CHAR_ARG_DEF(s, len) octave_cray_ftn_ch_dsc s
#define F77_CHAR_ARG_LEN_DEF(len)
#define F77_CHAR_ARG_USE(s) s.ptr
#define F77_CHAR_ARG_LEN_USE(s, len) (s.mask.len >> 3)

#define F77_RET_T int

/* Use these macros to return from C-language functions that are
   supposed to act like Fortran subroutines.  F77_NORETURN is intended
   to be used as the last statement of such a function that has been
   tagged with a "noreturn" attribute.  */

#define F77_RETURN(retval) return retval;
#if defined (HAVE_OCTAVE_NORETURN_ATTR)
#  define F77_NORETURN(retval)
#else
#  define F77_NORETURN(retval) return retval;
#endif

/* FIXME: These should work for SV1 or Y-MP systems but will
          need to be changed for others.  */

typedef union
  {
  const char *const_ptr;
  char *ptr;
  struct
  {
    unsigned off : 6;
    unsigned len : 26;
    unsigned add : 32;
  } mask;
} octave_cray_descriptor;

typedef void *octave_cray_ftn_ch_dsc;

#if defined (__cplusplus)
#  define OCTAVE_F77_FCN_INLINE inline
#else
#  define OCTAVE_F77_FCN_INLINE
#endif

static OCTAVE_F77_FCN_INLINE octave_cray_ftn_ch_dsc
octave_make_cray_ftn_ch_dsc (char *ptr_arg, unsigned long len_arg)
{
  octave_cray_descriptor desc;
  desc.ptr = ptr_arg;
  desc.mask.len = len_arg << 3;
  return *((octave_cray_ftn_ch_dsc *) &desc);
}

static OCTAVE_F77_FCN_INLINE octave_cray_ftn_ch_dsc
octave_make_cray_const_ftn_ch_dsc (const char *ptr_arg, unsigned long len_arg)
{
  octave_cray_descriptor desc;
  desc.const_ptr = ptr_arg;
  desc.mask.len = len_arg << 3;
  return *((octave_cray_ftn_ch_dsc *) &desc);
}

#undef OCTAVE_F77_FCN_INLINE

#elif defined (F77_USES_VISUAL_FORTRAN_CALLING_CONVENTION)

/* Use these macros to pass character strings from C to Fortran.
   Visual Fortran inserts the length after each character string
   argument.  */

#define F77_CHAR_ARG(x) x, strlen (x)
#define F77_CONST_CHAR_ARG(x) F77_CHAR_ARG (x)
#define F77_CHAR_ARG2(x, l) x, l
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)
#define F77_CXX_STRING_ARG(x) F77_CONST_CHAR_ARG2 (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l)
#define F77_CHAR_ARG_LEN_TYPE int
#define F77_CHAR_ARG_LEN_DECL
#define F77_CHAR_ARG_DECL char *, F77_CHAR_ARG_LEN_TYPE
#define F77_CONST_CHAR_ARG_DECL const char *, F77_CHAR_ARG_LEN_TYPE

#define F77_CHAR_ARG_DEF(s, len) char *s, F77_CHAR_ARG_LEN_TYPE len
#define F77_CONST_CHAR_ARG_DEF(s, len) const char *s, F77_CHAR_ARG_LEN_TYPE len
#define F77_CHAR_ARG_LEN_DEF(len)
#define F77_CHAR_ARG_USE(s) s
#define F77_CHAR_ARG_LEN_USE(s, len) len

#define F77_RET_T void

#define F77_RETURN(retval) return;
#define F77_NORETURN(retval)

#elif defined (F77_USES_GFORTRAN_CALLING_CONVENTION)

/* Use these macros to pass character strings from C to Fortran.
   gfortran appends length arguments for assumed size character
   strings to the and ignores others.

   FIXME: I don't think we correctly handle the case of mixing some
   fixed-length and some assumed-length character string arguments as
   we don't handle each case separately, so it seems there could be
   mismatch?  However, I don't think we currently have to handle this
   case in Octave.  */

#define F77_CHAR_ARG(x) x
#define F77_CONST_CHAR_ARG(x) F77_CHAR_ARG (x)
#define F77_CHAR_ARG2(x, l) x
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)
#define F77_CXX_STRING_ARG(x) F77_CONST_CHAR_ARG2 (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l) , l
#if defined (__GNUC__) && __GNUC__ >= 8
#  define F77_CHAR_ARG_LEN_TYPE size_t
#else
#  define F77_CHAR_ARG_LEN_TYPE int
#endif
#define F77_CHAR_ARG_LEN_DECL , F77_CHAR_ARG_LEN_TYPE
#define F77_CHAR_ARG_DECL char *
#define F77_CONST_CHAR_ARG_DECL const char *

#define F77_CHAR_ARG_DEF(s, len) char *s
#define F77_CONST_CHAR_ARG_DEF(s, len) const char *s
#define F77_CHAR_ARG_LEN_DEF(len) , F77_CHAR_ARG_LEN_TYPE len
#define F77_CHAR_ARG_USE(s) s
#define F77_CHAR_ARG_LEN_USE(s, len) len

#define F77_RET_T void

#define F77_RETURN(retval) return;
#if defined (HAVE_OCTAVE_NORETURN_ATTR)
#  define F77_NORETURN(retval)
#else
#  define F77_NORETURN(retval) return retval;
#endif

#elif defined (F77_USES_F2C_CALLING_CONVENTION)

/* Assume f2c-compatible calling convention.  */

/* Use these macros to pass character strings from C to Fortran.  f2c
   appends all length arguments at the end of the parameter list.  */

#define F77_CHAR_ARG(x) x
#define F77_CONST_CHAR_ARG(x) F77_CHAR_ARG (x)
#define F77_CHAR_ARG2(x, l) x
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)
#define F77_CXX_STRING_ARG(x) F77_CONST_CHAR_ARG2 (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l) , l
#define F77_CHAR_ARG_LEN_TYPE long
#define F77_CHAR_ARG_LEN_DECL , F77_CHAR_ARG_LEN_TYPE
#define F77_CHAR_ARG_DECL char *
#define F77_CONST_CHAR_ARG_DECL const char *

#define F77_CHAR_ARG_DEF(s, len) char *s
#define F77_CONST_CHAR_ARG_DEF(s, len) const char *s
#define F77_CHAR_ARG_LEN_DEF(len) , F77_CHAR_ARG_LEN_TYPE len
#define F77_CHAR_ARG_USE(s) s
#define F77_CHAR_ARG_LEN_USE(s, len) len

#define F77_RET_T int

#define F77_RETURN(retval) return retval;
#if defined (HAVE_OCTAVE_NORETURN_ATTR)
#  define F77_NORETURN(retval)
#else
#  define F77_NORETURN(retval) return retval;
#endif

#else

#error "unknown C++ to Fortran calling convention"

#endif

typedef double F77_DBLE;
typedef float F77_REAL;
typedef double _Complex F77_DBLE_CMPLX;
typedef float _Complex F77_CMPLX;
typedef octave_f77_int_type F77_INT;
typedef int32_t F77_INT4;
typedef octave_f77_int_type F77_LOGICAL;

#define F77_CMPLX_ARG(x)                        \
  reinterpret_cast<float _Complex *> (x)

#define F77_CONST_CMPLX_ARG(x)                  \
  reinterpret_cast<const float _Complex *> (x)

#define F77_DBLE_CMPLX_ARG(x)                   \
  reinterpret_cast<double _Complex *> (x)

#define F77_CONST_DBLE_CMPLX_ARG(x)             \
  reinterpret_cast<const double _Complex *> (x)

/* Build a C string local variable CS from the Fortran string parameter S
   declared as F77_CHAR_ARG_DEF(s, len) or F77_CONST_CHAR_ARG_DEF(s, len).
   The string will be cleaned up at the end of the current block.
   Needs to include <cstring> and <vector>.  */

#define F77_CSTRING(s, len, cs)                                         \
  OCTAVE_LOCAL_BUFFER (char, cs, F77_CHAR_ARG_LEN_USE (s, len) + 1);    \
  memcpy (cs, F77_CHAR_ARG_USE (s), F77_CHAR_ARG_LEN_USE (s, len));     \
  cs[F77_CHAR_ARG_LEN_USE(s, len)] = '\0'

OCTAVE_NORETURN OCTAVE_API extern
F77_RET_T
F77_FUNC (xstopx, XSTOPX) (F77_CONST_CHAR_ARG_DECL
                           F77_CHAR_ARG_LEN_DECL);

#if defined (__cplusplus)

OCTAVE_BEGIN_NAMESPACE(octave)

inline F77_INT
to_f77_int (octave_idx_type x)
{
  if (x < std::numeric_limits<F77_INT>::min ()
      || x > std::numeric_limits<F77_INT>::max ())
    (*current_liboctave_error_handler)
      ("integer dimension or index out of range for Fortran INTEGER type");

  return static_cast<F77_INT> (x);
}

OCTAVE_END_NAMESPACE(octave)

#endif

#if defined (__cplusplus)
}
#endif

#endif
