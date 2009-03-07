/*

Copyright (C) 1996, 1997, 2002, 2003, 2004, 2005, 2006, 2007, 2008
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_f77_fcn_h)
#define octave_f77_fcn_h 1

#include "quit.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Hack to stringize macro results. */
#define xSTRINGIZE(x) #x
#define STRINGIZE(x) xSTRINGIZE(x)

/* How to print an error for the F77_XFCN macro. */

#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler) \
    ("exception encountered in Fortran subroutine %s", \
     STRINGIZE (F77_FUNC (f, F)))

/* This can be used to call a Fortran subroutine that might call
   XSTOPX.  XSTOPX will call lonjmp with current_context.  Once back
   here, we'll restore the previous context and return.  We may also
   end up here if an interrupt is processed when the Fortran
   subroutine is called.  In that case, we resotre the context and go
   to the top level.  The error_state should be checked immediately
   after this macro is used. */

#define F77_XFCN(f, F, args) \
  do \
    { \
      octave_jmp_buf saved_context; \
      sig_atomic_t saved_octave_interrupt_immediately = octave_interrupt_immediately; \
      f77_exception_encountered = 0; \
      octave_save_current_context (saved_context); \
      if (octave_set_current_context) \
	{ \
	  octave_interrupt_immediately = saved_octave_interrupt_immediately; \
          octave_restore_current_context (saved_context); \
	  if (f77_exception_encountered) \
	    F77_XFCN_ERROR (f, F); \
          else \
	    octave_rethrow_exception (); \
	} \
      else \
        { \
	  octave_interrupt_immediately++; \
	  F77_FUNC (f, F) args; \
	  octave_interrupt_immediately--; \
          octave_restore_current_context (saved_context); \
        } \
    } \
  while (0)

/* So we can check to see if an exception has occurred. */
CRUFT_API extern int f77_exception_encountered;

#if !defined (F77_FCN)
#define F77_FCN(f, F) F77_FUNC (f, F)
#endif

#if defined (F77_USES_CRAY_CALLING_CONVENTION)

#include <fortran.h>

/* Use these macros to pass character strings from C to Fortran.  */
#define F77_CHAR_ARG(x) octave_make_cray_ftn_ch_dsc (x, strlen (x))
#define F77_CONST_CHAR_ARG(x) \
  octave_make_cray_const_ftn_ch_dsc (x, strlen (x))
#define F77_CHAR_ARG2(x, l) octave_make_cray_ftn_ch_dsc (x, l)
#define F77_CONST_CHAR_ARG2(x, l) octave_make_cray_const_ftn_ch_dsc (x, l)
#define F77_CXX_STRING_ARG(x) \
  octave_make_cray_const_ftn_ch_dsc (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l)
#define F77_CHAR_ARG_DECL octave_cray_ftn_ch_dsc
#define F77_CONST_CHAR_ARG_DECL octave_cray_ftn_ch_dsc
#define F77_CHAR_ARG_LEN_DECL

/* Use these macros to write C-language functions that accept
   Fortran-style character strings.  */
#define F77_CHAR_ARG_DEF(s, len) octave_cray_ftn_ch_dsc s
#define F77_CONST_CHAR_ARG_DEF(s, len) octave_cray_ftn_ch_dsc s
#define F77_CHAR_ARG_LEN_DEF(len) 
#define F77_CHAR_ARG_USE(s) s.ptr
#define F77_CHAR_ARG_LEN_USE(s, len) (s.mask.len>>3)

#define F77_RET_T int
#define F77_RETURN(retval) return retval;

/* FIXME -- these should work for SV1 or Y-MP systems but will
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

#ifdef __cplusplus
#define OCTAVE_F77_FCN_INLINE inline
#else
#define OCTAVE_F77_FCN_INLINE
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

#ifdef __cplusplus
#undef OCTAVE_F77_FCN_INLINE
#endif

#elif defined (F77_USES_VISUAL_FORTRAN_CALLING_CONVENTION)

/* Use these macros to pass character strings from C to Fortran.  */
#define F77_CHAR_ARG(x) x, strlen (x)
#define F77_CONST_CHAR_ARG(x) F77_CHAR_ARG (x)
#define F77_CHAR_ARG2(x, l) x, l
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)
#define F77_CXX_STRING_ARG(x) F77_CONST_CHAR_ARG2 (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l)
#define F77_CHAR_ARG_DECL char *, int
#define F77_CONST_CHAR_ARG_DECL const char *, int
#define F77_CHAR_ARG_LEN_DECL

/* Use these macros to write C-language functions that accept
   Fortran-style character strings.  */
#define F77_CHAR_ARG_DEF(s, len) char *s, int len
#define F77_CONST_CHAR_ARG_DEF(s, len) const char *s, int len
#define F77_CHAR_ARG_LEN_DEF(len) 
#define F77_CHAR_ARG_USE(s) s
#define F77_CHAR_ARG_LEN_USE(s, len) len

#define F77_RET_T void
#define F77_RETURN(retval)

#else

/* Assume f2c-compatible calling convention.  */

/* Use these macros to pass character strings from C to Fortran.  */
#define F77_CHAR_ARG(x) x
#define F77_CONST_CHAR_ARG(x) F77_CHAR_ARG (x)
#define F77_CHAR_ARG2(x, l) x
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)
#define F77_CXX_STRING_ARG(x) F77_CONST_CHAR_ARG2 (x.c_str (), x.length ())
#define F77_CHAR_ARG_LEN(l) , l
#define F77_CHAR_ARG_DECL char *
#define F77_CONST_CHAR_ARG_DECL const char *
#define F77_CHAR_ARG_LEN_DECL , long

/* Use these macros to write C-language functions that accept
   Fortran-style character strings.  */
#define F77_CHAR_ARG_DEF(s, len) char *s
#define F77_CONST_CHAR_ARG_DEF(s, len) const char *s
#define F77_CHAR_ARG_LEN_DEF(len) , long len
#define F77_CHAR_ARG_USE(s) s
#define F77_CHAR_ARG_LEN_USE(s, len) len

#define F77_RET_T int
#define F77_RETURN(retval) return retval;

#endif


/* Build a C string local variable CS from the Fortran string parameter S
   declared as F77_CHAR_ARG_DEF(s, len) or F77_CONST_CHAR_ARG_DEF(s, len).
   The string will be cleaned up at the end of the current block.  
   Needs to include <cstring> and <vector>.  */

#define F77_CSTRING(s, len, cs) \
 OCTAVE_LOCAL_BUFFER (char, cs, F77_CHAR_ARG_LEN_USE (s, len) + 1); \
 memcpy (cs, F77_CHAR_ARG_USE (s), F77_CHAR_ARG_LEN_USE (s, len)); \
 cs[F77_CHAR_ARG_LEN_USE(s, len)] = '\0' 


extern CRUFT_API F77_RET_T
F77_FUNC (xstopx, XSTOPX) (F77_CONST_CHAR_ARG_DECL
			   F77_CHAR_ARG_LEN_DECL) GCC_ATTR_NORETURN;

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
