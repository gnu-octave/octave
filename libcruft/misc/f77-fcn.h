/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_f77_fcn_h)
#define octave_f77_fcn_h 1

#ifdef __cplusplus
extern "C" {
#endif

#include "quit.h"

#include <stdio.h>

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
      jmp_buf saved_context; \
      f77_exception_encountered = 0; \
      octave_save_current_context ((char *) saved_context); \
      if (octave_set_current_context) \
	{ \
          octave_restore_current_context ((char *) saved_context); \
	  if (f77_exception_encountered) \
	    F77_XFCN_ERROR (f, F); \
          else \
            OCTAVE_THROW_TO_TOP_LEVEL; \
	} \
      else \
        { \
	  octave_interrupt_immediately++; \
	  F77_FUNC (f, F) args; \
	  octave_interrupt_immediately--; \
          octave_restore_current_context ((char *) saved_context); \
        } \
    } \
  while (0)

/* So we can check to see if an exception has occurred. */
extern int f77_exception_encountered;

extern void
F77_FUNC (xstopx, XSTOPX) (const char *s, long int slen) GCC_ATTR_NORETURN;

#if !defined (F77_FCN)
#define F77_FCN(f, F) F77_FUNC (f, F)
#endif

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
