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

#include <setjmp.h>

/* hack to stringize macro results */
#define xSTRINGIZE(x) #x
#define STRINGIZE(x) xSTRINGIZE(x)

/* How to print an error for the F77_XFCN macro. */

#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler) \
    ("exception encountered in Fortran subroutine %s", \
     STRINGIZE (F77_FUNC (f, F)))

/* This can be used to call a Fortran subroutine that might call
   XSTOPX.  XSTOPX will call lonjmp with f77_context and we'll return,
   call the error function, restore the previous context.  After using
   this macro, error_state should be checked. */

#define F77_XFCN(f, F, args) \
  do \
    { \
      jmp_buf saved_f77_context; \
      f77_exception_encountered = 0; \
      copy_f77_context ((char *) f77_context, (char *) saved_f77_context, \
			sizeof (jmp_buf)); \
      if (setjmp (f77_context)) \
	{ \
	  f77_exception_encountered = 1; \
	  F77_XFCN_ERROR (f, F); \
	} \
      else \
	F77_FUNC (f, F) args; \
      copy_f77_context ((char *) saved_f77_context, (char *) f77_context, \
			sizeof (jmp_buf)); \
    } \
  while (0)

/* So we can check to see if an exception has occurred. */
extern int f77_exception_encountered;

/* For setjmp/longjmp. */
extern jmp_buf f77_context;

/* Defining this as a separate function allows us to avoid having to
   include string.h in this file. */

extern void copy_f77_context (void *, void *, unsigned int);

#ifdef __cplusplus
}
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
