/*

Copyright (C) 1996 John W. Eaton

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

#include <setjmp.h>

/* Some Fortran compilers append underscores or generate uppercase
   external names. */

#if defined (F77_APPEND_UNDERSCORE)
#if defined (F77_UPPERCASE_NAMES)
#define F77_FCN(f, F) F ## _
#else
#define F77_FCN(f, F) f ## _
#endif
#else
#if defined (F77_UPPERCASE_NAMES)
#define F77_FCN(f, F) F
#else
#define F77_FCN(f, F) f
#endif
#endif

/* How to print an error for the F77_XFCN macro. */

#if defined (F77_UPPERCASE_NAMES)
#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler)
    ("exception encountered in Fortran subroutine %s", F);
#else
#define F77_XFCN_ERROR(f, F) \
  (*current_liboctave_error_handler)
    ("exception encountered in Fortran subroutine %s", f);
#endif

/* This can be used to call a Fortran subroutine that might call
   XSTOPX.  XSTOPX will call lonjmp with f77_context and we'll return,
   call the error function, restore the previous context.  After using
   this macro, error_state should be checked. */

#define F77_XFCN(f, F, args) \
  do \
    { \
      jmp_buf saved_f77_context; \
      copy_f77_context ((char *) f77_context, (char *) saved_f77_context, \
			sizeof (jmp_buf)); \
      if (setjmp (f77_context)) \
	F77_XFCN_ERROR (f, F); \
      else \
	F77_FCN (f, F) args; \
      copy_f77_context ((char *) saved_f77_context, (char *) f77_context, \
			sizeof (jmp_buf)); \
    } \
  while (0)

/* For setjmp/longjmp. */
jmp_buf f77_context;

/* Defining this as a separate function allows us to avoid having to
   include string.h in this file. */

extern void copy_f77_context (void *, void *, unsigned int);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
