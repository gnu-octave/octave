/*

Copyright (C) 2002 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_quit_h)
#define octave_quit_h 1

#ifdef __cplusplus
#include <new>
extern "C" {
#endif

#include <stdio.h>

#include <signal.h>
#include <setjmp.h>

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

#include <windows.h>

extern void w32_sigint_init (void);   /* setup */
extern void w32_raise_final (void);   /* tear down */
extern void w32_raise (int sig);      /* raise signal in main thread */
extern int w32_in_main_thread (void); /* return true if in main thread */

#endif

#if defined (OCTAVE_HAVE_SIG_JUMP)

typedef sigjmp_buf octave_jmp_buf;

#define octave_set_current_context sigsetjmp (current_context, 1)

#else

typedef jmp_buf octave_jmp_buf;

#define octave_set_current_context setjmp (current_context)

#endif

extern octave_jmp_buf current_context;

extern void octave_save_current_context (void *);

extern void octave_restore_current_context (void *);

extern void octave_jump_to_enclosing_context (void) GCC_ATTR_NORETURN;

extern void octave_save_signal_mask (void);

extern void octave_restore_signal_mask (void);

#ifdef __cplusplus
class
octave_interrupt_exception
{
};
#endif

extern sig_atomic_t octave_interrupt_immediately;

/*
  > 0: interrupt pending
    0: no interrupt pending
  < 0: handling interrupt
*/
extern sig_atomic_t octave_interrupt_state;

extern sig_atomic_t octave_allocation_error;

extern sig_atomic_t octave_signal_caught;

extern void octave_handle_signal (void);

extern void octave_throw_interrupt_exception (void) GCC_ATTR_NORETURN;

extern void octave_throw_bad_alloc (void) GCC_ATTR_NORETURN;

#define OCTAVE_QUIT \
  do \
    { \
      if (octave_signal_caught) \
        { \
          octave_signal_caught = 0; \
          octave_handle_signal (); \
        } \
    } \
  while (0)

/* Normally, you just want to use

     BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
     ... some code that calls a "foreign" function ...
     END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

   but sometimes it is useful to do something like

     BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_1;
     ... custom code here, normally ending in a call to
         octave_throw_interrupt_exception ...
     BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_2;

   so that you can perform extra clean up operations before throwing
   the interrupt exception.  */

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE \
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_1; \
  octave_throw_interrupt_exception (); \
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_2

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_1 \
  do \
    { \
      octave_jmp_buf saved_context; \
 \
      octave_save_current_context (saved_context); \
 \
      if (octave_set_current_context) \
	{ \
	  octave_restore_current_context (saved_context)

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_2 \
	} \
      else \
	{ \
	  octave_interrupt_immediately++

#define END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE \
	  octave_interrupt_immediately--; \
          octave_restore_current_context (saved_context); \
        } \
    } \
  while (0)

#ifdef __cplusplus

#define BEGIN_INTERRUPT_WITH_EXCEPTIONS \
  sig_atomic_t saved_octave_interrupt_immediately = octave_interrupt_immediately; \
 \
  try \
    { \
      octave_interrupt_immediately = 0;

#define END_INTERRUPT_WITH_EXCEPTIONS \
    } \
  catch (octave_interrupt_exception) \
    { \
      octave_interrupt_immediately = saved_octave_interrupt_immediately; \
      octave_jump_to_enclosing_context (); \
    } \
  catch (std::bad_alloc) \
    { \
      octave_interrupt_immediately = saved_octave_interrupt_immediately; \
      octave_allocation_error = 1; \
      octave_jump_to_enclosing_context (); \
    } \
 \
  octave_interrupt_immediately = saved_octave_interrupt_immediately
#endif

#ifdef __cplusplus
}

/* These should only be declared for C++ code, and should also be
   outside of any extern "C" block.  */

extern void (*octave_signal_hook) (void);
extern void (*octave_interrupt_hook) (void);
extern void (*octave_bad_alloc_hook) (void);

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
