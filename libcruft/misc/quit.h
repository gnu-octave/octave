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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_quit_h)
#define octave_quit_h 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#include <signal.h>
#include <setjmp.h>

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

#if defined (USE_EXCEPTIONS_FOR_INTERRUPTS)

#ifdef __cplusplus
class
octave_interrupt_exception
{
};
#endif

extern sig_atomic_t octave_interrupt_immediately;

extern sig_atomic_t octave_interrupt_state;

extern sig_atomic_t octave_allocation_error;

extern void octave_throw_interrupt_exception (void) GCC_ATTR_NORETURN;

extern void octave_throw_bad_alloc (void) GCC_ATTR_NORETURN;

#define OCTAVE_QUIT \
  do \
    { \
      if (octave_interrupt_state) \
        { \
          octave_interrupt_state = 0; \
          octave_throw_interrupt_exception (); \
        } \
    } \
  while (0)

#define OCTAVE_JUMP_TO_TOP_LEVEL \
  do { octave_interrupt_state = 1; } while (0)

#define OCTAVE_THROW_TO_TOP_LEVEL octave_throw_interrupt_exception ()

#define OCTAVE_THROW_BAD_ALLOC octave_throw_bad_alloc ()

#define OCTAVE_TRY_WITH_INTERRUPTS try

#define OCTAVE_CATCH_INTERRUPTS catch (octave_interrupt_exception)

#define SAVE_OCTAVE_INTERRUPT_IMMEDIATELY(var) \
  sig_atomic_t var = octave_interrupt_immediately

#define INCREMENT_OCTAVE_INTERRUPT_IMMEDIATELY \
  do { octave_interrupt_immediately++; } while (0)

#define DECREMENT_OCTAVE_INTERRUPT_IMMEDIATELY \
  do { octave_interrupt_immediately--; } while (0)

#define SET_OCTAVE_INTERRUPT_IMMEDIATELY(x) \
  do { octave_interrupt_immediately = x; } while (0)

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE \
  do \
    { \
      jmp_buf saved_context; \
 \
      octave_save_current_context ((char *) saved_context); \
 \
      if (octave_set_current_context) \
	{ \
	  octave_restore_current_context ((char *) saved_context); \
	  OCTAVE_THROW_TO_TOP_LEVEL; \
	} \
      else \
	{ \
	  INCREMENT_OCTAVE_INTERRUPT_IMMEDIATELY

#define END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE \
	  DECREMENT_OCTAVE_INTERRUPT_IMMEDIATELY; \
          octave_restore_current_context ((char *) saved_context); \
        } \
    } \
  while (0)

#define BEGIN_INTERRUPT_WITH_EXCEPTIONS \
  SAVE_OCTAVE_INTERRUPT_IMMEDIATELY (saved_octave_interrupt_immediately); \
 \
  OCTAVE_TRY_WITH_INTERRUPTS \
    { \
      SET_OCTAVE_INTERRUPT_IMMEDIATELY (0)

#define END_INTERRUPT_WITH_EXCEPTIONS \
    } \
  OCTAVE_CATCH_INTERRUPTS \
    { \
      SET_OCTAVE_INTERRUPT_IMMEDIATELY (saved_octave_interrupt_immediately); \
      octave_jump_to_enclosing_context (); \
    } \
  catch (bad_alloc) \
    { \
      SET_OCTAVE_INTERRUPT_IMMEDIATELY (saved_octave_interrupt_immediately); \
      octave_allocation_error = 1; \
      octave_jump_to_enclosing_context (); \
    } \
 \
  SET_OCTAVE_INTERRUPT_IMMEDIATELY (saved_octave_interrupt_immediately)

#else

#define OCTAVE_QUIT do { } while (0)

#define OCTAVE_JUMP_TO_TOP_LEVEL octave_jump_to_enclosing_context ()

#define OCTAVE_THROW_TO_TOP_LEVEL OCTAVE_JUMP_TO_TOP_LEVEL

#define OCTAVE_THROW_BAD_ALLOC OCTAVE_JUMP_TO_TOP_LEVEL

#define OCTAVE_TRY_WITH_INTERRUPTS

#define OCTAVE_CATCH_INTERRUPTS if (0)

#define SAVE_OCTAVE_INTERRUPT_IMMEDIATELY(var) do { } while (0)

#define SET_OCTAVE_INTERRUPT_IMMEDIATELY(x) do { } while (0)

#define INCREMENT_OCTAVE_INTERRUPT_IMMEDIATELY do { } while (0)

#define DECREMENT_OCTAVE_INTERRUPT_IMMEDIATELY do { } while (0)

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE do { } while (0)

#define END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE do { } while (0)

#define BEGIN_INTERRUPT_WITH_EXCEPTIONS do { } while (0)

#define END_INTERRUPT_WITH_EXCEPTIONS do { } while (0)

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
