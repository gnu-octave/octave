/*

Copyright (C) 2003-2011 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <signal.h>
#include <string.h>

#include "quit.h"

octave_jmp_buf current_context;

void
octave_save_current_context (void *save_buf)
{
  memcpy (save_buf, current_context, sizeof (octave_jmp_buf));
}

void
octave_restore_current_context (void *save_buf)
{
  memcpy (current_context, save_buf, sizeof (octave_jmp_buf));
}

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

/* FIXME -- eventually remove the debugging */
#if defined (DEBUG)

#define PRINT_CURRENT_THREAD() printf ("%lx: ", GetCurrentThreadId ())

#define DEBUGs(s) \
  do \
    { \
      PRINT_CURRENT_THREAD (); \
      printf (s "\n"); \
      fflush (stdout); \
    } \
  while (0)

#define DEBUGd(s, d) \
  do \
    { \
      PRINT_CURRENT_THREAD (); \
      printf (s "\n", d); \
      fflush (stdout); \
    } \
  while (0)

#else
#define DEBUGs(s)
#define DEBUGd(s, d)
#endif

CRITICAL_SECTION w32_thread_setjmp_mutex;
static CONTEXT w32_signal_context;
static int w32_signal_to_raise = 0;
static DWORD w32_main_thread_id;
static HANDLE w32_main_thread;
static HANDLE w32_restore_thread = NULL;

int
w32_in_main_thread(void)
{
  return (GetCurrentThreadId () == w32_main_thread_id);
}

static DWORD WINAPI
w32_reset_context (LPVOID v)
{
  PCONTEXT context = (PCONTEXT)v;
  int ret;

  /* Mutex the setjmp/longjmp */
  EnterCriticalSection (&w32_thread_setjmp_mutex);

  DEBUGs ("enter w32_set_context");
  SuspendThread (w32_main_thread);
  DEBUGs ("main suspended");
  if (! SetThreadContext (w32_main_thread, context)) 
    {
      fprintf (stderr, "%lx: context failed: ctrl-c won't work\n",
               GetCurrentThreadId ()); 
      fflush (stderr);
    }
  DEBUGs ("context captured (or not)");
  ret = ResumeThread (w32_main_thread);
  DEBUGd ("main resumed with %d", ret);

  LeaveCriticalSection (&w32_thread_setjmp_mutex);
  return 0;
}

static void 
w32_raise_in_main (void)
{
  DWORD threadid;

  DEBUGd ("w32_raise_in_main with signal %d", w32_signal_to_raise);
  raise (w32_signal_to_raise);
  DEBUGd ("w32_raise_in_main signal %d returned a value",
          w32_signal_to_raise);

  DEBUGs ("attempting to restore main to pre-signal configuration");
  if (w32_restore_thread != NULL) /* Catch leaky threads */
    CloseHandle (w32_restore_thread);
  w32_restore_thread = CreateThread (NULL, 10000, w32_reset_context,
                                     &w32_signal_context, 0, &threadid);
  if (w32_restore_thread == NULL) 
    {
      fprintf (stderr, "w32_raise_in_main couldn't create thread\n"); 
      fflush (stderr);
    } 
  else 
    {
      DEBUGs ("waiting to restore raise context");
      WaitForSingleObject (w32_restore_thread, INFINITE);
      fprintf (stderr, "w32_raise_in_main couldn't restore context\n"); 
      fflush (stderr);
    }
}

void
w32_raise_final (void)
{
  CloseHandle (w32_main_thread);
  if (w32_restore_thread != NULL) /* Catch leaky threads */
    CloseHandle (w32_restore_thread);
  w32_main_thread = w32_restore_thread = NULL;
}

/* Raise the given signal in the main thread.  w32_raise_init ()
   must have been called from the main thread already.  */
void
w32_raise (int sig)
{
  int ret;

  if (w32_in_main_thread ()) 
    {
      /* Called from main thread -- a simple raise () should work.  */
      DEBUGd ("raising signal %d within main", signal);
      raise (sig);
      DEBUGd ("returning from signal %d within main", signal);
    } 
  else 
    {
      /* Called from alternate thread -- call w32_raise_in_main in the
         main thread with w32_signal_to_raise set to the signal */
      CONTEXT raise_context;
      DEBUGd ("raising signal %d from separate thread", signal);

      /* Suspend main and remember the context.  */
      SuspendThread (w32_main_thread);
      /* X86 code */
      w32_signal_context.ContextFlags 
        = CONTEXT_FULL|CONTEXT_FLOATING_POINT|CONTEXT_DEBUG_REGISTERS;
      GetThreadContext (w32_main_thread, &w32_signal_context);

      /* Change the context to w32_raise_in_main.  The
         context.Eip=&fn trick for setting the program counter is
         courtesy of

           http://fit.c2.com/files/LispPlatform/lisp/clisp-2.28/src/win32aux.d

         Auxiliary functions for CLISP on Win32, Bruno Haible
         1997-1999.  */

      memcpy (&raise_context, &w32_signal_context, sizeof (CONTEXT));
      raise_context.Eip = (DWORD)&w32_raise_in_main; /* X86 code */
      w32_signal_to_raise = sig;
      SetThreadContext (w32_main_thread, &raise_context);

      /* Resume main at w32_raise_in_main */
      ret = ResumeThread (w32_main_thread);
      DEBUGd ("main resumed at w32_raise_in_main with suspend count %d",
              ret);
    }
}

void
w32_sigint_init (void)
{
  /* Capture main context */
  w32_main_thread_id = GetCurrentThreadId ();
  DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
                   GetCurrentProcess (), &w32_main_thread,
                   0, FALSE, DUPLICATE_SAME_ACCESS);

  InitializeCriticalSectionAndSpinCount (&w32_thread_setjmp_mutex, 0);
}

#endif /* #if defined (__WIN32__) && ! defined (_POSIX_VERSION) */

void
octave_jump_to_enclosing_context (void)
{
#if defined (OCTAVE_HAVE_SIG_JUMP)
  siglongjmp (current_context, 1);
#else
  longjmp (current_context, 1);
#endif
}

/* Allow us to save the signal mask and then restore it to the most
   recently saved value.  This is necessary when using the POSIX
   signal handling interface on some systems calling longjmp out of
   the signal handler to get to the top level on an interrupt doesn't
   restore the original signal mask.  Alternatively, we could use
   sigsetjmp/siglongjmp, but saving and restoring the signal mask
   ourselves works ok and seems simpler just now.  */

static sigset_t octave_signal_mask;

void
octave_save_signal_mask (void)
{
  sigprocmask (0, 0, &octave_signal_mask);
}

void
octave_restore_signal_mask (void)
{
  sigprocmask (SIG_SETMASK, &octave_signal_mask, 0);
}

sig_atomic_t octave_interrupt_immediately = 0;

sig_atomic_t octave_interrupt_state = 0;

sig_atomic_t octave_exception_state = 0;

volatile sig_atomic_t octave_signal_caught = 0;
