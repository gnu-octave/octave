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

/*

The signal blocking macros defined below were adapted from similar
functions from GNU Bash, the Bourne Again SHell, copyright (C) 1994
Free Software Foundation, Inc.

*/

// This file should always be included after config.h!

#if !defined (octave_sighandlers_h)
#define octave_sighandlers_h 1

// Include signal.h, not csignal since the latter might only define
// the ANSI standard C signal interface.

#include <signal.h>

#include "syswait.h"
#include "siglist.h"

#include <Array.h>

// Signal handler return type.
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#ifndef BADSIG
#define BADSIG (RETSIGTYPE (*)(int))-1
#endif

#define BLOCK_SIGNAL(sig, nvar, ovar) \
  do \
    { \
      sigemptyset (&nvar); \
      sigaddset (&nvar, sig); \
      sigemptyset (&ovar); \
      sigprocmask (SIG_BLOCK, &nvar, &ovar); \
    } \
  while (0)

#if defined (HAVE_POSIX_SIGNALS)
#define BLOCK_CHILD(nvar, ovar) BLOCK_SIGNAL (SIGCHLD, nvar, ovar)
#define UNBLOCK_CHILD(ovar) sigprocmask (SIG_SETMASK, &ovar, 0)
#else
#define BLOCK_CHILD(nvar, ovar) ovar = sigblock (sigmask (SIGCHLD))
#define UNBLOCK_CHILD(ovar) sigsetmask (ovar)
#endif

typedef RETSIGTYPE sig_handler (int);

// XXX FIXME XXX -- the data should probably be private...

struct
octave_interrupt_handler
{
#ifdef SIGINT
  sig_handler *int_handler;
#endif

#ifdef SIGBREAK
  sig_handler *brk_handler;
#endif
};

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
extern int pipe_handler_error_count;

// TRUE means we can be interrupted.
extern bool can_interrupt;

extern sig_handler *octave_set_signal_handler (int, sig_handler *);

extern void install_signal_handlers (void);

extern octave_interrupt_handler octave_catch_interrupts (void);

extern octave_interrupt_handler octave_ignore_interrupts (void);

extern octave_interrupt_handler
octave_set_interrupt_handler (const volatile octave_interrupt_handler&);

// extern void ignore_sigchld (void);

// Maybe this should be in a separate file?

class
octave_child
{
public:

  typedef void (*dead_child_handler) (pid_t, int);

  octave_child (pid_t id = -1, dead_child_handler f = 0)
    : pid (id), handler (f) { }

  octave_child (const octave_child& oc)
    : pid (oc.pid), handler (oc.handler) { }

  octave_child& operator = (const octave_child& oc)
    {
      if (&oc != this)
	{
	  pid = oc.pid;
	  handler = oc.handler;
	}
      return *this;
    }

  ~octave_child (void) { }

  // The process id of this child.
  pid_t pid;

  // The function we call if this child dies.
  dead_child_handler handler;
};

class
octave_child_list
{
protected:

  octave_child_list (void) : list (0), curr_len (0) { }

public:

  ~octave_child_list (void);

  static bool instance_ok (void);

  static void insert (pid_t pid, octave_child::dead_child_handler f);

  static void remove (pid_t pid);

  static int length (void);

  static octave_child& elem (int i);

private:

  Array<octave_child> list;

  int curr_len;

  static octave_child_list *instance;

  void do_insert (pid_t pid, octave_child::dead_child_handler f);

  void do_remove (pid_t pid);

  int do_length (void) const;

  octave_child& do_elem (int i);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
