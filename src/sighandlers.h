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

// This file should always be included after config.h!

#if !defined (octave_sighandlers_h)
#define octave_sighandlers_h 1

#include <Array.h>

#include "syswait.h"

// Signal handler return type.
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#ifndef BADSIG
#define BADSIG (RETSIGTYPE (*)(int))-1
#endif

typedef RETSIGTYPE sig_handler (int);

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
extern int pipe_handler_error_count;

// Nonzero means we can be interrupted.
extern int can_interrupt;

extern sig_handler *octave_set_signal_handler (int, sig_handler *);

extern void install_signal_handlers (void);

extern void catch_interrupts (void);

extern void octave_save_signal_mask (void);

extern void octave_restore_signal_mask (void);

// extern void ignore_sigchld (void);

// This is taken directly from Emacs 19:

#ifndef SYS_SIGLIST_DECLARED
extern char *sys_siglist[];
#endif

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

  ~octave_child_list (void) { }

  static void insert (pid_t pid, octave_child::dead_child_handler f);

  static void remove (pid_t pid);

  static int length (void) { return instance ? instance->curr_len : 0; }

  static octave_child& elem (int i)
    {
      static octave_child foo;

      if (instance)
	{
	  int n = length ();

	  if (i >= 0 && i < n)
	    return instance->list (i);
	}

      return foo;
    }

private:

  Array<octave_child> list;

  int curr_len;

  static octave_child_list *instance;

  void do_insert (pid_t pid, octave_child::dead_child_handler f);

  void do_remove (pid_t pid);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
