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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "quit.h"

#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-except.h"
#include "pt-exp.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "variables.h"

// Simple exception handling.

tree_try_catch_command::~tree_try_catch_command (void)
{
  delete try_code;
  delete catch_code;
  delete lead_comm;
  delete mid_comm;
  delete trail_comm;
}

static void
do_catch_code (void *ptr)
{
  // Is it safe to call OCTAVE_QUIT here?  We are already running
  // something on the unwind_protect stack, but the element for this
  // action would have already been popped from the top of the stack,
  // so we should not be attempting to run it again.

  OCTAVE_QUIT;

  // If we are interrupting immediately, or if an interrupt is in
  // progress (octave_interrupt_state < 0), then we don't want to run
  // the catch code (it should only run on errors, not interrupts).

  // If octave_interrupt_state is positive, an interrupt is pending.
  // The only way that could happen would be for the interrupt to
  // come in after the OCTAVE_QUIT above and before the if statement
  // below -- it's possible, but unlikely.  In any case, we should
  // probably let the catch code throw the exception because we don't
  // want to skip that and potentially run some other code.  For
  // example, an error may have originally brought us here for some
  // cleanup operation and we shouldn't skip that.

  if (octave_interrupt_immediately || octave_interrupt_state < 0)
    return;

  tree_statement_list *list = static_cast<tree_statement_list *> (ptr);

  // Set up for letting the user print any messages from errors that
  // occurred in the body of the try_catch statement.

  buffer_error_messages--;

  if (list)
    list->eval ();
}

void
tree_try_catch_command::eval (void)
{
  unwind_protect::begin_frame ("tree_try_catch::eval");
  
  MAYBE_DO_BREAKPOINT;

  if (catch_code)
    {
      unwind_protect_int (buffer_error_messages);
      buffer_error_messages++;
    }

  unwind_protect::add (do_catch_code, catch_code);

  if (try_code)
    try_code->eval ();

  if (catch_code && error_state)
    {
      error_state = 0;
      unwind_protect::run_frame ("tree_try_catch::eval");
    }
  else
    {
      error_state = 0;

      // Unwind stack elements must be cleared or run in the reverse
      // order in which they were added to the stack.

      // For clearing the do_catch_code cleanup function.
      unwind_protect::discard ();

      // For restoring buffer_error_messages.
      if (catch_code)
	unwind_protect::run ();

      // Also clear the frame marker.
      unwind_protect::discard ();
    }
}

void
tree_try_catch_command::accept (tree_walker& tw)
{
  tw.visit_try_catch_command (*this);
}

// Simple exception handling.

tree_unwind_protect_command::~tree_unwind_protect_command (void)
{
  delete unwind_protect_code;
  delete cleanup_code;
  delete lead_comm;
  delete mid_comm;
  delete trail_comm;
}

static void
do_unwind_protect_cleanup_code (void *ptr)
{
  tree_statement_list *list = static_cast<tree_statement_list *> (ptr);

  // We want to run the cleanup code without error_state being set,
  // but we need to restore its value, so that any errors encountered
  // in the first part of the unwind_protect are not completely
  // ignored.

  unwind_protect_int (error_state);
  error_state = 0;

  // Similarly, if we have seen a return or break statement, allow all
  // the cleanup code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  unwind_protect_int (tree_return_command::returning);
  tree_return_command::returning = 0;

  unwind_protect_int (tree_break_command::breaking);
  tree_break_command::breaking = 0;

  if (list)
    list->eval ();

  // The unwind_protects are popped off the stack in the reverse of
  // the order they are pushed on.

  // XXX FIXME XXX -- these statements say that if we see a break or
  // return statement in the cleanup block, that we want to use the
  // new value of the breaking or returning flag instead of restoring
  // the previous value.  Is that the right thing to do?  I think so.
  // Consider the case of
  //
  //   function foo ()
  //     unwind_protect
  //       stderr << "1: this should always be executed\n";
  //       break;
  //       stderr << "1: this should never be executed\n";
  //     unwind_protect_cleanup
  //       stderr << "2: this should always be executed\n";
  //       return;
  //       stderr << "2: this should never be executed\n";
  //     end_unwind_protect
  //   endfunction
  //
  // If we reset the value of the breaking flag, both the returning
  // flag and the breaking flag will be set, and we shouldn't have
  // both.  So, use the most recent one.  If there is no return or
  // break in the cleanup block, the values should be reset to
  // whatever they were when the cleanup block was entered.

  if (tree_break_command::breaking || tree_return_command::returning)
    {
      unwind_protect::discard ();
      unwind_protect::discard ();
    }
  else
    {
      unwind_protect::run ();
      unwind_protect::run ();
    }

  // We don't want to ignore errors that occur in the cleanup code, so
  // if an error is encountered there, leave error_state alone.
  // Otherwise, set it back to what it was before.

  if (error_state)
    unwind_protect::discard ();
  else
    unwind_protect::run ();
}

void
tree_unwind_protect_command::eval (void)
{
  unwind_protect::add (do_unwind_protect_cleanup_code, cleanup_code);

  MAYBE_DO_BREAKPOINT;

  if (unwind_protect_code)
    unwind_protect_code->eval ();

  unwind_protect::run ();
}

void
tree_unwind_protect_command::accept (tree_walker& tw)
{
  tw.visit_unwind_protect_command (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
