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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
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
}

static void
do_catch_code (void *ptr)
{
  tree_statement_list *list = static_cast<tree_statement_list *> (ptr);

  // Set up for letting the user print any messages from errors that
  // occurred in the body of the try_catch statement.

  buffer_error_messages = false;
  bind_global_error_variable ();
  unwind_protect::add (clear_global_error_variable, 0);

  // Similarly, if we have seen a return or break statement, allow all
  // the catch code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  unwind_protect_int (tree_return_command::returning);
  tree_return_command::returning = 0;

  unwind_protect_int (tree_break_command::breaking);
  tree_break_command::breaking = 0;

  if (list)
    list->eval ();

  // This is the one for breaking.  (The unwind_protects are popped
  // off the stack in the reverse of the order they are pushed on).

  // XXX FIXME XXX -- inside a try-catch, should break work like
  // a return, or just jump to the end of the try_catch block?
  // The following code makes it just jump to the end of the block.

  unwind_protect::run ();
  if (tree_break_command::breaking)
    tree_break_command::breaking--;

  // This is the one for returning.

  if (tree_return_command::returning)
    unwind_protect::discard ();
  else
    unwind_protect::run ();

  unwind_protect::run ();
}

void
tree_try_catch_command::eval (void)
{
  unwind_protect::begin_frame ("tree_try_catch::eval");

  unwind_protect::add (do_catch_code, catch_code);

  if (catch_code)
    {
      unwind_protect_bool (buffer_error_messages);
      buffer_error_messages = true;
    }

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
      unwind_protect::discard_frame ("tree_try_catch::eval");
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

  // This is the one for breaking.  (The unwind_protects are popped
  // off the stack in the reverse of the order they are pushed on).

  // XXX FIXME XXX -- inside an unwind_protect, should break work like
  // a return, or just jump to the end of the unwind_protect block?
  // The following code makes it just jump to the end of the block.

  unwind_protect::run ();
  if (tree_break_command::breaking)
    tree_break_command::breaking--;

  // This is the one for returning.

  if (tree_return_command::returning)
    unwind_protect::discard ();
  else
    unwind_protect::run ();

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
