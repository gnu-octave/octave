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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"
#include "pt-bp.h"
#include "pt-jump.h"
#include "pt-walk.h"

class octave_value_list;

// Break.

// Nonzero means we're breaking out of a loop or function body.
int tree_break_command::breaking = 0;

void
tree_break_command::eval (void)
{
  // Even if we have an error we should still enter debug mode.
  MAYBE_DO_BREAKPOINT;

  if (! error_state)
    breaking = 1;
}

tree_command *
tree_break_command::dup (symbol_table *)
{
  return new tree_break_command (line (), column ());
}

void
tree_break_command::accept (tree_walker& tw)
{
  tw.visit_break_command (*this);
}

// Continue.

// Nonzero means we're jumping to the end of a loop.
int tree_continue_command::continuing = 0;

void
tree_continue_command::eval (void)
{
  MAYBE_DO_BREAKPOINT;

  if (! error_state)
    continuing = 1;
}

tree_command *
tree_continue_command::dup (symbol_table *)
{
  return new tree_continue_command (line (), column ());
}

void
tree_continue_command::accept (tree_walker& tw)
{
  tw.visit_continue_command (*this);
}

// Return.

// Nonzero means we're returning from a function.
int tree_return_command::returning = 0;

void
tree_return_command::eval (void)
{
  MAYBE_DO_BREAKPOINT;

  if (! error_state)
    returning = 1;
}

tree_command *
tree_return_command::dup (symbol_table *)
{
  return new tree_return_command (line (), column ());
}

void
tree_return_command::accept (tree_walker& tw)
{
  tw.visit_return_command (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
