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

#if !defined (octave_tree_except_h)
#define octave_tree_except_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_statement_list;

class tree_walker;

#include "pt-cmd.h"

// Simple exception handling.

class
tree_try_catch_command : public tree_command
{
public:

  tree_try_catch_command (int l = -1, int c = -1)
    : tree_command (l, c), try_code (0), catch_code (0) { }

  tree_try_catch_command (tree_statement_list *tc, tree_statement_list *cc,
			  int l = -1, int c = -1)
    : tree_command (l, c), try_code (tc), catch_code (cc) { }

  ~tree_try_catch_command (void);

  void eval (void);

  tree_statement_list *body (void) { return try_code; }

  tree_statement_list *cleanup (void) { return catch_code; }

  void accept (tree_walker& tw);

private:

  // The first block of code to attempt to execute.
  tree_statement_list *try_code;

  // The code to execute if an error occurs in the first block.
  tree_statement_list *catch_code;
};

// Simple exception handling.

class
tree_unwind_protect_command : public tree_command
{
public:

  tree_unwind_protect_command (int l = -1, int c = -1)
    : tree_command (l, c), unwind_protect_code (0), cleanup_code (0) { }

  tree_unwind_protect_command (tree_statement_list *tc,
			       tree_statement_list *cc,
			       int l = -1, int c = -1)
    : tree_command (l, c), unwind_protect_code (tc), cleanup_code (cc) { }

  ~tree_unwind_protect_command (void);

  void eval (void);

  tree_statement_list *body (void) { return unwind_protect_code; }

  tree_statement_list *cleanup (void) { return cleanup_code; }

  void accept (tree_walker& tw);

private:

  // The first body of code to attempt to execute.
  tree_statement_list *unwind_protect_code;

  // The body of code to execute no matter what happens in the first
  // body of code.
  tree_statement_list *cleanup_code;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
