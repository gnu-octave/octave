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

#if !defined (octave_tree_stmt_h)
#define octave_tree_stmt_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <SLList.h>

class octave_value_list;

class tree_command;
class tree_expression;

class tree_walker;

// A statement is either a command to execute or an expression to
// evaluate.

class
tree_statement
{
public:

  tree_statement (void)
    : cmd (0), expr (0), print_flag (true) { }

  tree_statement (tree_command *c)
    : cmd (c), expr (0), print_flag (true) { }

  tree_statement (tree_expression *e)
    : cmd (0), expr (e), print_flag (true) { }

  ~tree_statement (void);

  void set_print_flag (bool print)
    { print_flag = print; }

  bool is_command (void)
    { return cmd != 0; }

  bool is_expression (void)
    { return expr != 0; }

  int line (void);
  int column (void);

  void maybe_echo_code (bool in_function_body);

  bool print_result (void) { return print_flag; }

  tree_command *command (void) { return cmd; }

  octave_value_list eval (bool silent, int nargout, bool in_function_body);

  tree_expression *expression (void) { return expr; }

  void accept (tree_walker& tw);

private:

  // Only one of cmd or expr can be valid at once.

  // Command to execute.
  tree_command *cmd;

  // Expression to evaluate.
  tree_expression *expr;

  // Print result of eval for this command?
  bool print_flag;

  // No copying!

  tree_statement (const tree_statement&);

  tree_statement& operator = (const tree_statement&);
};

// A list of statements to evaluate.

class
tree_statement_list : public SLList<tree_statement *>
{
public:

  tree_statement_list (void)
    : SLList<tree_statement *> (), function_body (false) { }

  tree_statement_list (tree_statement *s)
    : SLList<tree_statement *> (), function_body (false) { append (s); }

  ~tree_statement_list (void)
    {
      while (! empty ())
	{
	  tree_statement *t = remove_front ();
	  delete t;
	}
    }

  void mark_as_function_body (void) { function_body = true; }

  octave_value_list eval (bool silent = false, int nargout = 0);

  void accept (tree_walker& tw);

private:

  // Does this list of statements make up the body of a function?
  bool function_body;

  // No copying!

  tree_statement_list (const tree_statement_list&);

  tree_statement_list& operator = (const tree_statement_list&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
