/*

Copyright (C) 1996, 1997, 2000, 2001, 2002, 2004, 2005, 2006, 2007
              John W. Eaton

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

#if !defined (octave_tree_stmt_h)
#define octave_tree_stmt_h 1

class octave_value_list;

class tree_command;
class tree_expression;

class tree_walker;

#include <deque>

#include "base-list.h"
#include "comment-list.h"
#include "symtab.h"

// A statement is either a command to execute or an expression to
// evaluate.

class
tree_statement
{
public:

  tree_statement (void)
    : cmd (0), expr (0), comm (0), print_flag (true) { }

  tree_statement (tree_command *c, octave_comment_list *cl)
    : cmd (c), expr (0), comm (cl), print_flag (true) { }

  tree_statement (tree_expression *e, octave_comment_list *cl)
    : cmd (0), expr (e), comm (cl), print_flag (true) { }

  ~tree_statement (void);

  void set_print_flag (bool print) { print_flag = print; }

  bool is_command (void) { return cmd != 0; }

  bool is_expression (void) { return expr != 0; }

  int line (void);
  int column (void);

  void maybe_echo_code (bool in_function_body);

  bool print_result (void) { return print_flag; }

  tree_command *command (void) { return cmd; }

  octave_value_list eval (bool silent, int nargout,
			  bool in_function_or_script_body);

  tree_expression *expression (void) { return expr; }

  octave_comment_list *comment_text (void) { return comm; }

  // Allow modification of this statement.  Note that there is no
  // checking.  If you use these, are you sure you knwo what you are
  // doing?

  void set_command (tree_command *c) { cmd = c; }

  void set_expression (tree_expression *e) { expr = e; }

  tree_statement *dup (symbol_table::scope_id scope,
		       symbol_table::context_id context);

  void accept (tree_walker& tw);

private:

  // Only one of cmd or expr can be valid at once.

  // Command to execute.
  tree_command *cmd;

  // Expression to evaluate.
  tree_expression *expr;

  // Comment associated with this statement.
  octave_comment_list *comm;

  // Print result of eval for this command?
  bool print_flag;

  // No copying!
  tree_statement (const tree_statement&);

  tree_statement& operator = (const tree_statement&);
};

// A list of statements to evaluate.

class
tree_statement_list : public octave_base_list<tree_statement *>
{
public:

  tree_statement_list (void)
    : function_body (false), script_body (false) { }

  tree_statement_list (tree_statement *s)
    : function_body (false), script_body (false) { append (s); }

  ~tree_statement_list (void)
    {
      while (! empty ())
	{
	  iterator p = begin ();
	  delete *p;
	  erase (p);
	}
    }

  void mark_as_function_body (void) { function_body = true; }

  void mark_as_script_body (void) { script_body = true; }

  octave_value_list eval (bool silent = false, int nargout = 0);

  int set_breakpoint (int line);

  void delete_breakpoint (int line);

  octave_value_list list_breakpoints (void);

  tree_statement_list *dup (symbol_table::scope_id scope,
			    symbol_table::context_id context);

  void accept (tree_walker& tw);

private:

  // Does this list of statements make up the body of a function?
  bool function_body;

  // Does this list of statements make up the body of a script?
  bool script_body;

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
