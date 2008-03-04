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

  octave_value_list eval (bool silent, int nargout, bool in_function_body);

  tree_expression *expression (void) { return expr; }

  octave_comment_list *comment_text (void) { return comm; }

  // Allow modification of this statement.  Note that there is no
  // checking.  If you use these, are you sure you knwo what you are
  // doing?

  void set_command (tree_command *c) { cmd = c; }

  void set_expression (tree_expression *e) { expr = e; }

  tree_statement *dup (symbol_table::scope_id scope);

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
    : function_body (false) { }

  tree_statement_list (tree_statement *s)
    : function_body (false) { append (s); }

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

  octave_value_list eval (bool silent = false, int nargout = 0);

  int set_breakpoint (int line);

  void delete_breakpoint (int line);

  octave_value_list list_breakpoints (void);

  tree_statement_list *dup (symbol_table::scope_id scope);

  void accept (tree_walker& tw);

private:

  // Does this list of statements make up the body of a function?
  bool function_body;

  // No copying!

  tree_statement_list (const tree_statement_list&);

  tree_statement_list& operator = (const tree_statement_list&);
};

class tree_statement_stack
{
protected:

  tree_statement_stack (void) : tss () { }

public:

  typedef std::deque<tree_statement *>::iterator iterator ;

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new tree_statement_stack ();

    if (! instance)
      {
	::error ("unable to create stmt stack object!");

	retval = false;
      }

    return retval;
  }

  // Current statement (top of stack).
  static tree_statement *current (void) { return top (); }

  static int current_line (void)
  {
    tree_statement *s = current ();
    return s ? s->line () : -1;
  }

  static int current_column (void)
  {
    tree_statement *s = current ();
    return s ? s->column () : -1;
  }

  // Statement at position N on the call stack (N == 0 is current).
  static tree_statement *element (size_t n)
  {
    return instance_ok () ? instance->do_element (n) : 0;
  }

  // Caller statement
  static tree_statement *caller (void) { return element (1); }

  static void push (tree_statement *f)
  {
    if (instance_ok ())
      instance->do_push (f);
  }

  static tree_statement *top (void)
  {
    return instance_ok () ? instance->do_top (): 0;
  }

  static void pop (void)
  {
    if (instance_ok ())
      instance->do_pop ();
  }
  
  // A function for popping the top of the call stack that is suitable
  // for use as an unwind_protect handler.
  static void unwind_pop (void *) { pop (); }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

private:

  // The current stmt stack.
  std::deque<tree_statement *> tss;

  static tree_statement_stack *instance;

  tree_statement *do_element (size_t n) { return tss.size () > n ? tss[n] : 0; }

  void do_push (tree_statement *f) { tss.push_front (f); }

  tree_statement *do_top (void) { return tss.empty () ? 0 : tss.front (); }

  void do_pop (void)
  {
    if (! tss.empty ())
      tss.pop_front ();
  }

  void do_clear (void) { tss.clear (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
