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

#if !defined (octave_tree_cmd_h)
#define octave_tree_cmd_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class octave_value_list;

class tree_argument_list;
class tree_statement_list;
class tree_decl_init_list;
class tree_if_command_list;
class tree_switch_case_list;
class tree_expression;
class tree_index_expression;
class tree_identifier;
class tree_return_list;
class octave_value;

class tree_command;
class tree_decl_command;
class tree_global_command;
class tree_static_command;
class tree_while_command;
class tree_simple_for_command;
class tree_complex_for_command;
class tree_if_command;
class tree_switch_command;
class tree_try_catch_command;
class tree_unwind_protect_command;
class tree_no_op_command;
class tree_break_command;
class tree_continue_command;
class tree_return_command;

class tree_walker;

#include <string>

#include "pt-base.h"

// A base class for commands.

class
tree_command : public tree
{
public:

  tree_command (int l = -1, int c = -1) : tree (l, c) { }

  virtual ~tree_command (void) { }

  virtual void eval (void) = 0;
};

// Base class for declaration commands -- global, static, etc.

class
tree_decl_command : public tree_command
{
public:

  tree_decl_command (const string& n, int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (0) { }

  tree_decl_command (const string& n, tree_decl_init_list *t,
		     int l = -1, int c = -1)
    : tree_command (l, c), cmd_name (n), initialized (false), init_list (t) { }

  ~tree_decl_command (void);

  tree_decl_init_list *initializer_list (void) { return init_list; }

  void accept (tree_walker& tw);

  string name (void) { return cmd_name; }

protected:

  // The name of this command -- global, static, etc.
  string cmd_name;

  // TRUE if this command has been evaluated.
  bool initialized;

  // The list of variables or initializers in this declaration command.
  tree_decl_init_list *init_list;
};

// Global.

class
tree_global_command : public tree_decl_command
{
public:

  tree_global_command (int l = -1, int c = -1)
    : tree_decl_command ("global", l, c) { }

  tree_global_command (tree_decl_init_list *t, int l = -1, int c = -1)
    : tree_decl_command ("global", t, l, c) { }

  ~tree_global_command (void) { }

  void eval (void);
};

// Static.

class
tree_static_command : public tree_decl_command
{
public:

  tree_static_command (int l = -1, int c = -1)
    : tree_decl_command ("static", l, c) { }

  tree_static_command (tree_decl_init_list *t, int l = -1, int c = -1)
    : tree_decl_command ("static", t, l, c) { }

  ~tree_static_command (void) { }

  void eval (void);
};

// While.

class
tree_while_command : public tree_command
{
public:

  tree_while_command (int l = -1, int c = -1)
    : tree_command (l, c), expr (0), list (0) { }

  tree_while_command (tree_expression *e, int l = -1, int c = -1)
    : tree_command (l, c), expr (e), list (0) { }

  tree_while_command (tree_expression *e, tree_statement_list *lst,
		      int l = -1, int c = -1)
    : tree_command (l, c), expr (e), list (lst) { }

  ~tree_while_command (void);

  void eval (void);

  void eval_error (void);

  tree_expression *condition (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  void accept (tree_walker& tw);

private:

  // Expression to test.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;
};

// For.

class
tree_simple_for_command : public tree_command
{
public:

  tree_simple_for_command (int l = -1, int c = -1)
    : tree_command (l, c), lhs (0), expr (0), list (0) { }

  tree_simple_for_command (tree_expression *le, tree_expression *re,
			   tree_statement_list *lst, int l = -1, int c = -1)
    : tree_command (l, c), lhs (le), expr (re), list (lst) { }

  ~tree_simple_for_command (void);

  void eval (void);

  void eval_error (void);

  tree_expression *left_hand_side (void) { return lhs; }

  tree_expression *control_expr (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  void accept (tree_walker& tw);

private:

  // Expression to modify.
  tree_expression *lhs;

  // Expression to evaluate.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  void do_for_loop_once (octave_variable_reference &ult,
			 const octave_value& rhs, bool& quit);
};

class
tree_complex_for_command : public tree_command
{
public:

  tree_complex_for_command (int l = -1, int c = -1)
    : tree_command (l, c), lhs (0), expr (0), list (0) { }

  tree_complex_for_command (tree_argument_list *le, tree_expression *re,
			    tree_statement_list *lst, int l = -1, int c = -1)
    : tree_command (l, c), lhs (le), expr (re), list (lst) { }

  ~tree_complex_for_command (void);

  void eval (void);

  void eval_error (void);

  tree_argument_list *left_hand_side (void) { return lhs; }

  tree_expression *control_expr (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  void accept (tree_walker& tw);

private:

  // Expression to modify.
  tree_argument_list *lhs;

  // Expression to evaluate.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  void do_for_loop_once (octave_variable_reference &val_ref,
			 octave_variable_reference &key_ref,
			 const octave_value& val,
			 const octave_value& key,
			 bool& quit);
};

// If.

class
tree_if_command : public tree_command
{
public:

  tree_if_command (int l = -1, int c = -1)
    : tree_command (l, c), list (0) { }

  tree_if_command (tree_if_command_list *lst, int l = -1, int c = -1)
    : tree_command (l, c), list (lst) { }

  ~tree_if_command (void);

  void eval (void);

  tree_if_command_list *cmd_list (void) { return list; }

  void accept (tree_walker& tw);

private:

  // List of if commands (if, elseif, elseif, ... else, endif)
  tree_if_command_list *list;
};

// Switch.

class
tree_switch_command : public tree_command
{
public:

  tree_switch_command (int l = -1, int c = -1)
    : tree_command (l, c), expr (0), list (0) { }

  tree_switch_command (tree_expression *e, tree_switch_case_list *lst,
		       int l = -1, int c = -1)
    : tree_command (l, c), expr (e), list (lst) { }

  ~tree_switch_command (void);

  void eval (void);

  void eval_error (void);

  tree_expression *switch_value (void) { return expr; }

  tree_switch_case_list *case_list (void) { return list; }

  void accept (tree_walker& tw);

private:

  // Value on which to switch.
  tree_expression *expr;

  // List of cases (case 1, case 2, ..., default)
  tree_switch_case_list *list;
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

// Simple exception handling.

class
tree_try_catch_command : public tree_command
{
public:

  tree_try_catch_command (int l = -1, int c = -1)
    : tree_command (l, c), try_code (0), catch_code (0) { }

  tree_try_catch_command (tree_statement_list *tc,
			       tree_statement_list *cc,
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

// No-op.

class
tree_no_op_command : public tree_command
{
public:

  tree_no_op_command (const string& cmd = "no_op", int l = -1, int c = -1)
    : tree_command (l, c), orig_cmd (cmd) { }

  ~tree_no_op_command (void) { }

  void eval (void) { }

  void accept (tree_walker& tw);

  string original_command (void) { return orig_cmd; }

private:

  string orig_cmd;
};

// Break.

class
tree_break_command : public tree_command
{
public:

  tree_break_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_break_command (void) { }

  void eval (void);

  void accept (tree_walker& tw);
};

// Continue.

class
tree_continue_command : public tree_command
{
public:

  tree_continue_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_continue_command (void) { }

  void eval (void);

  void accept (tree_walker& tw);
};

// Return.

class
tree_return_command : public tree_command
{
public:

  tree_return_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_return_command (void) { }

  void eval (void);

  void accept (tree_walker& tw);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
