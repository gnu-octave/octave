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

#if !defined (octave_tree_cmd_h)
#define octave_tree_cmd_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class octave_value_list;

class tree_statement_list;
class tree_global_init_list;
class tree_if_command_list;
class tree_expression;
class tree_index_expression;
class tree_identifier;
class tree_return_list;
class octave_value;
class symbol_record;

class tree_command;
class tree_global_command;
class tree_while_command;
class tree_for_command;
class tree_if_command;
class tree_try_catch_command;
class tree_unwind_protect_command;
class tree_break_command;
class tree_continue_command;
class tree_return_command;

class tree_walker;

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

class
tree_global_command : public tree_command
{
public:

  tree_global_command (int l = -1, int c = -1)
    : tree_command (l, c), init_list (0) { }

  tree_global_command (tree_global_init_list *t, int l = -1, int c = -1)
    : tree_command (l, c), init_list (t) { }

  ~tree_global_command (void);

  void eval (void);

  tree_global_init_list *initializer_list (void) { return init_list; }

  void accept (tree_walker& tw);

private:

  // The list of global variables or initializers in this global
  // command.
  tree_global_init_list *init_list;
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
tree_for_command : public tree_command
{
public:

  tree_for_command (int l = -1, int c = -1)
    : tree_command (l, c), id (0), id_list (0), expr (0), list (0) { }

  tree_for_command (tree_index_expression *ident, tree_expression *e,
		    tree_statement_list *lst, int l = -1, int c = -1)
    : tree_command (l, c), id (ident), id_list (0), expr (e),
      list (lst) { }

  tree_for_command (tree_return_list *ident, tree_expression *e,
		    tree_statement_list *lst, int l = -1, int c = -1)
    : tree_command (l, c), id (0), id_list (ident), expr (e),
      list (lst) { }

  ~tree_for_command (void);

  void eval (void);

  void eval_error (void);

  tree_index_expression *ident (void) { return id; }

  tree_expression *control_expr (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  void accept (tree_walker& tw);

private:

  // Identifier to modify.
  tree_index_expression *id;

  // List of identifiers to modify.
  tree_return_list *id_list;

  // Expression to evaluate.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  void do_for_loop_once (tree_return_list *lst,
			 const octave_value_list& rhs, bool& quit);

  void do_for_loop_once (tree_index_expression *idx_expr,
			 const octave_value& rhs, bool& quit);

  void do_for_loop_once (tree_identifier *ident,
			 octave_value& rhs, bool& quit);
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

  void eval_error (void);

  tree_if_command_list *cmd_list (void) { return list; }

  void accept (tree_walker& tw);

private:

  // List of if commands (if, elseif, elseif, ... else, endif)
  tree_if_command_list *list;
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
