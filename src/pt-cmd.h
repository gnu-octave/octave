// Tree classes.                                      -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_tree_cmd_h)
#define octave_tree_cmd_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_expression;
class tree_index_expression;
class tree_constant;
class symbol_record;

class tree_command;
class tree_command_list;
class tree_global_command;
class tree_while_command;
class tree_for_command;
class tree_if_command;
class tree_break_command;
class tree_continue_command;
class tree_return_command;

/*
 * A base class for commands.
 */
class
tree_command : public tree
{
};

/*
 * A command or two to be executed.
 */
class
tree_command_list : public tree_command
{
 public:
  tree_command_list (void);
  tree_command_list (tree *t);

  ~tree_command_list (void);

  tree_command_list *chain (tree *t);
  tree_command_list *reverse (void);

  void set_print_flag (int print);

  tree_constant eval (int print);

 private:
  tree *command;		// Command to execute.
  int print_flag;		// Print result of eval for this command?
  tree_command_list *next;	// Next command in list.
};

/*
 * Global.
 */
class
tree_global_command : public tree_command
{
 public:
  tree_global_command (int l = -1, int c = -1);
  tree_global_command (symbol_record *s, int l = -1, int c = -1);
  tree_global_command (symbol_record *s, tree_expression *e,
		       int l = -1, int c = -1); 

  ~tree_global_command (void);

  tree_global_command *chain (symbol_record *s, int l = -1, int c = -1);
  tree_global_command *chain (symbol_record *s, tree_expression *e,
			      int l = -1, int c = -1);
  tree_global_command *reverse (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  symbol_record *sr;		// Symbol record from local symbol table.
  tree_expression *rhs;		// RHS of assignment.
  tree_global_command *next;	// Next global command.
};

/*
 * While.
 */
class
tree_while_command : public tree_command
{
 public:
  tree_while_command (int l = -1, int c = -1);
  tree_while_command (tree_expression *e, int l = -1, int c = -1);
  tree_while_command (tree_expression *e, tree *lst, int l = -1, int c = -1);

  ~tree_while_command (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_expression *expr;	// Expression to test.
  tree *list;			// List of commands to execute.
};

/*
 * For.
 */
class
tree_for_command : public tree_command
{
 public:
  tree_for_command (int l = -1, int c = -1);
  tree_for_command (tree_index_expression *id, tree_expression *e, tree *lst,
		    int l = -1, int c = -1);

  ~tree_for_command (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_constant do_for_loop_once (tree_constant *rhs, int& quit);

  tree_index_expression *id;	// Identifier to modify.
  tree_expression *expr;	// Expression to evaluate.
  tree *list;			// List of commands to execute.
};

/*
 * Simple if.
 */
class
tree_if_command : public tree_command
{
 public:
  tree_if_command (int l = -1, int c = -1);
  tree_if_command (tree *lst, int l = -1, int c = -1);
  tree_if_command (tree_expression *e, tree *lst, int l = -1, int c = -1);

  ~tree_if_command (void);

  tree_if_command *chain (tree *lst, int l = -1, int c = -1);
  tree_if_command *chain (tree_expression *e, tree *lst, int l = -1,
			  int c = -1);
  tree_if_command *reverse (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_expression *expr;	// Expression to test.
  tree *list;			// Commands to execute.
  tree_if_command *next;	// Next if command.
};

/*
 * Break.
 */
class
tree_break_command : public tree_command
{
 public:
  tree_break_command (int l = -1, int c = -1);

  ~tree_break_command (void);

  tree_constant eval (int print);
};

/*
 * Continue.
 */
class
tree_continue_command : public tree_command
{
 public:
  tree_continue_command (int l = -1, int c = -1);

  ~tree_continue_command (void);

  tree_constant eval (int print);
};

/*
 * Return.
 */
class
tree_return_command : public tree_command
{
 public:
  tree_return_command (int l = -1, int c = -1);

  ~tree_return_command (void);

  tree_constant eval (int print);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
