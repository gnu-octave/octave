// tree-cmd.h                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#include <iostream.h>

class tree_statement_list;
class tree_global_init_list;
class tree_if_command_list;
class tree_expression;
class tree_index_expression;
class tree_constant;
class symbol_record;

class tree_command;
class tree_global_command;
class tree_while_command;
class tree_for_command;
class tree_if_command;
class tree_unwind_protect_command;
class tree_break_command;
class tree_continue_command;
class tree_return_command;

#include "tree-base.h"

// A base class for commands.

class
tree_command : public tree
{
public:
  tree_command (int l = -1, int c = -1) : tree (l, c) { }

  virtual void eval (void) = 0;
};

class
tree_global_command : public tree_command
{
public:
  tree_global_command (int l = -1, int c = -1) : tree_command (l, c)
    { init_list = 0; }

  tree_global_command (tree_global_init_list *t, int l = -1, int c = -1)
    : tree_command (l, c)
      { init_list = t; }

  ~tree_global_command (void);

  void eval (void);

  void print_code (ostream& os);

private:
  tree_global_init_list *init_list;
};

// While.

class
tree_while_command : public tree_command
{
public:
  tree_while_command (int l = -1, int c = -1) : tree_command (l, c)
    {
      expr = 0;
      list = 0;
    }

  tree_while_command (tree_expression *e, int l = -1, int c = -1)
    : tree_command (l, c)
      {
	expr = e;
	list = 0;
      }

  tree_while_command (tree_expression *e, tree_statement_list *lst,
		      int l = -1, int c = -1)
    : tree_command (l, c)
      {
	expr = e;
	list = lst;
      }

  ~tree_while_command (void);

  void eval (void);

  void eval_error (void);

  void print_code (ostream& os);

private:
  tree_expression *expr;	// Expression to test.
  tree_statement_list *list;	// List of commands to execute.
};

// For.

class
tree_for_command : public tree_command
{
public:
  tree_for_command (int l = -1, int c = -1) : tree_command (l, c)
    {
      id = 0;
      expr = 0;
      list = 0;
    }

  tree_for_command (tree_index_expression *ident, tree_expression *e,
		    tree_statement_list *lst, int l = -1, int c = -1)
    : tree_command (l, c)
      {
	id = ident;
	expr = e;
	list = lst;
      }

  ~tree_for_command (void);

  void eval (void);

  void eval_error (void);

  void print_code (ostream& os);

private:
  void do_for_loop_once (tree_constant *rhs, int& quit);

  tree_index_expression *id;	// Identifier to modify.
  tree_expression *expr;	// Expression to evaluate.
  tree_statement_list *list;	// List of commands to execute.
};

// If.

class
tree_if_command : public tree_command
{
public:
  tree_if_command (int l = -1, int c = -1) : tree_command (l, c)
    { list = 0; }

  tree_if_command (tree_if_command_list *lst, int l = -1, int c = -1)
    : tree_command (l, c)
      { list = lst; }

  ~tree_if_command (void);

  void eval (void);

  void eval_error (void);

  void print_code (ostream& os);

private:
  tree_if_command_list *list;
};

// Simple exception handling.

class
tree_unwind_protect_command : public tree_command
{
public:
  tree_unwind_protect_command (int l = -1, int c = -1) : tree_command (l, c)
    {
      unwind_protect_code = 0;
      cleanup_code = 0;
    }

  tree_unwind_protect_command (tree_statement_list *tc,
			       tree_statement_list *cc,
			       int l = -1, int c = -1)
    : tree_command (l, c)
      {
	unwind_protect_code = tc;
	cleanup_code = cc;
      }

  ~tree_unwind_protect_command (void);

  void eval (void);

  void print_code (ostream& os);

private:
  tree_statement_list *unwind_protect_code;
  tree_statement_list *cleanup_code;
};

// Break.

class
tree_break_command : public tree_command
{
public:
  tree_break_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_break_command (void) { }

  void eval (void);

  void print_code (ostream& os);
};

// Continue.

class
tree_continue_command : public tree_command
{
public:
  tree_continue_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_continue_command (void) { }

  void eval (void);

  void print_code (ostream& os);
};

// Return.

class
tree_return_command : public tree_command
{
public:
  tree_return_command (int l = -1, int c = -1) : tree_command (l, c) { }

  ~tree_return_command (void) { }

  void eval (void);

  void print_code (ostream& os);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
