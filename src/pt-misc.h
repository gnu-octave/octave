// tree-misc.h                                      -*- C++ -*-
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

#if !defined (octave_tree_misc_h)
#define octave_tree_misc_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;
class Octave_object;
class tree_constant;
class tree_command;
class tree_expression;
class tree_simple_assignment_expression;
class tree_identifier;
class symbol_record;
class symbol_table;

class tree_statement;
class tree_statement_list;
class tree_argument_list;
class tree_parameter_list;
class tree_return_list;
class tree_global;
class tree_global_init_list;

#include <SLList.h>

#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"

// A list of expressions and commands to be executed.

class
tree_statement : public tree_print_code
{
friend class tree_statement_list;

public:
  tree_statement (void)
    {
      command = 0;
      expression = 0;
      print_flag = 1;
    }

  tree_statement (tree_command *c)
    {
      command = c;
      expression = 0;
      print_flag = 1;
    }

  tree_statement (tree_expression *e)
    {
      command = 0;
      expression = e;
      print_flag = 1;
    }

  ~tree_statement (void);

  void set_print_flag (int print)
    { print_flag = print; }

  void print_code (ostream& os);

private:
  tree_command *command;	// Command to execute.
  tree_expression *expression;	// Command to execute.
  int print_flag;		// Print result of eval for this command?
};

class
tree_statement_list : public SLList<tree_statement *>, public tree_print_code
{
public:
  tree_statement_list (void) : SLList<tree_statement *> () { }
  tree_statement_list (tree_statement *s) : SLList<tree_statement *> ()
    { append (s); }

  ~tree_statement_list (void)
    {
      while (! empty ())
	{
	  tree_statement *t = remove_front ();
	  delete t;
	}
    }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout);

  void print_code (ostream& os);
};

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class
tree_argument_list : public SLList<tree_expression *>, public tree_print_code
{
public:
  tree_argument_list (void) : SLList<tree_expression *> () { }
  tree_argument_list (tree_expression *t) : SLList<tree_expression *> ()
    { append (t); }

  ~tree_argument_list (void)
    {
      while (! empty ())
	{
	  tree_expression *t = remove_front ();
	  delete t;
	}
    }

  Octave_object convert_to_const_vector (void);

  void print_code (ostream& os);
};

// Parameter lists.  Used to hold the list of input and output
// parameters in a function definition.  Elements are identifiers
// only.

class
tree_parameter_list : public SLList<tree_identifier *>, public tree_print_code
{
public:
  tree_parameter_list (void) : SLList<tree_identifier *> ()
    { marked_for_varargs = 0; }

  tree_parameter_list (tree_identifier *t) : SLList<tree_identifier *> ()
    {
      marked_for_varargs = 0;
      append (t);
    }

  ~tree_parameter_list (void)
    {
      while (! empty ())
	{
	  tree_identifier *t = remove_front ();
	  delete t;
	}
    }

//  char *name (void) const;

  void mark_as_formal_parameters (void);

  void mark_varargs (void)
    { marked_for_varargs = 1; }

  int takes_varargs (void) const
    { return marked_for_varargs; }

  void mark_varargs_only (void)
    { marked_for_varargs = -1; }

  int varargs_only (void)
    { return (marked_for_varargs < 0); }

  void define_from_arg_vector (const Octave_object& args);

  int is_defined (void);

  Octave_object convert_to_const_vector (void);

  void print_code (ostream& os);

private:
  int marked_for_varargs;
};

// Return lists.  Used to hold the right hand sides of multiple
// assignment expressions.

class
tree_return_list : public SLList<tree_index_expression *>,
  public tree_print_code 
{
public:
  tree_return_list (void) : SLList<tree_index_expression *> () { }
  tree_return_list (tree_index_expression *t)
    : SLList<tree_index_expression *> ()
      { append (t); }

  ~tree_return_list (void)
    {
      while (! empty ())
	{
	  tree_index_expression *t = remove_front ();
	  delete t;
	}
    }

  void print_code (ostream& os);
};

// List of expressions that make up a global statement.

class
tree_global : public tree_print_code
{
public:
  tree_global (void)
    {
      ident = 0;
      assign_expr = 0;
    }

  tree_global (tree_identifier *id)
    {
      ident = id;
      assign_expr = 0;
    }

  tree_global (tree_simple_assignment_expression *ass)
    {
      ident = 0;
      assign_expr = ass;
    }

  ~tree_global (void)
    {
      delete ident;
      delete assign_expr;
    }

  void eval (void);

  void print_code (ostream& os);

private:
  tree_identifier *ident;
  tree_simple_assignment_expression *assign_expr;
};

class
tree_global_init_list : public SLList<tree_global *>, public tree_print_code
{
public:
  tree_global_init_list (void) : SLList<tree_global *> () { }
  tree_global_init_list (tree_global *t) : SLList<tree_global *> ()
    { append (t); }

  ~tree_global_init_list (void)
    {
      while (! empty ())
	{
	  tree_global *t = remove_front ();
	  delete t;
	}
    }

  void eval (void);

  void print_code (ostream& os);
};

class
tree_if_clause : public tree_print_code
{
public:
  tree_if_clause (void)
    {
      expr = 0;
      list = 0;
    }

  tree_if_clause (tree_statement_list *l)
    {
      expr = 0;
      list = l;
    }

  tree_if_clause (tree_expression *e, tree_statement_list *l)
    {
      expr = e;
      list = l;
    }

  ~tree_if_clause (void)
    {
      delete expr;
      delete list;
    }

  int eval (void);

  void print_code (ostream& os);

private:
  tree_expression *expr;
  tree_statement_list *list;
};

class
tree_if_command_list : public SLList<tree_if_clause *>, public tree_print_code
{
public:
  tree_if_command_list (void) : SLList<tree_if_clause *> () { }
  tree_if_command_list (tree_if_clause *t) : SLList<tree_if_clause *> ()
    { append (t); }

  ~tree_if_command_list (void)
    {
      while (! empty ())
	{
	  tree_if_clause *t = remove_front ();
	  delete t;
	}
    }

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
