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

#if !defined (octave_tree_misc_h)
#define octave_tree_misc_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class octave_value_list;
class octave_value;
class tree_command;
class tree_expression;
class tree_simple_assignment_expression;
class tree_index_expression;
class tree_identifier;
class symbol_record;
class symbol_table;

class tree_statement;
class tree_statement_list;
class tree_argument_list;
class tree_parameter_list;
class tree_return_list;
class tree_va_return_list;
class tree_global;
class tree_global_init_list;

class tree_walker;

#include <SLList.h>

#include "pt-base.h"

// A statement is either a command to execute or an expression to
// evaluate.

class
tree_statement
{
friend class tree_statement_list;

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

  void maybe_echo_code (bool);

  bool print_result (void) { return print_flag; }

  tree_command *command (void) { return cmd; }

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

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout);

  void accept (tree_walker& tw);

private:

  // Does this list of statements make up the body of a function?
  bool function_body;
};

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class
tree_argument_list : public SLList<tree_expression *>
{
public:

  tree_argument_list (void)
    : SLList<tree_expression *> () { }

  tree_argument_list (tree_expression *t)
    : SLList<tree_expression *> () { append (t); }

  ~tree_argument_list (void)
    {
      while (! empty ())
	{
	  tree_expression *t = remove_front ();
	  delete t;
	}
    }

  octave_value_list convert_to_const_vector (void);

  void accept (tree_walker& tw);
};

// Parameter lists.  Used to hold the list of input and output
// parameters in a function definition.  Elements are identifiers
// only.

class
tree_parameter_list : public SLList<tree_identifier *>
{
public:

  tree_parameter_list (void)
    : SLList<tree_identifier *> (), marked_for_varargs (0) { }

  tree_parameter_list (tree_identifier *t)
    : SLList<tree_identifier *> (), marked_for_varargs (0) { append (t); }

  ~tree_parameter_list (void);

  void mark_as_formal_parameters (void);

  void mark_varargs (void)
    { marked_for_varargs = 1; }

  bool takes_varargs (void) const
    { return marked_for_varargs != 0; }

  void mark_varargs_only (void)
    { marked_for_varargs = -1; }

  bool varargs_only (void)
    { return (marked_for_varargs < 0); }

  void initialize_undefined_elements (octave_value& val);

  void define_from_arg_vector (const octave_value_list& args);

  bool is_defined (void);

  octave_value_list convert_to_const_vector (tree_va_return_list *vr_list);

  void accept (tree_walker& tw);

private:

  int marked_for_varargs;
};

// Return lists.  Used to hold the right hand sides of multiple
// assignment expressions.

class
tree_return_list : public SLList<tree_index_expression *>
{
public:

  tree_return_list (void)
    : SLList<tree_index_expression *> () { }

  tree_return_list (tree_index_expression *t)
    : SLList<tree_index_expression *> () { append (t); }

  ~tree_return_list (void);

  void accept (tree_walker& tw);
};

class
tree_va_return_list : public SLList<octave_value>
{
public:

  tree_va_return_list (void) : SLList<octave_value> () { }

  ~tree_va_return_list (void) { }
};

// List of expressions that make up a global statement.

class
tree_global
{
public:

  tree_global (void)
    : id (0), ass_expr (0) { }

  tree_global (tree_identifier *i)
    : id (i), ass_expr (0) { }

  tree_global (tree_simple_assignment_expression *ass)
    : id (0), ass_expr (ass) { }

  ~tree_global (void);

  void eval (void);

  tree_identifier *ident (void) { return id; }

  tree_simple_assignment_expression *assign_expr (void) { return ass_expr; }

  void accept (tree_walker& tw);

private:

  // Only one of id or ass_expr can be valid at once.

  // An identifier to make global.
  tree_identifier *id;

  // An assignemnt expression.  Valid only if the left hand side of
  // the assignment is a simple identifier.
  tree_simple_assignment_expression *ass_expr;
};

class
tree_global_init_list : public SLList<tree_global *>
{
public:

  tree_global_init_list (void)
    : SLList<tree_global *> () { }

  tree_global_init_list (tree_global *t)
    : SLList<tree_global *> () { append (t); }

  ~tree_global_init_list (void)
    {
      while (! empty ())
	{
	  tree_global *t = remove_front ();
	  delete t;
	}
    }

  void eval (void);

  void accept (tree_walker& tw);
};

class
tree_if_clause
{
public:

  tree_if_clause (void) : expr (0), list (0) { }

  tree_if_clause (tree_statement_list *l)
    : expr (0), list (l) { }

  tree_if_clause (tree_expression *e, tree_statement_list *l)
    : expr (e), list (l) { }

  ~tree_if_clause (void);

  bool is_else_clause (void)
    { return ! expr; }

  int eval (void);

  tree_expression *condition (void) { return expr; }

  tree_statement_list *commands (void) { return list; }

  void accept (tree_walker& tw);

private:

  // The condition to test.
  tree_expression *expr;

  // The list of statements to evaluate if expr is true.
  tree_statement_list *list;
};

class
tree_if_command_list : public SLList<tree_if_clause *>
{
public:

  tree_if_command_list (void)
    : SLList<tree_if_clause *> () { }

  tree_if_command_list (tree_if_clause *t)
    : SLList<tree_if_clause *> () { append (t); }

  ~tree_if_command_list (void)
    {
      while (! empty ())
	{
	  tree_if_clause *t = remove_front ();
	  delete t;
	}
    }

  void eval (void);

  void accept (tree_walker& tw);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
