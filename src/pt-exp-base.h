// tree-expr.h                                      -*- C++ -*-
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

#if !defined (octave_tree_expr_h)
#define octave_tree_expr_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <time.h>
#include <stdio.h>

#include "variables.h"
#include "mappers.h"
#include "error.h"
#include "oct-obj.h"

class tree_constant;
class symbol_record;
class symbol_table;

class tree_matrix;
class tree_builtin;
class tree_identifier;
class tree_function;
class tree_expression;
class tree_prefix_expression;
class tree_postfix_expression;
class tree_unary_expression;
class tree_binary_expression;
class tree_assignment_expression;
class tree_simple_assignment_expression;
class tree_multi_assignment_expression;
class tree_colon_expression;
class tree_index_expression;
class tree_argument_list;
class tree_parameter_list;
class tree_return_list;

/*
 * A base class for expressions.
 */
class
tree_expression : public tree
{
public:
  tree_expression (void);

  ~tree_expression (void);

  tree_constant eval (int print);

  virtual int is_identifier (void) const
    { return 0; }

  virtual int is_index_expression (void) const
    { return 0; }

  virtual int is_assignment_expression (void) const
    { return 0; }

  virtual int is_prefix_expression (void) const
    { return 0; }

  virtual void mark_for_possible_ans_assign (void)
    { panic_impossible (); }

  virtual Octave_object eval (int print, int nargout,
			      const Octave_object& args);

protected:
  expression_type etype;
};

/*
 * General matrices.  This allows us to construct matrices from
 * other matrices, variables, and functions.
 */
class
tree_matrix : public tree_expression
{
public:
  tree_matrix (void);
  tree_matrix (tree_expression *e, tree::matrix_dir d);

  ~tree_matrix (void);

  tree_matrix *chain (tree_expression *e, tree::matrix_dir d);
  tree_matrix *reverse (void);
  int length (void);

  tree_return_list *to_return_list (void);

  tree_constant eval (int print);

private:
  tree::matrix_dir dir; // Direction to the next element.
  tree_expression *element;
  tree_matrix *next;
};

/*
 * Prefix expressions.
 */
class
tree_prefix_expression : public tree_expression
{
 public:
  tree_prefix_expression (int l = -1, int c = -1);
  tree_prefix_expression (tree_identifier *t, tree::expression_type et,
			  int l = -1, int c = -1);

  ~tree_prefix_expression (void);

  tree_constant eval (int print);

  void eval_error (void);

  int is_prefix_expression (void) const;

 private:
  tree_identifier *id;
};

/*
 * Postfix expressions.
 */
class
tree_postfix_expression : public tree_expression
{
 public:
  tree_postfix_expression (int l = -1, int c = -1);
  tree_postfix_expression (tree_identifier *t, tree::expression_type et,
			   int l = -1, int c = -1);

  ~tree_postfix_expression (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_identifier *id;
};

/*
 * Unary expressions.
 */
class
tree_unary_expression : public tree_expression
{
 public:
  tree_unary_expression (int l = -1, int c = -1);
  tree_unary_expression (tree_expression *a, tree::expression_type t,
			 int l = -1, int c = -1);

  ~tree_unary_expression (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_expression *op;
};

/*
 * Binary expressions.
 */
class
tree_binary_expression : public tree_expression
{
 public:
  tree_binary_expression (int l = -1, int c = -1);
  tree_binary_expression (tree_expression *a, tree_expression *b,
			  tree::expression_type t, int l = -1, int c = -1);

  ~tree_binary_expression (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_expression *op1;
  tree_expression *op2;
};

/*
 * Assignment expressions.
 */
class
tree_assignment_expression : public tree_expression
{
public:
  int in_parens;

  tree_assignment_expression (void);

  ~tree_assignment_expression (void);

  tree_constant eval (int print);

  int is_assignment_expression (void) const;
};

/*
 * Simple assignment expressions.
 */
class
tree_simple_assignment_expression : public tree_assignment_expression
{
 public:
  tree_simple_assignment_expression (int l = -1, int c = -1);
  tree_simple_assignment_expression (tree_identifier *i,
				     tree_expression *r,
				     int l = -1, int c = -1);
  tree_simple_assignment_expression (tree_index_expression *idx_expr,
				     tree_expression *r, int l = -1, int c = -1);

  ~tree_simple_assignment_expression (void);

  tree_constant eval (int print);

  void eval_error (void);

 private:
  tree_identifier *lhs;
  tree_argument_list *index;
  tree_expression *rhs;
};

/*
 * Multi-valued assignment expressions.
 */
class
tree_multi_assignment_expression : public tree_assignment_expression
{
 public:
  tree_multi_assignment_expression (int l = -1, int c = -1);
  tree_multi_assignment_expression (tree_return_list *lst,
				    tree_expression *r,
				    int l = -1, int c = -1);

  ~tree_multi_assignment_expression (void);

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void eval_error (void);

 private:
  tree_return_list *lhs;
  tree_expression *rhs;
};

/*
 * Colon expressions.
 */
class
tree_colon_expression : public tree_expression
{
 public:
  tree_colon_expression (int l = -1, int c = -1);
  tree_colon_expression (tree_expression *a, tree_expression *b,
			 int l = -1, int c = -1);

  ~tree_colon_expression (void);

  tree_colon_expression *chain (tree_expression *t);

  tree_constant eval (int print);

  void eval_error (const char *s);

 private:
  tree_expression *op1;
  tree_expression *op2;
  tree_expression *op3;
};

/*
 * Index expressions.
 */
class
tree_index_expression : public tree_expression
{
 public:
  tree_index_expression (int l = -1, int c = -1);
  tree_index_expression (tree_identifier *i, int l = -1, int c = -1);
  tree_index_expression (tree_identifier *i, tree_argument_list *lst,
			 int l = -1, int c = -1);

  ~tree_index_expression (void);

  int is_index_expression (void) const;

  tree_identifier *ident (void);

  tree_argument_list *arg_list (void);

  void mark_for_possible_ans_assign (void);

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void eval_error (void);

 private:
  tree_identifier *id;
  tree_argument_list *list;
};

/*
 * A base class for objects that can be evaluated with argument lists.
 */
class
tree_fvc : public tree_expression
{
public:
  virtual int is_constant (void) const
    { return 0; }

//  virtual int is_builtin (void) const
//    { return 0; }

  virtual tree_constant assign (tree_constant& t, const Octave_object& args);

  virtual char *name (void) const
    { panic_impossible (); return 0; }

  virtual void bump_value (tree::expression_type)
    { panic_impossible (); }

  virtual int max_expected_args (void)
    { panic_impossible (); return 0; }
  
  virtual char *fcn_file_name (void)
    { return 0; }

  virtual time_t time_parsed (void)
    { panic_impossible (); return 0; }

  virtual int is_system_fcn_file (void) const
    { return 0; }

  virtual int save (ostream& os, int mark_as_global = 0,
		    int precision = 17)
    { panic_impossible (); return 0; }
};

/*
 * Builtin functions.
 */
class
tree_builtin : public tree_fvc
{
public:
  tree_builtin (const char *nm = 0);

  tree_builtin (int i_max, int o_max, Mapper_fcn& m_fcn,
		const char *nm = 0);

  tree_builtin (int i_max, int o_max, Octave_builtin_fcn f,
		const char *nm = 0);

  ~tree_builtin (void);

//  int is_builtin (void) const;

  int is_mapper_function (void) const;

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  char *name (void) const;

  int max_expected_args (void);

private:
  int nargin_max;
  int nargout_max;
  int is_mapper;
  Mapper_fcn mapper_fcn;
  Octave_builtin_fcn fcn;
  char *my_name;
};

/*
 * Symbols from the symbol table.
 */
class
tree_identifier : public tree_fvc
{
  friend class tree_index_expression;

public:
  tree_identifier (int l = -1, int c = -1);
  tree_identifier (symbol_record *s, int l = -1, int c = -1);

  ~tree_identifier (void);

  int is_identifier (void) const;

  char *name (void) const;
  void rename (const char *n);

  tree_identifier *define (tree_constant *t);
  tree_identifier *define (tree_function *t);

  void document (char *s);

  tree_constant assign (tree_constant& t);
  tree_constant assign (tree_constant& t, const Octave_object& args);

  int is_defined (void);

  void bump_value (tree::expression_type);

  int parse_fcn_file (int exec_script = 1);
  int parse_fcn_file (char *ff, int exec_script = 1);
  void parse_fcn_file (FILE *ffile, char *ff);

  tree_fvc *do_lookup (int& script_file_executed);

  void mark_as_formal_parameter (void);

  void mark_for_possible_ans_assign (void);

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void eval_undefined_error (void);

private:
  symbol_record *sym;
  int maybe_do_ans_assign;
};

/*
 * User defined functions.
 */
class
tree_function : public tree_fvc
{
public:
  tree_function (void);
  tree_function (tree *cl, symbol_table *st);

  ~tree_function (void);

  tree_function *define (tree *t);
  tree_function *define_param_list (tree_parameter_list *t);
  tree_function *define_ret_list (tree_parameter_list *t);

  void stash_fcn_file_name (char * s);
  void stash_fcn_file_time (time_t t);

  char *fcn_file_name (void);
  time_t time_parsed (void);

  void mark_as_system_fcn_file (void);
  int is_system_fcn_file (void) const;

  int takes_varargs (void) const;
  void octave_va_start (void);
  tree_constant octave_va_arg (void);

  void stash_function_name (char *s);
  char *function_name (void);

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  int max_expected_args (void);

  void traceback_error (void);

private:
  int call_depth;
  tree_parameter_list *param_list;
  tree_parameter_list *ret_list;
  symbol_table *sym_tab;
  tree *cmd_list;
  char *file_name;
  char *fcn_name;
  time_t t_parsed;
  int system_fcn_file;
  int num_named_args;
  Octave_object args_passed;
  int num_args_passed;
  int curr_va_arg_number;
};

/*
 * Argument lists.
 */
class
tree_argument_list : public tree
{
 public:
  tree_argument_list (void);
  tree_argument_list (tree *t);

  ~tree_argument_list (void);

  tree_argument_list *chain (tree *t);
  tree_argument_list *reverse (void);
  int length (void);

  tree_argument_list *next_elem (void);

  Octave_object convert_to_const_vector (void);

  tree_constant eval (int print);

 private:
  tree *arg;
  tree_argument_list *next;
};

/*
 * Parameter lists.  Almost like argument lists, except that the
 * elements are only supposed to be identifiers, never constants or
 * expressions.
 */
class
tree_parameter_list : public tree
{
 public:
  tree_parameter_list (void);
  tree_parameter_list (tree_identifier *t);

  ~tree_parameter_list (void);

  tree_parameter_list *chain (tree_identifier *t);
  tree_parameter_list *reverse (void);
  int length (void);

  char *name (void) const;

  void mark_as_formal_parameters (void);

  void mark_varargs (void);
  int takes_varargs (void) const;

  void mark_varargs_only (void);
  int varargs_only (void);

  tree_identifier *define (tree_constant *t);

  void define_from_arg_vector (const Octave_object& args);

  int is_defined (void);

  Octave_object convert_to_const_vector (void);

  tree_parameter_list *next_elem (void);

  tree_constant eval (int print);

 private:
  int marked_for_varargs;
  tree_identifier *param;
  tree_parameter_list *next;
};

/*
 * Return lists.  Almost like parameter lists, except that the
 * elements may also be index expressions.
 */
class
tree_return_list : public tree
{
 public:
  tree_return_list (void);
  tree_return_list (tree_identifier *t);
  tree_return_list (tree_index_expression *t);

  ~tree_return_list (void);

  tree_return_list *chain (tree_identifier *t);
  tree_return_list *chain (tree_index_expression *t);
  tree_return_list *reverse (void);
  int length (void);

  tree_index_expression *idx_expr (void);

  tree_return_list *next_elem (void);

  tree_constant eval (int print);

 private:
  tree_index_expression *retval;
  tree_return_list *next;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
