// tree-expr.h                                      -*- C++ -*-
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

#if !defined (octave_tree_expr_h)
#define octave_tree_expr_h 1

#include <time.h>
#include <stdio.h>
#include <iostream.h>

#include "SLList.h"

#include "variables.h"
#include "mappers.h"
#include "error.h"
#include "oct-obj.h"

class tree_constant;
class tree_statement_list;
class tree_argument_list;
class tree_parameter_list;
class tree_return_list;
class tree_va_return_list;
class symbol_record;
class symbol_table;

class tree_matrix;
class tree_builtin;
class tree_identifier;
class tree_indirect_ref;
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

#include "tree-base.h"

// A base class for expressions.

class
tree_expression : public tree
{
public:
  int in_parens;

  enum type
    {
      unknown,
      assignment,
      simple_assignment,
      multi_assignment,
      add,
      subtract,
      multiply,
      el_mul,
      divide,
      el_div,
      leftdiv,
      el_leftdiv,
      power,
      elem_pow,
      cmp_lt,
      cmp_le,
      cmp_eq,
      cmp_ge,
      cmp_gt,
      cmp_ne,
      and_and,
      or_or,
      and,
      or,
      not,
      unot,
      uminus,
      hermitian,
      transpose,
      colon,
      index,
      increment,
      decrement,
   };

  tree_expression (int l = -1, int c = -1) : tree (l, c)
    {
      in_parens = 0;
      etype = unknown;
    }

  virtual ~tree_expression (void) { }

  virtual int is_multi_val_ret_expression (void) const
    { return 0; }

  virtual int is_identifier (void) const
    { return 0; }

  virtual int is_indirect_ref (void) const
    { return 0; }

  virtual int is_index_expression (void) const
    { return 0; }

  virtual int is_assignment_expression (void) const
    { return 0; }

  virtual int is_prefix_expression (void) const
    { return 0; }

  virtual void mark_for_possible_ans_assign (void)
    { panic_impossible (); }

  virtual tree_constant eval (int print) = 0;

protected:
  type etype;
};

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class
tree_matrix : public tree_expression
{
public:
  enum dir
    {
      md_none,
      md_right,
      md_down,
    };

  tree_matrix (void)
    {
      direction = tree_matrix::md_none;
      element = 0;
      next = 0;
    }

  tree_matrix (tree_expression *e, tree_matrix::dir d)
    {
      direction = d;
      element = e;
      next = 0;
    }

  ~tree_matrix (void);

  tree_matrix *chain (tree_expression *e, tree_matrix::dir d);
  tree_matrix *reverse (void);
  int length (void);

  tree_return_list *to_return_list (void);

  tree_constant eval (int print);

  void print_code (ostream& os);

private:
  tree_matrix::dir direction; // Direction from the previous element.
  tree_expression *element;
  tree_matrix *next;
};

// A base class for objects that can be return multiple values

class
tree_multi_val_ret : public tree_expression
{
public:
  tree_multi_val_ret (int l = -1, int c = -1) : tree_expression (l, c) { }

  ~tree_multi_val_ret (void) { }

  int is_multi_val_ret_expression (void) const
    { return 1; }

  tree_constant eval (int print);

  virtual Octave_object eval (int print, int nargout,
			      const Octave_object& args) = 0;
};

// Used internally.

class
tree_oct_obj : public tree_multi_val_ret
{
public:
  tree_oct_obj (int l = -1, int c = -1) : tree_multi_val_ret (l, c) { }

  tree_oct_obj (const Octave_object& v, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	values = v;
      }

  ~tree_oct_obj (void) { }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void print_code (ostream& os) { }

private:
  Octave_object values;
};

// A base class for objects that can be evaluated with argument lists.

class
tree_fvc : public tree_multi_val_ret
{
public:
  tree_fvc (int l = -1, int c = -1) : tree_multi_val_ret (l, c) { }

  ~tree_fvc (void) { }

  virtual int is_constant (void) const
    { return 0; }

  virtual tree_constant assign (tree_constant& t,
				const Octave_object& args);

  virtual char *name (void) const
    { panic_impossible (); return 0; }

  virtual void bump_value (tree_expression::type)
    { panic_impossible (); }

  virtual tree_constant lookup_map_element (SLList<char*>& list);

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

// Symbols from the symbol table.

class
tree_identifier : public tree_fvc
{
  friend class tree_index_expression;

public:
  tree_identifier (int l = -1, int c = -1) : tree_fvc (l, c)
    {
      sym = 0;
      maybe_do_ans_assign = 0;
    }

  tree_identifier (symbol_record *s, int l = -1, int c = -1) : tree_fvc (l, c)
    {
      sym = s;
      maybe_do_ans_assign = 0;
    }

  ~tree_identifier (void) { }

  int is_identifier (void) const
    { return 1; }

  char *name (void) const;

  tree_identifier *define (tree_constant *t);
  tree_identifier *define (tree_function *t);

  void document (char *s);

  tree_constant assign (tree_constant& t);
  tree_constant assign (tree_constant& t, const Octave_object& args);

  tree_constant assign (SLList<char*> list, tree_constant& t);
  tree_constant assign (SLList<char*> list, tree_constant& t,
			const Octave_object& args); 

  int is_defined (void);

  void bump_value (tree_expression::type);

  tree_fvc *do_lookup (int& script_file_executed, int exec_script = 1);

  void link_to_global (void);

  void mark_as_formal_parameter (void);

  void mark_for_possible_ans_assign (void)
    { maybe_do_ans_assign = 1; }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void eval_undefined_error (void);

  void print_code (ostream& os);

private:
  symbol_record *sym;
  int maybe_do_ans_assign;
};

// Indirect references to values (structure references).

class
tree_indirect_ref : public tree_fvc
{
public:
  tree_indirect_ref (int l = -1, int c = -1) : tree_fvc (l, c)
    {
      id = 0;
      preserve_ident = 0;
    }

  tree_indirect_ref (tree_identifier *i, int l = -1, int c = -1)
    : tree_fvc (l, c)
      {
	id = i;
	preserve_ident = 0;
      }

  ~tree_indirect_ref (void);

  tree_indirect_ref *chain (const char *s);

  int is_indirect_ref (void) const
    { return 1; }

  int is_identifier_only (void) const
    { return (id && refs.empty ()); }

  tree_identifier *ident (void)
    { return id; }

  void preserve_identifier (void)
    { preserve_ident = 1; }

  char *name (void);

  tree_constant assign (tree_constant& t);
  tree_constant assign (tree_constant& t, const Octave_object& args);

  void mark_for_possible_ans_assign (void)
    { id->mark_for_possible_ans_assign (); }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void print_code (ostream& os);

private:
  tree_identifier *id;
  SLList<char*> refs;
  int preserve_ident;
};

// Index expressions.

class
tree_index_expression : public tree_multi_val_ret
{
public:
  tree_index_expression (int l = -1, int c = -1) : tree_multi_val_ret (l, c)
    {
      id = 0;
      list = 0;
    }

  tree_index_expression (tree_identifier *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = 0;
      }

  tree_index_expression (tree_identifier *i, tree_argument_list *lst,
			 int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = new tree_indirect_ref (i);
	list = lst;
      }

  tree_index_expression (tree_indirect_ref *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = i;
	list = 0;
      }

  tree_index_expression (tree_indirect_ref *i, tree_argument_list *lst,
			 int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	id = i;
	list = lst;
      }

  ~tree_index_expression (void);

  int is_index_expression (void) const
    { return 1; }

  tree_indirect_ref *ident (void)
    { return id; }

  char *name (void)
    { return id->name (); }

  tree_argument_list *arg_list (void)
    { return list; }

  void mark_for_possible_ans_assign (void)
    {
      if (id)
	id->mark_for_possible_ans_assign ();
    }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  void eval_error (void);

  void print_code (ostream& os);

 private:
  tree_indirect_ref *id;
  tree_argument_list *list;
};

// Prefix expressions.

class
tree_prefix_expression : public tree_expression
{
 public:
  tree_prefix_expression (int l = -1, int c = -1) : tree_expression (l, c)
    {
      id = 0;
      etype = unknown;
    }

  tree_prefix_expression (tree_identifier *t, tree_expression::type et,
			  int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	id = t;
	etype = et;
      }

  ~tree_prefix_expression (void)
    { delete id; }

  tree_constant eval (int print);

  void eval_error (void);

  int is_prefix_expression (void) const
    { return 1; }

  char *oper (void) const;

  void print_code (ostream& os);

 private:
  tree_identifier *id;
};

// Postfix expressions.

class
tree_postfix_expression : public tree_expression
{
 public:
  tree_postfix_expression (int l = -1, int c = -1) : tree_expression (l, c)
    {
      id = 0;
      etype = unknown;
    }

  tree_postfix_expression (tree_identifier *t, tree_expression::type et,
			   int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	id = t;
	etype = et;
      }

  ~tree_postfix_expression (void)
    { delete id; }

  tree_constant eval (int print);

  void eval_error (void);

  char *oper (void) const;

  void print_code (ostream& os);

 private:
  tree_identifier *id;
};

// Unary expressions.

class
tree_unary_expression : public tree_expression
{
 public:
  tree_unary_expression (int l = -1, int c = -1) : tree_expression (l, c)
    {
      etype = tree_expression::unknown;
      op = 0;
    }

  tree_unary_expression (tree_expression *a, tree_expression::type t,
			 int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	etype = t;
	op = a;
      }

  ~tree_unary_expression (void)
    { delete op; }

  tree_constant eval (int print);

  void eval_error (void);

  char *oper (void) const;

  void print_code (ostream& os);

 private:
  tree_expression *op;
};

// Binary expressions.

class
tree_binary_expression : public tree_expression
{
 public:
  tree_binary_expression (int l = -1, int c = -1) : tree_expression (l, c)
    {
      etype = tree_expression::unknown;
      op1 = 0;
      op2 = 0;
    }

  tree_binary_expression (tree_expression *a, tree_expression *b,
			  tree_expression::type t, int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	etype = t;
	op1 = a;
	op2 = b;
      }

  ~tree_binary_expression (void)
    {
      delete op1;
      delete op2;
    }

  tree_constant eval (int print);

  void eval_error (void);

  char *oper (void) const;

  void print_code (ostream& os);

 private:
  tree_expression *op1;
  tree_expression *op2;
};

// Simple assignment expressions.

class
tree_simple_assignment_expression : public tree_expression
{
private:
  void init (int plhs, int ans_assign)
    {
      etype = tree_expression::assignment;
      lhs_idx_expr = 0;
      lhs = 0;
      index = 0;
      rhs = 0;
      preserve = plhs;
      ans_ass = ans_assign;
    }

 public:
  tree_simple_assignment_expression (int plhs = 0, int ans_assign = 0,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      { init (plhs, ans_assign); }

  tree_simple_assignment_expression (tree_identifier *i,
				     tree_expression *r,
				     int plhs = 0, int ans_assign = 0,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs = new tree_indirect_ref (i);
	rhs = r;
      }

  tree_simple_assignment_expression (tree_indirect_ref *i,
				     tree_expression *r,
				     int plhs = 0, int ans_assign = 0,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs = i;
	rhs = r;
      }

  tree_simple_assignment_expression (tree_index_expression *idx_expr,
				     tree_expression *r,
				     int plhs = 0, int ans_assign = 0,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs_idx_expr = idx_expr; // cache this -- we may need to delete it.
	lhs = idx_expr->ident ();
	index = idx_expr->arg_list ();
	rhs = r;
      }

  ~tree_simple_assignment_expression (void);

  int left_hand_side_is_identifier_only (void)
    { return lhs->is_identifier_only (); }

  tree_identifier *left_hand_side_id (void)
    { return lhs->ident (); }

  int is_ans_assign (void)
    { return ans_ass; }

  tree_constant eval (int print);

  int is_assignment_expression (void) const
    { return 1; }

  void eval_error (void);

  void print_code (ostream& os);

 private:
  tree_index_expression *lhs_idx_expr;
  tree_indirect_ref *lhs;
  tree_argument_list *index;
  tree_expression *rhs;
  int preserve;
  int ans_ass;
};

// Multi-valued assignment expressions.

class
tree_multi_assignment_expression : public tree_multi_val_ret
{
 public:
  tree_multi_assignment_expression (int plhs = 0, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	etype = tree_expression::multi_assignment;
	preserve = plhs;
	lhs = 0;
	rhs = 0;
      }

  tree_multi_assignment_expression (tree_return_list *lst,
				    tree_multi_val_ret *r,
				    int plhs = 0,
				    int l = -1, int c = -1)
    : tree_multi_val_ret (l, c)
      {
	etype = tree_expression::multi_assignment;
	preserve = plhs;
	lhs = lst;
	rhs = r;
      }

  ~tree_multi_assignment_expression (void);

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  int is_assignment_expression (void) const
    { return 1; }

  void eval_error (void);

  void print_code (ostream& os);

 private:
  int preserve;
  tree_return_list *lhs;
  tree_multi_val_ret *rhs;
};

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
 public:
  tree_colon_expression (int l = -1, int c = -1) : tree_expression (l, c)
    {
      etype = tree_expression::colon;
      op1 = 0;
      op2 = 0;
      op3 = 0;
    }

  tree_colon_expression (tree_expression *a, tree_expression *b,
			 int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	etype = tree_expression::colon;
	op1 = a;
	op2 = b;
	op3 = 0;
      }

  ~tree_colon_expression (void)
    {
      delete op1;
      delete op2;
      delete op3;
    }

  tree_colon_expression *chain (tree_expression *t);

  tree_constant eval (int print);

  void eval_error (const char *s);

  void print_code (ostream& os);

 private:
  tree_expression *op1;
  tree_expression *op2;
  tree_expression *op3;
};

// Builtin functions.

class
tree_builtin : public tree_fvc
{
public:
  tree_builtin (const char *nm = 0);

  tree_builtin (int i_max, int o_max, Mapper_fcn& m_fcn,
		const char *nm = 0);

  tree_builtin (int i_max, int o_max, Octave_builtin_fcn f,
		const char *nm = 0);

  ~tree_builtin (void) { }  // XXX ?? XXX

//  int is_builtin (void) const;

  int is_mapper_function (void) const
    { return is_mapper; }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  char *name (void) const
    { return my_name; }

  int max_expected_args (void);

  void print_code (ostream& os)
    {
      os << my_name << " can't be printed because it is a builtin function\n";
    }

private:
  int nargin_max;
  int nargout_max;
  int is_mapper;
  Mapper_fcn mapper_fcn;
  Octave_builtin_fcn fcn;
  char *my_name;
};

// User defined functions.

class
tree_function : public tree_fvc
{
private:
  void install_nargin_and_nargout (void);

  void bind_nargin_and_nargout (int nargin, int nargout);

  void init (void)
    {
      call_depth = 0;
      param_list = 0;
      ret_list = 0;
      sym_tab = 0;
      cmd_list = 0;
      file_name = 0;
      fcn_name = 0;
      t_parsed = 0;
      system_fcn_file = 0;
      num_named_args = 0;
      num_args_passed = 0;
      curr_va_arg_number = 0;
      vr_list = 0;
    }

public:
  tree_function (int l = -1, int c = -1) : tree_fvc (l, c)
    { init (); }

  tree_function (tree_statement_list *cl, symbol_table *st,
		 int l = -1, int c = -1)
     : tree_fvc (l, c)
       {
	 init ();
	 sym_tab = st;
	 cmd_list = cl;
	 install_nargin_and_nargout ();
       }

  ~tree_function (void);

//  tree_function *define (tree_statement_list *t);
  tree_function *define_param_list (tree_parameter_list *t);
  tree_function *define_ret_list (tree_parameter_list *t);

  void stash_fcn_file_name (void);

  void stash_fcn_file_time (time_t t)
    { t_parsed = t; }

  char *fcn_file_name (void)
    { return file_name; }

  time_t time_parsed (void)
    { return t_parsed; }

  void mark_as_system_fcn_file (void);

  int is_system_fcn_file (void) const
    { return system_fcn_file; }

  int takes_varargs (void) const;

  void octave_va_start (void)
    { curr_va_arg_number = num_named_args; }

  tree_constant octave_va_arg (void);

  Octave_object octave_all_va_args (void);

  int takes_var_return (void) const;

  void octave_vr_val (const tree_constant& val);

  void stash_function_name (char *s);

  char *function_name (void)
    { return fcn_name; }

  tree_constant eval (int print);

  Octave_object eval (int print, int nargout, const Octave_object& args);

  int max_expected_args (void);

  void traceback_error (void);

  void print_code (ostream& os);

private:
  int call_depth;
  tree_parameter_list *param_list;
  tree_parameter_list *ret_list;
  symbol_table *sym_tab;
  tree_statement_list *cmd_list;
  char *file_name;
  char *fcn_name;
  time_t t_parsed;
  int system_fcn_file;
  int num_named_args;
  Octave_object args_passed;
  int num_args_passed;
  int curr_va_arg_number;
  tree_va_return_list *vr_list;
  symbol_record *nargin_sr;
  symbol_record *nargout_sr;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
