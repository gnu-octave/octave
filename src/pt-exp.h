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

#if !defined (octave_tree_expr2_h)
#define octave_tree_expr2_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class tree_identifier;
class tree_index_expression;
class tree_indirect_ref;
class tree_argument_list;
class tree_assignment_lhs;

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "oct-obj.h"
#include "pt-exp-base.h"

// Unary expressions.

class
tree_unary_expression : public tree_expression
{
public:

  tree_unary_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op (0)  { }

  tree_unary_expression (tree_expression *e, int l = -1, int c = -1)
    : tree_expression (l, c), op (e) { }

  ~tree_unary_expression (void) { delete op; }

  tree_expression *operand (void) { return op; }

protected:

  // The operand for the expression.
  tree_expression *op;
};

// Prefix expressions.

class
tree_prefix_expression : public tree_unary_expression
{
public:

  enum type
    {
      unknown,
      unot,
      uminus,
      increment,
      decrement
    };

  tree_prefix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c), etype (unknown) { }

  tree_prefix_expression (type t = unknown, tree_expression *e,
			  int l = -1, int c = -1)
    : tree_unary_expression (e, l, c), etype (t) { }

  ~tree_prefix_expression (void) { }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargou);

  void eval_error (void);

  string oper (void) const;

  void accept (tree_walker& tw);

private:

  // The type of the expression.
  type etype;
};

// Postfix expressions.

class
tree_postfix_expression : public tree_unary_expression
{
public:

  enum type
    {
      unknown,
      hermitian,
      transpose,
      increment,
      decrement
    };

  tree_postfix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c), etype (unknown) { }

  tree_postfix_expression (type t = unknown, tree_expression *e,
			   int l = -1, int c = -1)
    : tree_unary_expression (e, l, c), etype (t) { }

  ~tree_postfix_expression (void) { }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void eval_error (void);

  string oper (void) const;

  void accept (tree_walker& tw);

private:

  // The type of the expression.
  type etype;
};

// Binary expressions.

class
tree_binary_expression : public tree_expression
{
public:

  tree_binary_expression (int l = -1, int c = -1,
			  octave_value::binary_op t
			    = octave_value::unknown_binary_op)
    : tree_expression (l, c), op_lhs (0), op_rhs (0), etype (t) { }

  tree_binary_expression (tree_expression *a, tree_expression *b,
			  int l = -1, int c = -1,
			  octave_value::binary_op t
			    = octave_value::unknown_binary_op)
    : tree_expression (l, c), op_lhs (a), op_rhs (b), etype (t) { }

  ~tree_binary_expression (void)
    {
      delete op_lhs;
      delete op_rhs;
    }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargou);

  void eval_error (void);

  string oper (void) const;

  tree_expression *lhs (void) { return op_lhs; }
  tree_expression *rhs (void) { return op_rhs; }

  void accept (tree_walker& tw);

protected:

  // The operands for the expression.
  tree_expression *op_lhs;
  tree_expression *op_rhs;

private:

  // The type of the expression.
  octave_value::binary_op etype;
};

// Boolean expressions.

class
tree_boolean_expression : public tree_binary_expression
{
public:

  enum type
    {
      unknown,
      bool_and,
      bool_or
    };

  tree_boolean_expression (int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (l, c), etype (t) { }

  tree_boolean_expression (tree_expression *a, tree_expression *b,
			   int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (a, b, l, c), etype (t) { }

  ~tree_boolean_expression (void) { }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  string oper (void) const;

private:

  // The type of the expression.
  type etype;
};

// Simple assignment expressions.

class
tree_simple_assignment : public tree_expression
{
public:

  tree_simple_assignment (bool plhs = false, int l = -1, int c = -1,
			  octave_value::assign_op t = octave_value::asn_eq)
    : tree_expression (l, c), lhs (0), rhs (0), preserve (plhs), etype (t) { }

  tree_simple_assignment (tree_expression *le, tree_expression *re,
			  bool plhs = false, int l = -1, int c = -1,
			  octave_value::assign_op t = octave_value::asn_eq)
    : tree_expression (l, c), lhs (le), rhs (re), preserve (plhs),
      etype (t) { }

  ~tree_simple_assignment (void);

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  bool is_assignment_expression (void) const
    { return true; }

  void eval_error (void);

  string oper (void) const;

  tree_expression *left_hand_side (void) { return lhs; }

  tree_expression *right_hand_side (void) { return rhs; }

  void accept (tree_walker& tw);

private:

  void do_assign (octave_lvalue& ult, const octave_value_list& args,
		  const octave_value& rhs_val);

  void do_assign (octave_lvalue& ult, const octave_value& rhs_val);

  // The left hand side of the assignment.
  tree_expression *lhs;

  // The right hand side of the assignment.
  tree_expression *rhs;

  // True if we should not delete the lhs.
  bool preserve;

  // True if this is an assignment to the built-in variable ans.
  bool ans_ass;

  // The type of the expression.
  octave_value::assign_op etype;
};

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
public:

  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op_base (0), op_limit (0), op_increment (0) { }

  tree_colon_expression (tree_expression *e, int l = -1, int c = -1)
    : tree_expression (l, c), op_base (e), op_limit (0), op_increment (0) { }

  ~tree_colon_expression (void)
    {
      delete op_base;
      delete op_limit;
      delete op_increment;
    }

  tree_colon_expression *append (tree_expression *t);

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void eval_error (const string& s = string ());

  tree_expression *base (void) { return op_base; }
  tree_expression *limit (void) { return op_limit; }
  tree_expression *increment (void) { return op_increment; }

  void accept (tree_walker& tw);

private:

  // The components of the expression.
  tree_expression *op_base;
  tree_expression *op_limit;
  tree_expression *op_increment;
};

// Index expressions.

class
tree_index_expression : public tree_expression
{
public:

  tree_index_expression (tree_expression *e = 0, tree_argument_list *lst = 0,
			 int l = -1, int c = -1)
    : tree_expression (l, c), expr (e), list (lst), arg_nm () { }

  ~tree_index_expression (void);

  bool is_index_expression (void) const
    { return true; }

  tree_expression *expression (void)
    { return expr; }

  tree_argument_list *arg_list (void)
    { return list; }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  octave_lvalue lvalue (void);

  void eval_error (void);

  void accept (tree_walker& tw);

private:

  tree_expression *expr;

  tree_argument_list *list;

  string_vector arg_nm;
};

// Multi-valued assignment expressions.

class
tree_multi_assignment : public tree_expression
{
public:

  tree_multi_assignment (bool plhs = false, int l = -1, int c = -1)
    : tree_expression (l, c), preserve (plhs), lhs (0), rhs (0) { }

  tree_multi_assignment (tree_argument_list *lst, tree_expression *r,
			 bool plhs = false, int l = -1, int c = -1)
    : tree_expression (l, c), preserve (plhs), lhs (lst), rhs (r) { }

  ~tree_multi_assignment (void);

  bool is_assignment_expression (void) const
    { return true; }

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void eval_error (void);

  tree_argument_list *left_hand_side (void) { return lhs; }

  tree_expression *right_hand_side (void) { return rhs; }

  void accept (tree_walker& tw);

private:

  bool preserve;
  tree_argument_list *lhs;
  tree_expression *rhs;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
