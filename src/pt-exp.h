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

class tree_walker;

#include "pt-exp-base.h"

// Prefix expressions.

class
tree_prefix_expression : public tree_expression
{
public:

  tree_prefix_expression (int l = -1, int c = -1)
    : tree_expression (l, c), id (0) { }

  tree_prefix_expression (tree_identifier *t, tree_expression::type et,
			  int l = -1, int c = -1)
    : tree_expression (l, c, et), id (t) { }

  ~tree_prefix_expression (void);

  octave_value eval (bool print);

  void eval_error (void);

  bool is_prefix_expression (void) const
    { return true; }

  char *oper (void) const;

  tree_identifier *ident (void) { return id; }

  void accept (tree_walker& tw);

private:

  // Currently, a prefix expression can only apply to an identifier.
  tree_identifier *id;
};

// Postfix expressions.

class
tree_postfix_expression : public tree_expression
{
public:

  tree_postfix_expression (int l = -1, int c = -1)
    : tree_expression (l, c), id (0) { }

  tree_postfix_expression (tree_identifier *t, tree_expression::type et,
			   int l = -1, int c = -1)
    : tree_expression (l, c, et), id (t) { }

  ~tree_postfix_expression (void);

  octave_value eval (bool print);

  void eval_error (void);

  char *oper (void) const;

  tree_identifier *ident (void) { return id; }

  void accept (tree_walker& tw);

private:

  // Currently, a prefix expression can only apply to an identifier.
  tree_identifier *id;
};

// Unary expressions.

class
tree_unary_expression : public tree_expression
{
public:

  tree_unary_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op (0) { }

  tree_unary_expression (tree_expression *a, tree_expression::type t,
			 int l = -1, int c = -1)
    : tree_expression (l, c, t), op (a) { }

  ~tree_unary_expression (void)
    { delete op; }

  octave_value eval (bool print);

  void eval_error (void);

  char *oper (void) const;

  tree_expression *operand (void) { return op; }

  void accept (tree_walker& tw);

private:

  // The operand for the expression.
  tree_expression *op;
};

// Binary expressions.

class
tree_binary_expression : public tree_expression
{
public:

  tree_binary_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op_lhs (0), op_rhs (0) { }

  tree_binary_expression (tree_expression *a, tree_expression *b,
			  tree_expression::type t, int l = -1, int c = -1)
    : tree_expression (l, c, t), op_lhs (a), op_rhs (b) { }

  ~tree_binary_expression (void)
    {
      delete op_lhs;
      delete op_rhs;
    }

  octave_value eval (bool print);

  void eval_error (void);

  char *oper (void) const;

  tree_expression *lhs (void) { return op_lhs; }
  tree_expression *rhs (void) { return op_rhs; }

  void accept (tree_walker& tw);

private:

  // The operands for the expression.
  tree_expression *op_lhs;
  tree_expression *op_rhs;
};

// Simple assignment expressions.

class
tree_simple_assignment_expression : public tree_expression
{
public:

  tree_simple_assignment_expression (bool plhs = false,
				     bool ans_assign = false,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      { init (plhs, ans_assign); }

  tree_simple_assignment_expression (tree_identifier *i,
				     tree_expression *r,
				     bool plhs = false,
				     bool ans_assign = false,
				     int l = -1, int c = -1);

  tree_simple_assignment_expression (tree_indirect_ref *i,
				     tree_expression *r,
				     bool plhs = false,
				     bool ans_assign = false,
				     int l = -1, int c = -1)
    : tree_expression (l, c)
      {
	init (plhs, ans_assign);
	lhs = i;
	rhs = r;
      }

  tree_simple_assignment_expression (tree_index_expression *idx_expr,
				     tree_expression *r,
				     bool plhs = false,
				     bool ans_assign = false,
				     int l = -1, int c = -1);

  ~tree_simple_assignment_expression (void);

  bool left_hand_side_is_identifier_only (void);

  tree_identifier *left_hand_side_id (void);

  bool is_ans_assign (void)
    { return ans_ass; }

  octave_value eval (bool print);

  bool is_assignment_expression (void) const
    { return true; }

  void eval_error (void);

  tree_indirect_ref *left_hand_side (void) { return lhs; }

  tree_argument_list *lhs_index (void) { return index; }

  tree_expression *right_hand_side (void) { return rhs; }

  void accept (tree_walker& tw);

private:

  // The left hand side of the assignment, as an index expression.  If
  // the assignment is constructed from an index expression, the index
  // expression is split into the its components in the constructor.
  tree_index_expression *lhs_idx_expr;

  // The indirect reference (id or structure reference) on the left
  // hand side of the assignemnt.
  tree_indirect_ref *lhs;

  // The index of the left hand side of the assignment, if any.
  tree_argument_list *index;

  // The right hand side of the assignment.
  tree_expression *rhs;

  // True if we should not delete the lhs.
  bool preserve;

  // True if this is an assignment to the built-in variable ans.
  bool ans_ass;

  void init (bool plhs, bool ans_assign)
    {
      etype = tree_expression::assignment;
      lhs_idx_expr = 0;
      lhs = 0;
      index = 0;
      rhs = 0;
      preserve = plhs;
      ans_ass = ans_assign;
    }

};

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
public:

  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c, tree_expression::colon),
      op_base (0), op_limit (0), op_increment (0) { }

  tree_colon_expression (tree_expression *a, tree_expression *b,
			 int l = -1, int c = -1)
    : tree_expression (l, c, tree_expression::colon),
      op_base (a), op_limit (b), op_increment (0) { }

  ~tree_colon_expression (void)
    {
      delete op_base;
      delete op_limit;
      delete op_increment;
    }

  bool is_range_constant (void) const;

  tree_colon_expression *chain (tree_expression *t);

  octave_value eval (bool print);

  void eval_error (const char *s);

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

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
