// tree-expr2.h                                      -*- C++ -*-
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
  tree_postfix_expression (int l = -1, int c = -1)
    : tree_expression (l, c), id (0) { }

  tree_postfix_expression (tree_identifier *t, tree_expression::type et,
			   int l = -1, int c = -1)
    : tree_expression (l, c, et), id (t) { }

  ~tree_postfix_expression (void);

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
  tree_unary_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op (0) { }

  tree_unary_expression (tree_expression *a, tree_expression::type t,
			 int l = -1, int c = -1)
    : tree_expression (l, c, t), op (a) { }

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
  tree_binary_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op1 (0), op2 (0) { }

  tree_binary_expression (tree_expression *a, tree_expression *b,
			  tree_expression::type t, int l = -1, int c = -1)
    : tree_expression (l, c, t), op1 (a), op2 (b) { }

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
				     int l = -1, int c = -1);

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
				     int l = -1, int c = -1);

  ~tree_simple_assignment_expression (void);

  int left_hand_side_is_identifier_only (void);

  tree_identifier *left_hand_side_id (void);

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

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
 public:
  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c, tree_expression::colon),
      op1(0), op2 (0), op3 (0) { }

  tree_colon_expression (tree_expression *a, tree_expression *b,
			 int l = -1, int c = -1)
    : tree_expression (l, c, tree_expression::colon),
      op1 (a), op2 (b), op3 (0) { }

  ~tree_colon_expression (void)
    {
      delete op1;
      delete op2;
      delete op3;
    }

  int is_range_constant (void) const;

  tree_colon_expression *chain (tree_expression *t);

  tree_constant eval (int print);

  void eval_error (const char *s);

  void print_code (ostream& os);

 private:
  tree_expression *op1;
  tree_expression *op2;
  tree_expression *op3;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
