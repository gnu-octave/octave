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

#if !defined (octave_tree_unop_h)
#define octave_tree_unop_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "pt-exp.h"

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

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
