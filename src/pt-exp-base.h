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

#if !defined (octave_tree_expr_h)
#define octave_tree_expr_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_constant;

#include "pt-base.h"

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

  tree_expression (int l = -1, int c = -1, type et = unknown)
    : tree (l, c), in_parens (0), etype (et) { }

  virtual ~tree_expression (void) { }

  virtual bool is_constant (void) const
    { return false; }

  virtual bool is_matrix_constant (void) const
    { return false; }

  virtual bool is_range_constant (void) const
    { return false; }

  virtual bool is_multi_val_ret_expression (void) const
    { return false; }

  virtual bool is_identifier (void) const
    { return false; }

  virtual bool is_indirect_ref (void) const
    { return false; }

  virtual bool is_index_expression (void) const
    { return false; }

  virtual bool is_assignment_expression (void) const
    { return false; }

  virtual bool is_prefix_expression (void) const
    { return false; }

  virtual bool is_logically_true (const char *);

  virtual void mark_for_possible_ans_assign (void);

  virtual tree_constant eval (bool print) = 0;

protected:
  type etype;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
