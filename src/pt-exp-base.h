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

class octave_value;

#include "pt-base.h"

// A base class for expressions.

class
tree_expression : public tree
{
public:

  enum type
    {
      unknown,
      assignment,
      simple_assignment,
      multi_assignment,
      colon,
      index,
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

  virtual void mark_in_parens (void) { in_parens++; }

  virtual bool is_in_parens (void) { return in_parens; }

  virtual void mark_for_possible_ans_assign (void);

  virtual octave_value eval (bool print) = 0;

  virtual char *oper (void) const { return "<unknown>"; }

protected:

  // Nonzero if this expression appears inside parentheses.
  int in_parens;

  // The type of this expression.
  type etype;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
