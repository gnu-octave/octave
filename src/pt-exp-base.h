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

#if !defined (octave_tree_expr_h)
#define octave_tree_expr_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

class octave_value;
class octave_variable_reference;

#include "pt-base.h"

// A base class for expressions.

class
tree_expression : public tree
{
public:

  tree_expression (int l = -1, int c = -1)
    : tree (l, c), num_parens (0) { }

  virtual ~tree_expression (void) { }

  virtual bool is_constant (void) const
    { return false; }

  virtual bool is_matrix_constant (void) const
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

  virtual int paren_count (void) const
    { return num_parens; }

  virtual void mark_for_possible_ans_assign (void);

  virtual octave_value eval (bool print = false) = 0;

  virtual octave_variable_reference reference (void);

  virtual string oper (void) const
    { return "<unknown>"; }

  virtual string original_text (void) const;

  tree_expression *mark_in_parens (void)
    {
      num_parens++;
      return this;
    }

protected:

  // A count of the number of times this expression appears directly
  // inside a set of parentheses.
  //
  //   (((e1)) + e2)  ==> 2 for expression e1
  //                  ==> 1 for expression ((e1)) + e2
  //                  ==> 0 for expression e2
  int num_parens;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
