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

#if !defined (octave_tree_mvr2_h)
#define octave_tree_mvr2_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class octave_value_list;

class tree_argument_list;
class tree_identifier;
class tree_index_expression;
class tree_indirect_ref;
class tree_return_list;

class tree_walker;

#include <string>

#include "ov.h"
#include "pt-mvr-base.h"
#include "oct-obj.h"

// Used internally.

class
tree_oct_obj : public tree_multi_val_ret
{
public:

  tree_oct_obj (int l = -1, int c = -1) : tree_multi_val_ret (l, c) { }

  tree_oct_obj (const octave_value_list& v, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), values (v) { }

  ~tree_oct_obj (void) { }

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout,
			  const octave_value_list& args);

  void accept (tree_walker& tw);

private:

  const octave_value_list values;
};

// Index expressions.

class
tree_index_expression : public tree_multi_val_ret
{
public:

  tree_index_expression (int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (0), list (0) { }

  tree_index_expression (tree_identifier *i, int l = -1, int c = -1);

  tree_index_expression (tree_identifier *i, tree_argument_list *lst,
			 int l = -1, int c = -1);

  tree_index_expression (tree_indirect_ref *i, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (i), list (0) { }

  tree_index_expression (tree_indirect_ref *i, tree_argument_list *lst,
			 int l = -1, int c = -1)
    : tree_multi_val_ret (l, c), id (i), list (lst) { }

  ~tree_index_expression (void);

  bool is_index_expression (void) const
    { return true; }

  tree_indirect_ref *ident (void)
    { return id; }

  string name (void);

  tree_argument_list *arg_list (void)
    { return list; }

  void mark_for_possible_ans_assign (void);

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout, const octave_value_list& args);

  void eval_error (void);

  void accept (tree_walker& tw);

private:

  tree_indirect_ref *id;

  tree_argument_list *list;
};

// Multi-valued assignment expressions.

class
tree_multi_assignment_expression : public tree_multi_val_ret
{
public:

  tree_multi_assignment_expression (bool plhs = false, int l = -1, int c = -1)
    : tree_multi_val_ret (l, c, tree_expression::multi_assignment),
      preserve (plhs), lhs (0), rhs (0) { }

  tree_multi_assignment_expression (tree_return_list *lst,
				    tree_multi_val_ret *r,
				    bool plhs = false,
				    int l = -1, int c = -1)
    : tree_multi_val_ret (l, c, tree_expression::multi_assignment),
      preserve (plhs), lhs (lst), rhs (r) { }

  ~tree_multi_assignment_expression (void);

  octave_value eval (bool print);

  octave_value_list eval (bool print, int nargout, const octave_value_list& args);

  bool is_assignment_expression (void) const
    { return true; }

  void eval_error (void);

  tree_return_list *left_hand_side (void) { return lhs; }

  tree_multi_val_ret *right_hand_side (void) { return rhs; }

  void accept (tree_walker& tw);

private:

  bool preserve;
  tree_return_list *lhs;
  tree_multi_val_ret *rhs;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
