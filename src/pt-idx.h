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

#if !defined (octave_tree_index_h)
#define octave_tree_index_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class tree_argument_list;

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "str-vec.h"

#include "pt-exp.h"

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

  // No copying!

  tree_index_expression (const tree_index_expression&);

  tree_index_expression& operator = (const tree_index_expression&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
