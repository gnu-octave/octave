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

#if !defined (octave_tree_colon_h)
#define octave_tree_colon 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "pt-exp.h"

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
public:

  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op_base (0), op_limit (0),
      op_increment (0), save_base (false) { }

  tree_colon_expression (tree_expression *e, int l = -1, int c = -1)
    : tree_expression (l, c), op_base (e), op_limit (0),
      op_increment (0), save_base (false) { }

  ~tree_colon_expression (void)
    {
      if (! save_base)
	delete op_base;

      delete op_limit;
      delete op_increment;
    }

  bool has_magic_end (void) const
    {
      return ((op_base && op_base->has_magic_end ())
	      || (op_limit && op_limit->has_magic_end ())
	      || (op_increment && op_increment->has_magic_end ()));
    }

  void preserve_base (void) { save_base = true; }

  tree_colon_expression *append (tree_expression *t);

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void eval_error (const std::string& s = std::string ());

  tree_expression *base (void) { return op_base; }

  tree_expression *limit (void) { return op_limit; }

  tree_expression *increment (void) { return op_increment; }

  int line (void) const;
  int column (void) const;

  void accept (tree_walker& tw);

private:

  // The components of the expression.
  tree_expression *op_base;
  tree_expression *op_limit;
  tree_expression *op_increment;

  bool save_base;

  // No copying!

  tree_colon_expression (const tree_colon_expression&);

  tree_colon_expression& operator = (const tree_colon_expression&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
