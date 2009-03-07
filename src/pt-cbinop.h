/*

Copyright (C) 2008, 2009 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_tree_cbinop_h)
#define octave_tree_cbinop_h 1

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "ov.h"
#include "pt-binop.h"
#include "symtab.h"

// Binary expressions that can be reduced to compound operations

class
tree_compound_binary_expression : public tree_binary_expression
{
public:

  tree_compound_binary_expression (tree_expression *a, tree_expression *b,
                                   int l, int c,
                                   octave_value::binary_op t,
                                   tree_expression *ca, tree_expression *cb,
                                   octave_value::compound_binary_op ct)
    : tree_binary_expression (a, b, l, c, t), op_lhs (ca), op_rhs (cb),
      etype (ct) { }

  octave_value rvalue1 (int nargout = 1);

  octave_value::compound_binary_op cop_type (void) const { return etype; }

private:

  tree_expression *op_lhs;
  tree_expression *op_rhs;
  octave_value::compound_binary_op etype;
};

// a "virtual constructor"

tree_binary_expression *
maybe_compound_binary_expression (tree_expression *a, tree_expression *b,
                                  int l = -1, int c = -1,
                                  octave_value::binary_op t
                                  = octave_value::unknown_binary_op);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
