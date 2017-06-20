/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "ov.h"
#include "pt-unop.h"

namespace octave
{
  // Unary expressions.

  std::string
  tree_unary_expression::oper (void) const
  {
    return octave_value::unary_op_as_string (etype);
  }

  // Prefix expressions.

  tree_expression *
  tree_prefix_expression::dup (symbol_table::scope& scope) const
  {
    tree_prefix_expression *new_pe
      = new tree_prefix_expression (op ? op->dup (scope) : 0,
                                    line (), column (), etype);

    new_pe->copy_base (*this);

    return new_pe;
  }

  // Postfix expressions.

  tree_expression *
  tree_postfix_expression::dup (symbol_table::scope& scope) const
  {
    tree_postfix_expression *new_pe
      = new tree_postfix_expression (op ? op->dup (scope) : 0,
                                     line (), column (), etype);

    new_pe->copy_base (*this);

    return new_pe;
  }
}
