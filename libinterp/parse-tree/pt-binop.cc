/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "ov.h"
#include "pt-binop.h"
#include "variables.h"

namespace octave
{
  // Binary expressions.

  void
  tree_binary_expression::matlab_style_short_circuit_warning (const char *op)
  {
    warning_with_id ("Octave:possible-matlab-short-circuit-operator",
                     "Matlab-style short-circuit operation performed for operator %s",
                     op);

    m_braindead_shortcircuit_warning_issued = true;
  }

  std::string
  tree_binary_expression::oper (void) const
  {
    return octave_value::binary_op_as_string (m_etype);
  }

  tree_expression *
  tree_binary_expression::dup (symbol_scope& scope) const
  {
    tree_binary_expression *new_be
      = new tree_binary_expression (m_lhs ? m_lhs->dup (scope) : nullptr,
                                    m_rhs ? m_rhs->dup (scope) : nullptr,
                                    line (), column (), m_etype);

    new_be->copy_base (*this);

    return new_be;
  }

  // Boolean expressions.

  std::string
  tree_boolean_expression::oper (void) const
  {
    std::string retval = "<unknown>";

    switch (m_etype)
      {
      case bool_and:
        retval = "&&";
        break;

      case bool_or:
        retval = "||";
        break;

      default:
        break;
      }

    return retval;
  }

  tree_expression *
  tree_boolean_expression::dup (symbol_scope& scope) const
  {
    tree_boolean_expression *new_be
      = new tree_boolean_expression (m_lhs ? m_lhs->dup (scope) : nullptr,
                                     m_rhs ? m_rhs->dup (scope) : nullptr,
                                     line (), column (), m_etype);

    new_be->copy_base (*this);

    return new_be;
  }
}
