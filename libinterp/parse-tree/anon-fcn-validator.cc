////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "anon-fcn-validator.h"
#include "ov.h"
#include "pt-all.h"

OCTAVE_BEGIN_NAMESPACE(octave)

anon_fcn_validator::anon_fcn_validator (tree_parameter_list *,
                                        tree_expression *expr)
  : m_ok (true), m_line (-1), m_column (-1), m_message ()
{
  expr->accept (*this);
}

void anon_fcn_validator::visit_postfix_expression (tree_postfix_expression& expr)
{
  octave_value::unary_op op = expr.op_type ();

  if (op == octave_value::op_incr || op == octave_value::op_decr)
    error (expr);
  else
    tree_walker::visit_postfix_expression (expr);
}

void anon_fcn_validator::visit_prefix_expression (tree_prefix_expression& expr)
{
  octave_value::unary_op op = expr.op_type ();

  if (op == octave_value::op_incr || op == octave_value::op_decr)
    error (expr);
  else
    tree_walker::visit_prefix_expression (expr);
}

void anon_fcn_validator::visit_multi_assignment (tree_multi_assignment& expr)
{
  error (expr);
}

void anon_fcn_validator::visit_simple_assignment (tree_simple_assignment& expr)
{
  error (expr);
}

void anon_fcn_validator::error (tree_expression& expr)
{
  m_ok = false;
  m_line = expr.line ();
  m_column = expr.column ();
  m_message
    = "invalid use of operator " + expr.oper () + " in anonymous function";
}

OCTAVE_END_NAMESPACE(octave)
