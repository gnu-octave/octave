////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include "error.h"
#include "input.h"
#include "ov-usr-fcn.h"
#include "pt-all.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
tree_checker::visit_argument_list (tree_argument_list& lst)
{
  auto p = lst.begin ();

  while (p != lst.end ())
    {
      tree_expression *elt = *p++;

      if (elt)
        {
          if (m_do_lvalue_check && ! elt->lvalue_ok ())
            errmsg ("invalid lvalue in multiple assignment", elt->line ());
        }
    }
}

void
tree_checker::visit_simple_for_command (tree_simple_for_command& cmd)
{
  tree_expression *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
        errmsg ("invalid lvalue in for command", cmd.line ());
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_expression *maxproc = cmd.maxproc_expr ();

  if (maxproc)
    maxproc->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_complex_for_command (tree_complex_for_command& cmd)
{
  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      int len = lhs->length ();

      if (len == 0 || len > 2)
        errmsg ("invalid number of output arguments in for command",
                cmd.line ());

      m_do_lvalue_check = true;

      lhs->accept (*this);

      m_do_lvalue_check = false;
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_multi_assignment (tree_multi_assignment& expr)
{
  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    {
      m_do_lvalue_check = true;

      lhs->accept (*this);

      m_do_lvalue_check = false;
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_simple_assignment (tree_simple_assignment& expr)
{
  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
        errmsg ("invalid lvalue in assignment", expr.line ());
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  tree_identifier *expr_id = cmd.identifier ();

  if (expr_id)
    {
      if (! expr_id->lvalue_ok ())
        errmsg ("invalid lvalue used for identifier in try-catch command",
                cmd.line ());
    }

  if (try_code)
    try_code->accept (*this);

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    catch_code->accept (*this);
}

void
tree_checker::errmsg (const std::string& msg, int line)
{
  if (m_file_name.empty ())
    error ("%s", msg.c_str ());
  else
    error ("%s: %d: %s", m_file_name.c_str (), line, msg.c_str ());
}

OCTAVE_END_NAMESPACE(octave)
