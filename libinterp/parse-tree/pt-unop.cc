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

#include "interpreter.h"
#include "ov.h"
#include "profiler.h"
#include "pt-unop.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Unary expressions.

std::string
tree_unary_expression::oper (void) const
{
  return octave_value::unary_op_as_string (m_etype);
}

// Prefix expressions.

tree_expression *
tree_prefix_expression::dup (symbol_scope& scope) const
{
  tree_prefix_expression *new_pe
    = new tree_prefix_expression (m_op ? m_op->dup (scope) : nullptr,
                                  line (), column (), m_etype);

  new_pe->copy_base (*this);

  return new_pe;
}

octave_value
tree_prefix_expression::evaluate (tree_evaluator& tw, int)
{
  octave_value val;

  if (m_op)
    {
      if (m_etype == octave_value::op_incr
          || m_etype == octave_value::op_decr)
        {
          octave_lvalue op_ref = m_op->lvalue (tw);

          profiler::enter<tree_prefix_expression>
          block (tw.get_profiler (), *this);

          op_ref.unary_op (m_etype);

          val = op_ref.value ();
        }
      else
        {
          octave_value op_val = m_op->evaluate (tw);

          if (op_val.is_defined ())
            {
              profiler::enter<tree_prefix_expression>
              block (tw.get_profiler (), *this);

              // Attempt to do the operation in-place if it is unshared
              // (a temporary expression).
              if (op_val.get_count () == 1)
                val = op_val.non_const_unary_op (m_etype);
              else
                {
                  interpreter& interp = tw.get_interpreter ();

                  type_info& ti = interp.get_type_info ();

                  val = unary_op (ti, m_etype, op_val);
                }
            }
        }
    }

  return val;
}

// Postfix expressions.

tree_expression *
tree_postfix_expression::dup (symbol_scope& scope) const
{
  tree_postfix_expression *new_pe
    = new tree_postfix_expression (m_op ? m_op->dup (scope) : nullptr,
                                   line (), column (), m_etype);

  new_pe->copy_base (*this);

  return new_pe;
}

octave_value
tree_postfix_expression::evaluate (tree_evaluator& tw, int)
{
  octave_value val;

  if (m_op)
    {
      if (m_etype == octave_value::op_incr
          || m_etype == octave_value::op_decr)
        {
          octave_lvalue ref = m_op->lvalue (tw);

          val = ref.value ();

          profiler::enter<tree_postfix_expression>
          block (tw.get_profiler (), *this);

          ref.unary_op (m_etype);
        }
      else
        {
          octave_value op_val = m_op->evaluate (tw);

          if (op_val.is_defined ())
            {
              profiler::enter<tree_postfix_expression>
              block (tw.get_profiler (), *this);

              interpreter& interp = tw.get_interpreter ();

              type_info& ti = interp.get_type_info ();

              val = unary_op (ti, m_etype, op_val);
            }
        }
    }

  return val;
}

OCTAVE_END_NAMESPACE(octave)
