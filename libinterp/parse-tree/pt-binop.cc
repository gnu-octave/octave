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
#include "interpreter.h"
#include "ov.h"
#include "profiler.h"
#include "pt-binop.h"
#include "pt-eval.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Binary expressions.

void
tree_binary_expression::matlab_style_short_circuit_warning (const char *op)
{
  warning_with_id ("Octave:possible-matlab-short-circuit-operator",
                   "Matlab-style short-circuit operation performed for operator %s",
                   op);
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

octave_value
tree_binary_expression::evaluate (tree_evaluator& tw, int)
{
  if (m_lhs)
    {
      octave_value a = m_lhs->evaluate (tw);

      if (a.is_defined () && m_rhs)
        {
          octave_value b = m_rhs->evaluate (tw);

          if (b.is_defined ())
            {
              profiler::enter<tree_binary_expression>
              block (tw.get_profiler (), *this);

              // Note: The profiler does not catch the braindead
              // short-circuit evaluation code above, but that should be
              // ok.  The evaluation of operands and the operator itself
              // is entangled and it's not clear where to start/stop
              // timing the operator to make it reasonable.

              interpreter& interp = tw.get_interpreter ();

              type_info& ti = interp.get_type_info ();

              return binary_op (ti, m_etype, a, b);
            }
        }
    }

  return octave_value ();
}

tree_expression *
tree_braindead_shortcircuit_binary_expression::dup (symbol_scope& scope) const
{
  tree_braindead_shortcircuit_binary_expression *new_be
    = new tree_braindead_shortcircuit_binary_expression
  (m_lhs ? m_lhs->dup (scope) : nullptr,
   m_rhs ? m_rhs->dup (scope) : nullptr,
   line (), column (), op_type ());

  new_be->copy_base (*this);

  return new_be;
}

octave_value
tree_braindead_shortcircuit_binary_expression::evaluate (tree_evaluator& tw,
    int)
{
  if (m_lhs)
    {
      octave_value a = m_lhs->evaluate (tw);

      if (a.ndims () == 2 && a.rows () == 1 && a.columns () == 1)
        {
          bool result = false;

          bool a_true = a.is_true ();

          octave_value::binary_op oper_type = op_type ();

          if (a_true)
            {
              if (oper_type == octave_value::op_el_or)
                {
                  matlab_style_short_circuit_warning ("|");
                  return octave_value (true);
                }
            }
          else
            {
              if (oper_type == octave_value::op_el_and)
                {
                  matlab_style_short_circuit_warning ("&");
                  return octave_value (false);
                }
            }

          if (m_rhs)
            {
              octave_value b = m_rhs->evaluate (tw);

              result = b.is_true ();
            }

          return octave_value (result);
        }
      else
        return tree_binary_expression::evaluate (tw);
    }

  return octave_value ();
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

octave_value
tree_boolean_expression::evaluate (tree_evaluator& tw, int)
{
  octave_value val;

  bool result = false;

  // This evaluation is not caught by the profiler, since we can't find
  // a reasonable place where to time.  Note that we don't want to
  // include evaluation of LHS or RHS into the timing, but this is
  // entangled together with short-circuit evaluation here.

  if (m_lhs)
    {
      octave_value a = m_lhs->evaluate (tw);

      bool a_true = a.is_true ();

      if (a_true)
        {
          if (m_etype == tree_boolean_expression::bool_or)
            return octave_value (true);
        }
      else
        {
          if (m_etype == tree_boolean_expression::bool_and)
            return octave_value (false);
        }

      if (m_rhs)
        {
          octave_value b = m_rhs->evaluate (tw);

          result = b.is_true ();
        }

      val = octave_value (result);
    }

  return val;
}

OCTAVE_END_NAMESPACE(octave)
