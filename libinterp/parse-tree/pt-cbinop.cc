////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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
#include "pt-cbinop.h"
#include "pt-eval.h"
#include "pt-unop.h"

OCTAVE_BEGIN_NAMESPACE(octave)

octave_value
tree_compound_binary_expression::evaluate (tree_evaluator& tw, int)
{
  octave_value val;

  if (m_lhs)
    {
      octave_value a = m_lhs->evaluate (tw);

      if (a.is_defined () && m_rhs)
        {
          octave_value b = m_rhs->evaluate (tw);

          if (b.is_defined ())
            {
              interpreter& interp = tw.get_interpreter ();

              type_info& ti = interp.get_type_info ();

              val = binary_op (ti, m_etype, a, b);
            }
        }
    }

  return val;
}

typedef tree_expression *tree_expression_ptr_t;

// If a tree expression is a transpose or hermitian transpose, return
// the argument and corresponding operator.

static octave_value::unary_op
strip_trans_herm (tree_expression_ptr_t& exp)
{
  if (exp->is_unary_expression ())
    {
      tree_unary_expression *uexp
        = dynamic_cast<tree_unary_expression *> (exp);

      octave_value::unary_op op = uexp->op_type ();

      if (op == octave_value::op_transpose
          || op == octave_value::op_hermitian)
        exp = uexp->operand ();
      else
        op = octave_value::unknown_unary_op;

      return op;
    }
  else
    return octave_value::unknown_unary_op;
}

#if 0
// Restore this code if short-circuit behavior can be preserved when needed.
// See bug #54465.

static octave_value::unary_op
strip_not (tree_expression_ptr_t& exp)
{
  if (exp->is_unary_expression ())
    {
      tree_unary_expression *uexp
        = dynamic_cast<tree_unary_expression *> (exp);

      octave_value::unary_op op = uexp->op_type ();

      if (op == octave_value::op_not)
        exp = uexp->operand ();
      else
        op = octave_value::unknown_unary_op;

      return op;
    }
  else
    return octave_value::unknown_unary_op;
}
#endif

// Possibly convert multiplication to trans_mul, mul_trans, herm_mul,
// or mul_herm.

static octave_value::compound_binary_op
simplify_mul_op (tree_expression_ptr_t& a, tree_expression_ptr_t& b)
{
  octave_value::compound_binary_op retop
    = octave_value::unknown_compound_binary_op;

  octave_value::unary_op opa = strip_trans_herm (a);

  if (opa == octave_value::op_hermitian)
    retop = octave_value::op_herm_mul;
  else if (opa == octave_value::op_transpose)
    retop = octave_value::op_trans_mul;
  else
    {
      octave_value::unary_op opb = strip_trans_herm (b);

      if (opb == octave_value::op_hermitian)
        retop = octave_value::op_mul_herm;
      else if (opb == octave_value::op_transpose)
        retop = octave_value::op_mul_trans;
    }

  return retop;
}

// Possibly convert left division to trans_ldiv or herm_ldiv.

static octave_value::compound_binary_op
simplify_ldiv_op (tree_expression_ptr_t& a, tree_expression_ptr_t&)
{
  octave_value::compound_binary_op retop
    = octave_value::unknown_compound_binary_op;

  octave_value::unary_op opa = strip_trans_herm (a);

  if (opa == octave_value::op_hermitian)
    retop = octave_value::op_herm_ldiv;
  else if (opa == octave_value::op_transpose)
    retop = octave_value::op_trans_ldiv;

  return retop;
}

// Possibly contract and/or with negation.

#if 0
// Restore this code if short-circuit behavior can be preserved when needed.
// See bug #54465.
static octave_value::compound_binary_op
simplify_and_or_op (tree_expression_ptr_t& a, tree_expression_ptr_t& b,
                    octave_value::binary_op op)
{
  octave_value::compound_binary_op retop
    = octave_value::unknown_compound_binary_op;

  octave_value::unary_op opa = strip_not (a);

  if (opa == octave_value::op_not)
    {
      if (op == octave_value::op_el_and)
        retop = octave_value::op_el_not_and;
      else if (op == octave_value::op_el_or)
        retop = octave_value::op_el_not_or;
    }
  else
    {
      octave_value::unary_op opb = strip_not (b);

      if (opb == octave_value::op_not)
        {
          if (op == octave_value::op_el_and)
            retop = octave_value::op_el_and_not;
          else if (op == octave_value::op_el_or)
            retop = octave_value::op_el_or_not;
        }
    }

  return retop;
}
#endif

tree_binary_expression *
maybe_compound_binary_expression (tree_expression *a, tree_expression *b,
                                  int l, int c, octave_value::binary_op t)
{
  tree_expression *ca = a;
  tree_expression *cb = b;
  octave_value::compound_binary_op ct;

  switch (t)
    {
    case octave_value::op_mul:
      ct = simplify_mul_op (ca, cb);
      break;

    case octave_value::op_ldiv:
      ct = simplify_ldiv_op (ca, cb);
      break;

#if 0
    // Restore this case if short-circuit behavior can be preserved
    // when needed.  See bug #54465.
    case octave_value::op_el_and:
    case octave_value::op_el_or:
      ct = simplify_and_or_op (ca, cb, t);
      break;
#endif

    default:
      ct = octave_value::unknown_compound_binary_op;
      break;
    }

  tree_binary_expression *ret
    = (ct == octave_value::unknown_compound_binary_op
       ? new tree_binary_expression (a, b, l, c, t)
       : new tree_compound_binary_expression (a, b, l, c, t, ca, cb, ct));

  return ret;
}

OCTAVE_END_NAMESPACE(octave)
