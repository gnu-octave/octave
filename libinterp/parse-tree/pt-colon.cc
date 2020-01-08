////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
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
#include "parse.h"
#include "pt-colon.h"
#include "pt-eval.h"

namespace octave
{
  // Colon expressions.

  tree_expression *
  tree_colon_expression::dup (symbol_scope& scope) const
  {
    tree_colon_expression *new_ce
      = new tree_colon_expression (m_base ? m_base->dup (scope) : nullptr,
                                   m_limit ? m_limit->dup (scope) : nullptr,
                                   m_increment ? m_increment->dup (scope) : nullptr,
                                   line (), column ());

    new_ce->copy_base (*this);

    return new_ce;
  }

  octave_value tree_colon_expression::evaluate (tree_evaluator& tw, int)
  {
    octave_value val;

    if (! m_base || ! m_limit)
      return val;

    octave_value ov_base = m_base->evaluate (tw);

    octave_value ov_limit = m_limit->evaluate (tw);

    if (ov_base.isobject () || ov_limit.isobject ())
      {
        octave_value_list tmp1;

        if (m_increment)
          {
            octave_value ov_increment = m_increment->evaluate (tw);

            tmp1(2) = ov_limit;
            tmp1(1) = ov_increment;
            tmp1(0) = ov_base;
          }
        else
          {
            tmp1(1) = ov_limit;
            tmp1(0) = ov_base;
          }

        interpreter& interp = tw.get_interpreter ();

        symbol_table& symtab = interp.get_symbol_table ();

        octave_value fcn = symtab.find_function ("colon", tmp1);

        if (! fcn.is_defined ())
          error ("can not find overloaded colon function");

        octave_value_list tmp2 = feval (fcn, tmp1, 1);

        val = tmp2 (0);
      }
    else
      {
        octave_value ov_increment = 1.0;

        if (m_increment)
          ov_increment = m_increment->evaluate (tw);

        val = do_colon_op (ov_base, ov_increment, ov_limit,
                           is_for_cmd_expr ());
      }

    return val;
  }
}
