////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2023 The Octave Project Developers
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

#include "Cell.h"
#include "ovl.h"
#include "pt-arg-list.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-cell.h"
#include "pt-walk.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

tree_expression *
tree_cell::dup (symbol_scope& scope) const
{
  tree_cell *new_cell = new tree_cell (nullptr, line (), column ());

  new_cell->copy_base (*this, scope);

  return new_cell;
}

octave_value
tree_cell::evaluate (tree_evaluator& tw, int)
{
  unwind_action act ([&tw] (const std::list<octave_lvalue> *lvl)
  {
    tw.set_lvalue_list (lvl);
  }, tw.lvalue_list ());
  tw.set_lvalue_list (nullptr);

  octave_idx_type nr = length ();
  octave_idx_type nc = -1;

  Cell val;

  octave_idx_type i = 0;

  for (tree_argument_list *elt : *this)
    {
      octave_value_list row = tw.convert_to_const_vector (elt);

      if (nr == 1)
        // Optimize the single row case.
        val = row.cell_value ();
      else if (nc < 0)
        {
          nc = row.length ();

          val = Cell (nr, nc);
        }
      else
        {
          octave_idx_type this_nc = row.length ();

          if (this_nc != nc)
            {
              if (this_nc == 0)
                continue;  // blank line
              else
                error ("number of columns must match");
            }
        }

      for (octave_idx_type j = 0; j < nc; j++)
        val(i, j) = row(j);

      i++;
    }

  if (i < nr)
    val.resize (dim_vector (i, nc));  // there were blank rows

  return octave_value (val);
}

OCTAVE_END_NAMESPACE(octave)
