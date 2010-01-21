/*

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-cell.h"
#include "pt-walk.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

octave_value
tree_cell::rvalue1 (int)
{
  octave_value retval;

  octave_idx_type nr = length ();
  octave_idx_type nc = -1;

  Cell val;

  int i = 0;

  for (iterator p = begin (); p != end (); p++)
    {
      tree_argument_list *elt = *p;

      octave_value_list row = elt->convert_to_const_vector ();
      
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

	  if (nc != this_nc)
	    {
	      ::error ("number of columns must match");
	      return retval;
	    }
	}

      for (octave_idx_type j = 0; j < nc; j++)
	val(i,j) = row(j);

      i++;
    }

  retval = val;

  return retval;
}

octave_value_list
tree_cell::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for cell array");
  else
    retval = rvalue1 (nargout);

  return retval;
}

tree_expression *
tree_cell::dup (symbol_table::scope_id scope,
		symbol_table::context_id context) const
{
  tree_cell *new_cell = new tree_cell (0, line (), column ());

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_argument_list *elt = *p;

      new_cell->append (elt ? elt->dup (scope, context) : 0);
    }

  new_cell->copy_base (*this);

  return new_cell;
}

void
tree_cell::accept (tree_walker& tw)
{
  tw.visit_cell (*this);
}
