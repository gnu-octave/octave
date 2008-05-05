/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2004, 2005, 2006, 2007
              John W. Eaton

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

#include "pt-cmd.h"
#include "pt-walk.h"

// No-op.

tree_command *
tree_no_op_command::dup (symbol_table::scope_id)
{
  return new tree_no_op_command (orig_cmd, line (), column ());
}

void
tree_no_op_command::accept (tree_walker& tw)
{
  tw.visit_no_op_command (*this);
}

// Function definition.

void
tree_function_def::eval (void)
{
  if (symbol_table::at_top_level ())
    {
      octave_function *f = function ();

      if (f)
	{
	  std::string nm = f->name ();

	  symbol_table::install_cmdline_function (nm, fcn);

	  // Make sure that any variable with the same name as the new
	  // function is cleared.

	  symbol_table::varref (nm) = octave_value ();
	}
    }
}

tree_command *
tree_function_def::dup (symbol_table::scope_id)
{
  return new tree_function_def (fcn, line (), column ());
}

void
tree_function_def::accept (tree_walker& tw)
{
  tw.visit_function_def (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
