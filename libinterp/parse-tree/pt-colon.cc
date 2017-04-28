/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "ovl.h"
#include "pager.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-colon.h"
#include "pt-walk.h"

namespace octave
{
  // Colon expressions.

  tree_colon_expression *
  tree_colon_expression::append (tree_expression *t)
  {
    tree_colon_expression *retval = nullptr;

    if (! op_base)
      error ("invalid colon expression");

    if (op_limit)
      {
        if (op_increment)
          error ("invalid colon expression");

        // Stupid syntax:
        //
        // base : limit
        // base : increment : limit

        op_increment = op_limit;
        op_limit = t;
      }
    else
      op_limit = t;

    retval = this;

    return retval;
  }

  void
  tree_colon_expression::eval_error (const std::string& s) const
  {
    error ("%s", s.c_str ());
  }

  int
  tree_colon_expression::line (void) const
  {
    return (op_base ? op_base->line ()
            : (op_increment ? op_increment->line ()
               : (op_limit ? op_limit->line ()
                  : -1)));
  }

  int
  tree_colon_expression::column (void) const
  {
    return (op_base ? op_base->column ()
            : (op_increment ? op_increment->column ()
               : (op_limit ? op_limit->column ()
                  : -1)));
  }

  tree_expression *
  tree_colon_expression::dup (symbol_table::scope_id scope,
                              symbol_table::context_id context) const
  {
    tree_colon_expression *new_ce = new
      tree_colon_expression (op_base ? op_base->dup (scope, context) : 0,
                             op_limit ? op_limit->dup (scope, context) : 0,
                             op_increment ? op_increment->dup (scope, context)
                             : 0,
                             line (), column ());

    new_ce->copy_base (*new_ce);

    return new_ce;
  }
}
