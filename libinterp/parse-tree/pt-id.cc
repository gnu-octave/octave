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
#include "interpreter-private.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "pager.h"
#include "parse.h"
#include "pt-bp.h"
#include "pt-const.h"
#include "pt-eval.h"
#include "pt-id.h"
#include "pt-walk.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

namespace octave
{
  // Symbols from the symbol table.

  void
  tree_identifier::eval_undefined_error (void)
  {
    int l = line ();
    int c = column ();

    maybe_missing_function_hook (name ());

    if (l == -1 && c == -1)
      error_with_id ("Octave:undefined-function",
                     "'%s' undefined", name ().c_str ());
    else
      error_with_id ("Octave:undefined-function",
                     "'%s' undefined near line %d column %d",
                     name ().c_str (), l, c);
  }

  octave_lvalue
  tree_identifier::lvalue (tree_evaluator *)
  {
    if (sym->is_added_static ())
      static_workspace_error ();

    return octave_lvalue (sym);
  }

  tree_identifier *
  tree_identifier::dup (symbol_table::scope& scope) const
  {
    // The new tree_identifier object contains a symbol_record
    // entry from the duplicated scope.

    symbol_table::symbol_record new_sym = scope.find_symbol (name ());

    tree_identifier *new_id
      = new tree_identifier (new_sym, line (), column ());

    new_id->copy_base (*this);

    return new_id;
  }
}
