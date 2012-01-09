/*

Copyright (C) 1996-2012 John W. Eaton

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

#include "error.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "pager.h"
#include "pt-bp.h"
#include "pt-const.h"
#include "pt-id.h"
#include "pt-walk.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// Symbols from the symbol table.

void
tree_identifier::eval_undefined_error (void)
{
  int l = line ();
  int c = column ();

  maybe_missing_function_hook (name ());
  if (error_state)
    return;

  if (l == -1 && c == -1)
    ::error ("`%s' undefined", name ().c_str ());
  else
    ::error ("`%s' undefined near line %d column %d",
             name ().c_str (), l, c);
}

octave_value_list
tree_identifier::rvalue (int nargout)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  octave_value val = xsym ().find ();

  if (val.is_defined ())
    {
      // GAGME -- this would be cleaner if we required
      // parens to indicate function calls.
      //
      // If this identifier refers to a function, we need to know
      // whether it is indexed so that we can do the same thing
      // for `f' and `f()'.  If the index is present, return the
      // function object and let tree_index_expression::rvalue
      // handle indexing.  Otherwise, arrange to call the function
      // here, so that we don't return the function definition as
      // a value.

      if (val.is_function () && ! is_postfix_indexed ())
        {
          octave_value_list tmp_args;

          retval = val.do_multi_index_op (nargout, tmp_args);
        }
      else
        {
          if (print_result () && nargout == 0)
            val.print_with_name (octave_stdout, name ());

          retval = val;
        }
    }
  else
    eval_undefined_error ();

  return retval;
}

octave_value
tree_identifier::rvalue1 (int nargout)
{
  octave_value retval;

  octave_value_list tmp = rvalue (nargout);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_lvalue
tree_identifier::lvalue (void)
{
  return octave_lvalue (&(xsym().varref ()));
}

tree_identifier *
tree_identifier::dup (symbol_table::scope_id sc,
                      symbol_table::context_id) const
{
  // The new tree_identifier object contains a symbol_record
  // entry from the duplicated scope.

  // FIXME -- is this the best way?
  symbol_table::symbol_record new_sym
    = symbol_table::find_symbol (name (), sc);

  tree_identifier *new_id
    = new tree_identifier (new_sym, line (), column ());

  new_id->copy_base (*this);

  return new_id;
}

void
tree_identifier::accept (tree_walker& tw)
{
  tw.visit_identifier (*this);
}
