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
#include "interpreter-private.h"
#include "oct-lvalue.h"
#include "parse.h"
#include "pt-const.h"
#include "pt-id.h"
#include "symscope.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Symbols from the symbol table.

void
tree_identifier::eval_undefined_error (void)
{
  int l = line ();
  int c = column ();

  std::string msg = "'" + name () + "' undefined";

  if (l > 0)
    {
      msg += " near line " + std::to_string (l);

      if (c > 0)
        msg += ", column " + std::to_string (c);
    }

  std::string missing_msg = maybe_missing_function_hook (name ());

  if (! missing_msg.empty ())
    msg += "\n\n" + missing_msg;

  error_with_id ("Octave:undefined-function", "%s", msg.c_str ());
}

octave_lvalue
tree_identifier::lvalue (tree_evaluator& tw)
{
  if (m_sym.is_added_static ())
    static_workspace_error ();

  return octave_lvalue (m_sym, tw.get_current_stack_frame ());
}

tree_identifier *
tree_identifier::dup (symbol_scope& scope) const
{
  // The new tree_identifier object contains a symbol_record
  // entry from the duplicated scope.

  symbol_record new_sym = scope.find_symbol (name ());

  tree_identifier *new_id
    = new tree_identifier (new_sym, line (), column ());

  new_id->copy_base (*this);

  return new_id;
}

octave_value_list
tree_identifier::evaluate_n (tree_evaluator& tw, int nargout)
{
  octave_value_list retval;

  octave_value val = tw.varval (m_sym);

  if (val.is_undefined ())
    {
      interpreter& interp = tw.get_interpreter ();

      symbol_table& symtab = interp.get_symbol_table ();

      val = symtab.find_function (m_sym.name ());
    }

  if (val.is_defined ())
    {
      // GAGME -- this would be cleaner if we required
      // parens to indicate function calls.
      //
      // If this identifier refers to a function, we need to know
      // whether it is indexed so that we can do the same thing
      // for 'f' and 'f()'.  If the index is present and the function
      // object declares it can handle it, return the function object
      // and let tree_index_expression::rvalue handle indexing.
      // Otherwise, arrange to call the function here, so that we don't
      // return the function definition as a value.

      octave_function *fcn = nullptr;

      if (val.is_function ())
        fcn = val.function_value (true);

      if (fcn && ! (is_postfix_indexed ()
                    && fcn->accepts_postfix_index (postfix_index ())))
        {
          retval = fcn->call (tw, nargout);
        }
      else
        {
          if (print_result () && nargout == 0
              && tw.statement_printing_enabled ())
            {
              octave_value_list args = ovl (val);
              args.stash_name_tags (string_vector (name ()));
              feval ("display", args);
            }

          retval = ovl (val);
        }
    }
  else if (m_sym.is_added_static ())
    static_workspace_error ();
  else
    eval_undefined_error ();

  return retval;
}

octave_lvalue
tree_black_hole::lvalue (tree_evaluator& tw)
{
  octave_lvalue retval (m_sym, tw.get_current_stack_frame ());

  retval.mark_black_hole ();

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
