/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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

std::string
tree_identifier::name (void) const
{
  std::string retval;
  if (sym)
    retval = sym->name ();
  return retval;
}

tree_identifier *
tree_identifier::define (octave_function *f, unsigned int sym_type)
{
  int status = sym->define (f, sym_type);
  return status ? this : 0;
}

void
tree_identifier::document (const std::string& s)
{
  if (sym)
    sym->document (s);
}

bool
tree_identifier::is_defined (void)
{
  return (sym && sym->is_defined ());
}

bool
tree_identifier::is_function (void)
{
  return (sym && sym->is_function ());
}

void
tree_identifier::eval_undefined_error (void)
{
  int l = line ();
  int c = column ();

  if (l == -1 && c == -1)
    ::error ("`%s' undefined", name ().c_str ());
  else
    ::error ("`%s' undefined near line %d column %d",
	     name ().c_str (), l, c);
}

// Try to find a definition for an identifier.  Here's how:
//
//   * If the identifier is already defined and is a function defined
//     in an function file that has been modified since the last time 
//     we parsed it, parse it again.
//
//   * If the identifier is not defined, try to find a builtin
//     variable or an already compiled function with the same name.
//
//   * If the identifier is still undefined, try looking for an
//     function file to parse.
//
//   * On systems that support dynamic linking, we prefer .oct files,
//     then .mex files, then .m files.

octave_value
tree_identifier::do_lookup (bool& script_file_executed, bool exec_script)
{
  static octave_value foo;

  script_file_executed = lookup (sym, exec_script);

  return script_file_executed ? foo : sym->def ();
}

void
tree_identifier::link_to_global (void)
{
  if (sym)
    {
      if (! sym->is_linked_to_global ())
	{
	  if (sym->is_defined () && sym->is_variable ())
	    warning ("local variable value may have changed to match global");

	  link_to_global_variable (sym);
	}
    }
}

void
tree_identifier::mark_as_static (void)
{
  if (sym)
    sym->mark_as_static ();
}

void
tree_identifier::mark_as_formal_parameter (void)
{
  if (sym)
    sym->mark_as_formal_parameter ();
}

octave_value_list
tree_identifier::rvalue (int nargout)
{
  octave_value_list retval;

  MAYBE_DO_BREAKPOINT;

  if (error_state)
    return retval;

  bool script_file_executed = false;

  octave_value val = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
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
    }

  return retval;
}

octave_value
tree_identifier::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_lvalue
tree_identifier::lvalue (void)
{
  MAYBE_DO_BREAKPOINT;

  return sym->variable_reference ();
}

tree_identifier *
tree_identifier::dup (symbol_table *sym_tab)
{
  symbol_record *sr = (sym_tab && sym) ? sym_tab->lookup (sym->name ()) : 0;

  tree_identifier *new_id = new tree_identifier (sr, line (), column ());

  new_id->copy_base (*this);

  return new_id;
}

void
tree_identifier::accept (tree_walker& tw)
{
  tw.visit_identifier (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
