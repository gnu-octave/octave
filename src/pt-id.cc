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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"
#include "oct-fcn.h"
#include "oct-sym.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-id.h"
#include "pt-walk.h"
#include "symtab.h"
#include "utils.h"

// Symbols from the symbol table.

string
tree_identifier::name (void) const
{
  string retval;
  if (sym)
    retval = sym->name ();
  return retval;
}

tree_identifier *
tree_identifier::define (octave_symbol *s, unsigned int sym_type)
{
  int status = sym->define (s, sym_type);
  return status ? this : 0;
}

tree_identifier *
tree_identifier::define (octave_function *f, unsigned int sym_type)
{
  int status = sym->define (f, sym_type);
  return status ? this : 0;
}

tree_identifier *
tree_identifier::define (const octave_value& v)
{
  int status = sym->define (v);
  return status ? this : 0;
}

void
tree_identifier::document (const string& s)
{
  if (sym)
    sym->document (s);
}

octave_value
tree_identifier::assign (octave_value::assign_op op, const octave_value& rhs)
{
  octave_value retval;

  if (rhs.is_defined ())
    {
      if (! sym->is_defined ())
	{
	  if (! (sym->is_formal_parameter ()
		 || sym->is_linked_to_global ()))
	    {
	      link_to_builtin_variable (sym);
	    }
	}
      else if (sym->is_function ())
	{
	  sym->clear ();
	}

      // XXX FIXME XXX -- make this work for ops other than `='.

      if (sym->define (rhs))
	retval = rhs;
    }

  return retval;
}

octave_value
tree_identifier::assign (octave_value::assign_op op,
			 const octave_value_list& args,
			 const octave_value& rhs)
{
  octave_value retval;

  if (rhs.is_defined ())
    {
      if (! sym->is_defined ())
	{
	  if (! (sym->is_formal_parameter ()
		 || sym->is_linked_to_global ()))
	    {
	      link_to_builtin_variable (sym);
	    }
	}
      else if (sym->is_function ())
	{
	  sym->clear ();
	}

      if (sym->is_variable () && sym->is_defined ())
	{
	  sym->variable_reference () . assign (op, args, rhs);
	}
      else
	{
	  assert (! sym->is_defined ());

	  if (! Vresize_on_range_error)
	    {
	      ::error ("indexed assignment to previously undefined variables");
	      ::error ("is only possible when resize_on_range_error is true");
	    }
	  else
	    {
	      retval.assign (op, args, rhs);

	      if (retval.is_defined ())
		sym->define (retval);
	    }
	}
    }

  return retval;
}

bool
tree_identifier::is_defined (void)
{
  return (sym && sym->is_defined ());
}

void
tree_identifier::increment (void)
{
  if (sym)
    {
      if (sym->is_read_only ())
	::error ("can't redefined read-only variable `%s'", name ().c_str ());
      else if (sym->is_defined () && sym->is_variable ())
	reference () . increment ();
      else
	::error ("can only increment variables");
    }
}

void
tree_identifier::decrement (void)
{
  if (sym)
    {
      if (sym->is_read_only ())
	::error ("can't redefined read-only variable `%s'", name ().c_str ());
      else if (sym->is_defined () && sym->is_variable ())
	reference () . decrement ();
      else
	::error ("can only decrement variables");
    }
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
//   * On systems that support dynamic linking, we prefer .oct files
//     over .m files.

octave_symbol *
tree_identifier::do_lookup (bool& script_file_executed, bool exec_script)
{
  script_file_executed = lookup (sym, exec_script);

  octave_symbol *retval = 0;

  if (! script_file_executed)
    retval = sym->def ();

  return retval;
}

void
tree_identifier::link_to_global (void)
{
  if (sym)
    link_to_global_variable (sym);
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

octave_value
tree_identifier::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool script_file_executed = false;

  octave_symbol *object_to_eval = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (object_to_eval)
	{
	  int nargout = maybe_do_ans_assign ? 0 : 1;

	  if (nargout)
	    {
	      octave_value_list tmp_args;
	      octave_value_list tmp = object_to_eval->eval (nargout, tmp_args);

	      if (tmp.length () > 0)
		retval = tmp(0);
	    }
	  else
	    retval = object_to_eval->eval ();
	}
      else
	eval_undefined_error ();
    }

  if (! error_state)
    {
      if (retval.is_defined ())
	{
	  if (maybe_do_ans_assign && ! object_to_eval->is_constant ())
	    bind_ans (retval, print);
	  else if (print)
	    retval.print_with_name (octave_stdout, name ());
	}
      else if (object_to_eval && object_to_eval->is_constant ())
	eval_undefined_error ();
    }

  return retval;
}

octave_value_list
tree_identifier::eval (bool print, int nargout, const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  bool script_file_executed = false;

  octave_symbol *object_to_eval = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (object_to_eval)
	{
	  if (maybe_do_ans_assign && nargout == 1)
	    {
	      // Don't count the output arguments that we create
	      // automatically.

	      nargout = 0;

	      retval = object_to_eval->eval (nargout, args);

	      if (retval.length () > 0 && retval(0).is_defined ())
		bind_ans (retval(0), print);
	    }
	  else
	    retval = object_to_eval->eval (nargout, args);
	}
      else
	eval_undefined_error ();
    }

  return retval;
}

void
tree_identifier::accept (tree_walker& tw)
{
  tw.visit_identifier (*this);
}

octave_value
tree_identifier::value (void) const
{
  return sym->variable_value ();
}

octave_value&
tree_identifier::reference (void)
{
  return sym->variable_reference ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
