/*

Copyright (C) 1996 John W. Eaton

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

#include <iostream.h>
#include <strstream.h>

#include <SLList.h>

#include "dynamic-ld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pager.h"
#include "symtab.h"
#include "pt-const.h"
#include "pt-fvc.h"
#include "user-prefs.h"
#include "utils.h"

// But first, some extra functions used by the tree classes.

static bool
any_element_less_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) < val)
	return true;

  return false;
}

static bool
any_element_greater_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) > val)
	return true;

  return false;
}

// Make sure that all arguments have values.

// Are any of the arguments `:'?

static bool
any_arg_is_magic_colon (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_magic_colon ())
	return true;

  return false;
}

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
tree_identifier::define (octave_value *t)
{
  int status = sym->define (t);
  return status ? this : 0;
}

tree_identifier *
tree_identifier::define (tree_function *t)
{
  int status = sym->define (t);
  return status ? this : 0;
}

void
tree_identifier::document (const string& s)
{
  if (sym)
    sym->document (s);
}

octave_value
tree_identifier::assign (octave_value& rhs)
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

      octave_value *tmp = new octave_value (rhs);

      if (sym->define (tmp))
	retval = rhs;
      else
	delete tmp;
    }

  return retval;
}

octave_value
tree_identifier::assign (octave_value& rhs, const octave_value_list& args)
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
	  tree_fvc *tmp = sym->def ();
	  retval = tmp->assign (rhs, args);
	}
      else
	{
	  assert (! sym->is_defined ());

	  if (! user_pref.resize_on_range_error)
	    {
	      ::error ("indexed assignment to previously undefined variables");
	      ::error ("is only possible when resize_on_range_error is true");
	    }
	  else
	    {
	      octave_value *tmp = new octave_value ();
	      retval = tmp->assign (rhs, args);
	      if (retval.is_defined ())
		sym->define (tmp);
	    }
	}
    }

  return retval;
}

octave_value
tree_identifier::assign (SLList<string> list, octave_value& rhs)
{
  octave_value retval;

  if (rhs.is_defined ())
    {
      if (sym->is_function ())
	sym->clear ();

      tree_fvc *curr_val = sym->def ();

      octave_value *tmp = 0;
      if (curr_val && curr_val->is_constant ())
	tmp = (octave_value *) curr_val;
      else
	{
	  tmp = new octave_value ();
	  if (! sym->define (tmp))
	    {
	      delete tmp;
	      tmp = 0;
	    }
	}

      if (tmp)
	retval = tmp->assign_map_element (list, rhs);
    }

  return retval;
}

octave_value
tree_identifier::assign (SLList<string> list, octave_value& rhs,
			 const octave_value_list& args)
{
  octave_value retval;

  if (rhs.is_defined ())
    {
      if (sym->is_function ())
	sym->clear ();

      if (sym->is_variable () && sym->is_defined ())
	{
	  tree_fvc *curr_val = sym->def ();

	  octave_value *tmp;
	  if (curr_val && curr_val->is_constant ())
	    tmp = (octave_value *) curr_val;
	  else
	    panic_impossible ();

	  retval = tmp->assign_map_element (list, rhs, args);
	}
      else
	{
	  assert (! sym->is_defined ());

	  if (! user_pref.resize_on_range_error)
	    {
	      ::error ("indexed assignment to previously undefined variables");
	      ::error ("is only possible when resize_on_range_error is true");
	    }
	  else
	    {
	      octave_value *tmp = new octave_value ();

	      retval = tmp->assign_map_element (list, rhs, args);

	      if (retval.is_defined ())
		sym->define (tmp);
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
tree_identifier::bump_value (tree_expression::type etype)
{
  if (sym)
    {
      if (sym->is_read_only ())
	{
	  ::error ("can't redefined read-only variable `%s'",
		   name ().c_str ());
	}
      else
	{
	  tree_fvc *tmp = sym->def ();
	  if (tmp)
	    tmp->bump_value (etype);
	}
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

tree_fvc *
tree_identifier::do_lookup (bool& script_file_executed, bool exec_script)
{
  script_file_executed = lookup (sym, exec_script);

  tree_fvc *retval = 0;

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

  tree_fvc *object_to_eval = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (object_to_eval)
	{
	  int nargout = maybe_do_ans_assign ? 0 : 1;

	  if (nargout)
	    {
	      octave_value_list tmp_args;
	      octave_value_list tmp = object_to_eval->eval (0, nargout, tmp_args);

	      if (tmp.length () > 0)
		retval = tmp(0);
	    }
	  else
	    retval = object_to_eval->eval (false);
	}
      else
	eval_undefined_error ();
    }

  if (! error_state && retval.is_defined ())
    {
      if (maybe_do_ans_assign && ! object_to_eval->is_constant ())
	bind_ans (retval, print);
      else if (print)
	retval.print_with_name (name ());
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

  tree_fvc *object_to_eval = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (object_to_eval)
	{
	  if (maybe_do_ans_assign && nargout == 1)
	    {

	      // Don't count the output arguments that we create
	      // automatically.

	      nargout = 0;

	      retval = object_to_eval->eval (0, nargout, args);

	      if (retval.length () > 0 && retval(0).is_defined ())
		bind_ans (retval(0), print);
	    }
	  else
	    retval = object_to_eval->eval (print, nargout, args);
	}
      else
	eval_undefined_error ();
    }

  return retval;
}

void
tree_identifier::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  string nm = name ();
  os << (nm.empty () ? string ("(empty)") : nm);

  if (in_parens)
    os << ")";
}

// Indirect references to values (structure elements).

tree_indirect_ref::~tree_indirect_ref (void)
{
  if (! preserve_ident)
    delete id;
}

tree_indirect_ref *
tree_indirect_ref::chain (const string& elt)
{
  refs.append (elt);
  return this;
}

string
tree_indirect_ref::name (void) const
{
  string id_nm = id->name ();

  if (refs.empty ())
    return id_nm;
  else
    {
      for (Pix p = refs.first (); p != 0; refs.next (p))
	{
	  id_nm.append (".");
	  id_nm.append (refs (p));
	}

      return id_nm;
    }
}

octave_value
tree_indirect_ref::assign (octave_value& t)
{
  octave_value retval;

  if (refs.empty ())
    retval = id->assign (t);
  else
    retval = id->assign (refs, t);

  return retval;
}

octave_value
tree_indirect_ref::assign (octave_value& t, const octave_value_list& args)
{
  octave_value retval;

  if (refs.empty ())
    retval = id->assign (t, args);
  else
    retval = id->assign (refs, t, args);

  return retval;
}

octave_value
tree_indirect_ref::eval (bool print)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (refs.empty ())
    {
      retval = id->eval (print);
    }
  else
    {
      bool script_file_executed;

      tree_fvc *object_to_eval = id->do_lookup (script_file_executed, 0);

      if (object_to_eval)
	{
	  retval = object_to_eval->lookup_map_element (refs);

	  if (! error_state && print)
	    retval.print_with_name (name ());
	}
      else
	id->eval_undefined_error ();
    }

  return retval;
}

octave_value_list
tree_indirect_ref::eval (bool print, int nargout, const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (refs.empty ())
    {
      retval = id->eval (print, nargout, args);
    }
  else
    {
      bool script_file_executed;

      tree_fvc *object_to_eval = id->do_lookup (script_file_executed, 0);

      if (object_to_eval)
	{
	  octave_value tmp = object_to_eval->lookup_map_element (refs);

	  if (! error_state)
	    {
	      retval = tmp.eval (0, nargout, args);

	      if (! error_state && print)
		{
		  tmp = retval (0);
		  if (tmp.is_defined ())
		    tmp.print_with_name (name ());
		}
	    }
	}
      else
	id->eval_undefined_error ();
    }

  return retval;
}

void
tree_indirect_ref::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  string nm = id ? id->name () : string ("(null)");
  os << (nm.empty () ? string ("(empty)") : nm);

  for (Pix p = refs.first (); p != 0; refs.next (p))
    os << "." << refs (p);

  if (in_parens)
    os << ")";
}

// Builtin functions.

tree_builtin::tree_builtin (const string& nm)
{
  is_mapper = 0;
  fcn = 0;
  my_name = nm;
}

tree_builtin::tree_builtin (Mapper_fcn& m_fcn, const string &nm)
{
  mapper_fcn = m_fcn;
  is_mapper = 1;
  fcn = 0;
  my_name = nm;
}

tree_builtin::tree_builtin (Octave_builtin_fcn g_fcn, const string& nm)
{
  is_mapper = 0;
  fcn = g_fcn;
  my_name = nm;
}

octave_value
tree_builtin::eval (bool /* print */)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (fcn)
    {
      octave_value_list args;
      octave_value_list tmp = (*fcn) (args, 0);
      if (tmp.length () > 0)
	retval = tmp(0);
    }
  else if (is_mapper)
    {
      ::error ("%s: too few arguments", my_name.c_str ());
    }
  else
    panic_impossible ();

  return retval;
}

static octave_value
apply_mapper_fcn (const octave_value& arg, Mapper_fcn& m_fcn,
		  bool /* print */)
{
  octave_value retval;

  if (arg.is_real_type ())
    {
      if (arg.is_scalar_type ())
	{
	  double d = arg.double_value ();

	  if (m_fcn.can_return_complex_for_real_arg
	      && (d < m_fcn.lower_limit || d > m_fcn.upper_limit))
	    {
	      if (m_fcn.c_c_mapper)
		retval = m_fcn.c_c_mapper (Complex (d));
	      else
		error ("%s: unable to handle real arguments",
		       m_fcn.name.c_str ());
	    }
	  else if (m_fcn.d_d_mapper)
	    retval = m_fcn.d_d_mapper (d);
	  else
	    error ("%s: unable to handle real arguments",
		   m_fcn.name.c_str ());
	}
      else
	{
	  Matrix m = arg.matrix_value ();

	  if (error_state)
	    return retval;

	  if (m_fcn.can_return_complex_for_real_arg
	      && (any_element_less_than (m, m_fcn.lower_limit)
		  || any_element_greater_than (m, m_fcn.upper_limit)))
	    {
	      if (m_fcn.c_c_mapper)
		retval = map (m_fcn.c_c_mapper, ComplexMatrix (m));
	      else
		error ("%s: unable to handle real arguments",
		       m_fcn.name.c_str ());
	    }
	  else if (m_fcn.d_d_mapper)
	    retval = map (m_fcn.d_d_mapper, m);
	  else
	    error ("%s: unable to handle real arguments",
		   m_fcn.name.c_str ());
	}
    }
  else if (arg.is_complex_type ())
    {
      if (arg.is_scalar_type ())
	{
	  Complex c = arg.complex_value ();

	  if (m_fcn.d_c_mapper)
	    retval = m_fcn.d_c_mapper (c);
	  else if (m_fcn.c_c_mapper)
	    retval = m_fcn.c_c_mapper (c);
	  else
	    error ("%s: unable to handle complex arguments",
		   m_fcn.name.c_str ());
	}
      else
	{
	  ComplexMatrix cm = arg.complex_matrix_value ();

	  if (error_state)
	    return retval;

	  if (m_fcn.d_c_mapper)
	    retval = map (m_fcn.d_c_mapper, cm);
	  else if (m_fcn.c_c_mapper)
	    retval = map (m_fcn.c_c_mapper, cm);
	  else
	    error ("%s: unable to handle complex arguments",
		   m_fcn.name.c_str ());
	}
    }
  else
    gripe_wrong_type_arg ("mapper", arg);

  return retval;
}

octave_value_list
tree_builtin::eval (bool /* print */, int nargout, const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  int nargin = args.length ();

  if (fcn)
    {
      if (any_arg_is_magic_colon (args))
	::error ("invalid use of colon in function argument list");
      else
	retval = (*fcn) (args, nargout);
    }
  else if (is_mapper)
    {
// XXX FIXME XXX -- should we just assume nargin_max == 1?
//
//      if (nargin > nargin_max)
//	::error ("%s: too many arguments", my_name.c_str ());
//      else
      if (nargin > 0 && args(0).is_defined ())
	{
	  octave_value tmp = apply_mapper_fcn (args(0), mapper_fcn, 0);
	  retval(0) = tmp;
	}
      else
	{
	  ::error ("%s: too few arguments", my_name.c_str ());
	}
    }
  else
    panic_impossible ();

  return retval;
}

void
tree_builtin::print_code (ostream& os)
{
  os << my_name << " can't be printed because it is a builtin function\n";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
