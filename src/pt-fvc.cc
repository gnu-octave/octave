// tree-fvc2.cc                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

static int
any_element_less_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) < val)
	return 1;
  return 0;
}

static int
any_element_greater_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) > val)
	return 1;
  return 0;
}

static void
print_constant (tree_constant& tc, char *name, int print_padding = 1)
{
  int pad_after = 0;
  if (user_pref.print_answer_id_name)
    {
      if (print_as_scalar (tc) || print_as_structure (tc))
	{
	  ostrstream output_buf;
	  output_buf << name << " = " << ends;
	  maybe_page_output (output_buf);
	}
      else
	{
	  pad_after = 1;
	  ostrstream output_buf;
	  output_buf << name << " =\n\n" << ends;
	  maybe_page_output (output_buf);
	}
    }

  tc.eval (1);

  if (print_padding && pad_after)
    {
      ostrstream output_buf;
      output_buf << "\n" << ends;
      maybe_page_output (output_buf);
    }
}

// Make sure that all arguments have values.

// Are any of the arguments `:'?

static int
any_arg_is_magic_colon (const Octave_object& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_magic_colon ())
	return 1;

  return 0;
}

// Symbols from the symbol table.

char *
tree_identifier::name (void) const
{
  return sym ? sym->name () : 0;
}

tree_identifier *
tree_identifier::define (tree_constant *t)
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
tree_identifier::document (char *s)
{
  if (sym && s)
    sym->document (strsave (s));
}

tree_constant
tree_identifier::assign (tree_constant& rhs)
{
  tree_constant retval;

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

      tree_constant *tmp = new tree_constant (rhs);

      if (sym->define (tmp))
	retval = rhs;
      else
	delete tmp;
    }

  return retval;
}

tree_constant
tree_identifier::assign (tree_constant& rhs, const Octave_object& args)
{
  tree_constant retval;

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
	      tree_constant *tmp = new tree_constant ();
	      retval = tmp->assign (rhs, args);
	      if (retval.is_defined ())
		sym->define (tmp);
	    }
	}
    }

  return retval;
}

tree_constant
tree_identifier::assign (SLList<char*> list, tree_constant& rhs)
{
  tree_constant retval;

  if (rhs.is_defined ())
    {
      if (sym->is_function ())
	sym->clear ();

      tree_fvc *curr_val = sym->def ();

      tree_constant *tmp = 0;
      if (curr_val && curr_val->is_constant ())
	tmp = (tree_constant *) curr_val;
      else
	{
	  tmp = new tree_constant ();
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

tree_constant
tree_identifier::assign (SLList<char*> list, tree_constant& rhs,
			 const Octave_object& args)
{
  tree_constant retval;

  if (rhs.is_defined ())
    {
      if (sym->is_function ())
	sym->clear ();

      if (sym->is_variable () && sym->is_defined ())
	{
	  tree_fvc *curr_val = sym->def ();

	  tree_constant *tmp;
	  if (curr_val && curr_val->is_constant ())
	    tmp = (tree_constant *) curr_val;
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
	      tree_constant *tmp = new tree_constant ();

	      retval = tmp->assign_map_element (list, rhs, args);

	      if (retval.is_defined ())
		sym->define (tmp);
	    }
	}
    }

  return retval;
}

int
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
	  ::error ("can't redefined read-only variable `%s'", name ());
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
  char *nm = name ();
  int l = line ();
  int c = column ();
  if (l == -1 && c == -1)
    ::error ("`%s' undefined", nm);
  else
    ::error ("`%s' undefined near line %d column %d", nm, l, c);
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
tree_identifier::do_lookup (int& script_file_executed, int exec_script)
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

tree_constant
tree_identifier::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  int script_file_executed = 0;

  tree_fvc *object_to_eval = do_lookup (script_file_executed);

  if (! script_file_executed)
    {
      if (object_to_eval)
	{
	  int nargout = maybe_do_ans_assign ? 0 : 1;

	  if (nargout)
	    {
	      Octave_object tmp_args;
	      Octave_object tmp = object_to_eval->eval (0, nargout, tmp_args);

	      if (tmp.length () > 0)
		retval = tmp(0);
	    }
	  else
	    retval = object_to_eval->eval (0);
	}
      else
	eval_undefined_error ();
    }

  if (! error_state && retval.is_defined ())
    {
      if (maybe_do_ans_assign && ! object_to_eval->is_constant ())
	bind_ans (retval, print);
      else if (print)
	print_constant (retval, name ());
    }

  return retval;
}

Octave_object
tree_identifier::eval (int print, int nargout, const Octave_object& args)
{
  Octave_object retval;

  if (error_state)
    return retval;

  int script_file_executed = 0;

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

  char *nm = name ();
  os << (nm) ? nm : "(null)";

  if (in_parens)
    os << ")";
}

// Indirect references to values (structure elements).

tree_indirect_ref::~tree_indirect_ref (void)
{
  while (! refs.empty ())
    {
      char *t = refs.remove_front ();
      delete [] t;
    }

  if (! preserve_ident)
    delete id;
}

tree_indirect_ref *
tree_indirect_ref::chain (const char *elt)
{
  refs.append (strsave (elt));
  return this;
}

char *
tree_indirect_ref::name (void)
{
  char *id_nm = id->name ();
  if (refs.empty ())
    return id_nm;
  else
    {
      static char *nm = 0;
      delete [] nm;

      ostrstream tmp;

      tmp << id_nm;

      for (Pix p = refs.first (); p != 0; refs.next (p))
	{
	  char *elt = refs (p);

	  if (elt)
	    tmp << "." << elt;
	}

      tmp << ends;
      nm = tmp.str ();
      return nm;
    }
}

tree_constant
tree_indirect_ref::assign (tree_constant& t)
{
  tree_constant retval;

  if (refs.empty ())
    retval = id->assign (t);
  else
    retval = id->assign (refs, t);

  return retval;
}

tree_constant
tree_indirect_ref::assign (tree_constant& t, const Octave_object& args)
{
  tree_constant retval;

  if (refs.empty ())
    retval = id->assign (t, args);
  else
    retval = id->assign (refs, t, args);

  return retval;
}

tree_constant
tree_indirect_ref::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (refs.empty ())
    {
      retval = id->eval (print);
    }
  else
    {
      int script_file_executed;

      tree_fvc *object_to_eval = id->do_lookup (script_file_executed, 0);

      if (object_to_eval)
	{
	  retval = object_to_eval->lookup_map_element (refs);

	  if (! error_state && print)
	    print_constant (retval, name ());
	}
      else
	id->eval_undefined_error ();
    }

  return retval;
}

Octave_object
tree_indirect_ref::eval (int print, int nargout, const Octave_object& args)
{
  Octave_object retval;

  if (error_state)
    return retval;

  if (refs.empty ())
    {
      retval = id->eval (print, nargout, args);
    }
  else
    {
      int script_file_executed;

      tree_fvc *object_to_eval = id->do_lookup (script_file_executed, 0);

      if (object_to_eval)
	{
	  tree_constant tmp = object_to_eval->lookup_map_element (refs);

	  if (! error_state)
	    {
	      retval = tmp.eval (0, nargout, args);

	      if (! error_state && print)
		{
		  tmp = retval (0);
		  if (tmp.is_defined ())
		    print_constant (tmp, name ());
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

  char *nm = id ? id->name () : "(null)";
  os << (nm) ? nm : "(null)";

  for (Pix p = refs.first (); p != 0; refs.next (p))
    {
      char *elt = refs (p);

      if (elt)
	os << "." << elt;
    }

  if (in_parens)
    os << ")";
}

// Builtin functions.

tree_builtin::tree_builtin (const char *nm)
{
  is_mapper = 0;
  fcn = 0;
  if (nm)
    my_name = strsave (nm);
}

tree_builtin::tree_builtin (Mapper_fcn& m_fcn, const char *nm)
{
  mapper_fcn = m_fcn;
  is_mapper = 1;
  fcn = 0;
  my_name = nm ? strsave (nm) : 0;
}

tree_builtin::tree_builtin (Octave_builtin_fcn g_fcn, const char *nm)
{
  is_mapper = 0;
  fcn = g_fcn;
  my_name = nm ? strsave (nm) : 0;
}

tree_constant
tree_builtin::eval (int /* print */)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (fcn)
    {
    eval_fcn:

      Octave_object args;
      Octave_object tmp = (*fcn) (args, 0);
      if (tmp.length () > 0)
	retval = tmp(0);
    }
  else if (is_mapper)
    {
      ::error ("%s: too few arguments", my_name);
    }
  else
    {
      fcn = load_octave_builtin (my_name);

      if (fcn)
	goto eval_fcn;
      else
	::error ("unable to load builtin function %s", my_name);
    }

  return retval;
}

static tree_constant
apply_mapper_fcn (const tree_constant& arg, Mapper_fcn& m_fcn,
		  int /* print */)
{
  tree_constant retval;

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
		error ("%s: unable to handle real arguments", m_fcn.name);
	    }
	  else if (m_fcn.d_d_mapper)
	    retval = m_fcn.d_d_mapper (d);
	  else
	    error ("%s: unable to handle real arguments", m_fcn.name);
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
		error ("%s: unable to handle real arguments", m_fcn.name);
	    }
	  else if (m_fcn.d_d_mapper)
	    retval = map (m_fcn.d_d_mapper, m);
	  else
	    error ("%s: unable to handle real arguments", m_fcn.name);
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
	    error ("%s: unable to handle complex arguments", m_fcn.name);
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
	    error ("%s: unable to handle complex arguments", m_fcn.name);
	}
    }
  else
    gripe_wrong_type_arg ("mapper", arg);

  return retval;
}

Octave_object
tree_builtin::eval (int /* print */, int nargout, const Octave_object& args)
{
  Octave_object retval;

  if (error_state)
    return retval;

  int nargin = args.length ();

  if (fcn)
    {
    eval_fcn:

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
//	::error ("%s: too many arguments", my_name);
//      else
      if (nargin > 0 && args(0).is_defined ())
	{
	  tree_constant tmp = apply_mapper_fcn (args(0), mapper_fcn, 0);
	  retval(0) = tmp;
	}
      else
	{
	  ::error ("%s: too few arguments", my_name);
	}
    }
  else
    {
      fcn = load_octave_builtin (my_name);

      if (fcn)
	goto eval_fcn;
      else
	::error ("unable to load builtin function %s", my_name);
    }

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
