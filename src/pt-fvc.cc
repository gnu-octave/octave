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
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"
#include "symtab.h"
#include "pt-const.h"
#include "pt-fvc.h"
#include "pt-walk.h"
#include "utils.h"

// But first, some extra functions used by the tree classes.

static bool
any_element_less_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a (i, j) < val)
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
      if (a (i, j) > val)
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
tree_identifier::document (const string& s)
{
  if (sym)
    sym->document (s);
}

octave_value
tree_identifier::assign (const octave_value& rhs)
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

      tree_constant *tmp = new tree_constant (rhs);

      if (sym->define (tmp))
	retval = rhs;
      else
	delete tmp;
    }

  return retval;
}

octave_value
tree_identifier::assign (const octave_value_list& args,
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
	  tree_constant *tmp = (tree_constant *) sym->def ();
	  retval = tmp->assign (args, rhs);
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
	      tree_constant *tmp = new tree_constant ();
	      retval = tmp->assign (args, rhs);
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
tree_identifier::increment (void)
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
	    tmp->increment ();
	}
    }
}

void
tree_identifier::decrement (void)
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
	    tmp->decrement ();
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

  if (! error_state)
    {
      if (retval.is_defined ())
	{
	  if (maybe_do_ans_assign && ! object_to_eval->is_constant ())
	    bind_ans (retval, print);
	  else if (print)
	    retval.print_with_name (name ());
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

// Indirect references to values (structure elements).

tree_indirect_ref::~tree_indirect_ref (void)
{
  if (! preserve_ident)
    delete id;

  if (! preserve_indir)
    delete indir;
}

string
tree_indirect_ref::name (void) const
{
  string retval;

  if (is_identifier_only ())
    retval = id->name ();
  else
    {
      if (id)
	retval = id->name ();
      else if (indir)
	retval = indir->name ();
      else
	panic_impossible ();

      retval.append (".");
      retval.append (nm);
    }
	
  return retval;
}

octave_value
tree_indirect_ref::eval (bool print)
{
  octave_value retval;

  if (is_identifier_only ())
    retval = id->eval (print);
  else
    {
      retval = value ();

      if (! error_state && retval.is_defined ())
	{
	  if (maybe_do_ans_assign)
	    bind_ans (retval, print);
	  else if (print)
	    retval.print_with_name (name ());
	}
    }

  return retval;
}

octave_value_list
tree_indirect_ref::eval (bool print, int nargout,
			 const octave_value_list& args)
{
  octave_value_list retval;

  if (is_identifier_only ())
    retval = id->eval (print, nargout, args);
  else
    {
      octave_value tmp = value ();

      if (! error_state && tmp.is_defined ())
	{
	  retval = tmp.index (args);

	  if (! error_state)
	    {
	      if (maybe_do_ans_assign && nargout == 1
		  && retval.length () > 0 && retval(0).is_defined ())
		bind_ans (retval(0), print);
	    }
	}
    }

  return retval;
}

void
tree_indirect_ref::accept (tree_walker& tw)
{
  tw.visit_indirect_ref (*this);
}

octave_value
tree_indirect_ref::value (void) const
{
  octave_value retval;

  if (is_identifier_only ())
    retval = id->value ();
  else
    {
      if (id)
	retval = id->value ();
      else if (indir)
	retval = indir->value ();
      else
	panic_impossible ();

      if (! error_state)
	retval = retval.struct_elt_val (nm);
    }

  return retval;
}

octave_value&
tree_indirect_ref::reference (void)
{
  if (is_identifier_only ())
    return id->reference ();
  else
    {
      if (id)
	{
	  octave_value& tmp = id->reference ();
	  if (tmp.is_undefined ())
	    tmp = Octave_map ();
	  return tmp.struct_elt_ref (nm);
	}
      else if (indir)
	{
	  octave_value& tmp = indir->reference ();
	  if (tmp.is_undefined ())
	    tmp = Octave_map ();
	  return tmp.struct_elt_ref (nm);
	}
      else
	{
	  static octave_value foo;
	  panic_impossible ();
	  return foo;
	}
    }
}

// Builtin functions.

tree_builtin::tree_builtin (const string& nm)
{
  is_mapper = 0;
  fcn = 0;
  my_name = nm;
}

tree_builtin::tree_builtin (const builtin_mapper_function& m_fcn,
			    const string &nm)
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
apply_mapper_fcn (const octave_value& arg, builtin_mapper_function& m_fcn,
		  bool /* print */)
{
  octave_value retval;

  if (m_fcn.ch_mapper)
    {
      // XXX FIXME XXX -- this could be done in a better way...

      octave_value tmp = arg.convert_to_str ();

      if (! error_state)
	{
	  charMatrix chm = tmp.char_matrix_value ();

	  if (! error_state)
	    {
	      int nr = chm.rows ();
	      int nc = chm.cols ();

	      switch (m_fcn.flag)
		{
		case 0:
		  {
		    Matrix result (nr, nc);

		    // islapha and friends can return any nonzero value
		    // to mean true, but we want to return 1 or 0 only.

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j)
			  = (*m_fcn.ch_mapper) (chm (i, j)) ? 1 : 0;

		    retval = result;
		  }
		  break;

		case 1:
		  {
		    Matrix result (nr, nc);

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j)
			  = (*m_fcn.ch_mapper) (chm (i, j));

		    retval = result;
		  }
		  break;

		case 2:
		  {
		    charMatrix result (nr, nc);

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j)
			  = (*m_fcn.ch_mapper) (chm (i, j));

		    retval = octave_value (result, true);
		  }
		  break;

		default:
		  panic_impossible ();
		  break;
		}
	    }
	}
    }
  else
    {
      if (arg.is_real_type ())
	{
	  if (arg.is_scalar_type ())
	    {
	      double d = arg.double_value ();

	      if (m_fcn.flag
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

	      if (m_fcn.flag
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
    }

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
tree_builtin::accept (tree_walker& tw)
{
  tw.visit_builtin (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
