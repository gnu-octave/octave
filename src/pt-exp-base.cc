// tree-expr.cc                                          -*- C++ -*-
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

#include <cctype>
#include <climits>
#include <cstdio>
#include <cstring>

#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "variables.h"
#include "user-prefs.h"
#include "dynamic-ld.h"
#include "help.h"
#include "error.h"
#include "gripes.h"
#include "pager.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-misc.h"
#include "tree-const.h"
#include "input.h"
#include "symtab.h"
#include "utils.h"
#include "octave.h"
#include "octave-hist.h"
#include "unwind-prot.h"
#include "parse.h"
#include "lex.h"
#include "defun.h"

// Nonzero means we're returning from a function.
extern int returning;

// Nonzero means we're breaking out of a loop or function body.
extern int breaking;

// But first, some extra functions used by the tree classes.

// We seem to have no use for this now.  Maybe it will be needed at
// some future date, so here it is.
#if 0
// Convert a linked list of trees to a vector of pointers to trees.

static tree **
list_to_vector (tree *list, int& len)
{
  len = list->length () + 1;

  tree **args = new tree * [len];

// args[0] may eventually hold something useful, like the function
// name.
  tree *tmp_list = list;
  for (int k = 1; k < len; k++)
    {
      args[k] = tmp_list;
      tmp_list = tmp_list->next_elem ();
    }
  return args;
}
#endif

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

static int
all_args_defined (const Octave_object& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (args(i).is_undefined ())
      return 0;

  return 1;
}

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

// Expressions.

tree_constant
tree_expression::eval (int print)
{
  panic ("invalid evaluation of generic expression");
  return tree_constant ();
}

// General matrices.  This list type is much more work to handle than
// constant matrices, but it allows us to construct matrices from
// other matrices, variables, and functions.

tree_matrix::~tree_matrix (void)
{
  delete element;
  delete next;
}

tree_matrix *
tree_matrix::chain (tree_expression *t, tree_matrix::dir d)
{
  tree_matrix *tmp = new tree_matrix (t, d);
  tmp->next = this;
  return tmp;
}

tree_matrix *
tree_matrix::reverse (void)
{
  tree_matrix *list = this;
  tree_matrix *next;
  tree_matrix *prev = 0;

  while (list)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_matrix::length (void)
{
  tree_matrix *list = this;
  int len = 0;
  while (list)
    {
      len++;
      list = list->next;
    }
  return len;
}

tree_return_list *
tree_matrix::to_return_list (void)
{
  tree_return_list *retval = 0;

  tree_matrix *list;

  for (list = this; list; list = list->next)
    {
      tree_expression *elem = list->element;

      int is_id = elem->is_identifier ();

      int is_idx_expr = elem->is_index_expression ();

      if (is_id || is_idx_expr)
	{
	  tree_index_expression *idx_expr;
	  if (is_id)
	    {
	      tree_identifier *id = (tree_identifier *) elem;
	      idx_expr = new tree_index_expression (id);
	    }
	  else
	    idx_expr = (tree_index_expression *) elem;

	  if (list == this)
	    retval = new tree_return_list (idx_expr);
	  else
	    retval->append (idx_expr);
	}
      else
	{
	  delete retval;
	  retval = 0;
	  break;
	}
    }

  return retval;
}

// Just about as ugly as it gets.

struct const_matrix_list
{
  tree_matrix::dir direction;
  tree_constant elem;
  int nr;
  int nc;
};

// Less ugly than before, anyway.

tree_constant
tree_matrix::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

// Just count the elements without looking at them.

  int total_len = length ();

// Easier to deal with this later instead of a tree_matrix structure.

  const_matrix_list *list = new const_matrix_list [total_len];

// Stats we want to keep track of.

  int all_strings = 1;

  int found_complex = 0;

  int row_total = 0;
  int col_total = 0;

  int row_height = 0;

  int cols_this_row = 0;

  int first_row = 1;

  int empties_ok = user_pref.empty_list_elements_ok;

  tree_matrix *ptr = this;

// Stuff for the result matrix or string.  Declared here so that we
// don't get warnings from gcc about the goto crossing the
// initialization of these values.

  int put_row = 0;
  int put_col = 0;

  int prev_nr = 0;
  int prev_nc = 0;

  Matrix m;
  ComplexMatrix cm;

  Octave_str_obj string;

// Eliminate empties and gather stats.

  int found_new_row_in_empties = 0;

  int len = 0;
  for (int i = 0; i < total_len; i++)
    {
      tree_expression *elem = ptr->element;
      if (! elem)
	{
	  retval = tree_constant (Matrix ());
	  goto done;
	}

      tree_constant tmp = elem->eval (0);
      if (error_state || tmp.is_undefined ())
	{
	  retval = tree_constant ();
	  goto done;
	}

      int nr = tmp.rows ();
      int nc = tmp.columns ();

      dir direct = ptr->direction;

      if (nr == 0 || nc == 0)
	{
	  if (empties_ok < 0)
	    warning ("empty matrix found in matrix list");
	  else if (empties_ok == 0)
	    {
	      ::error ("empty matrix found in matrix list");
	      retval = tree_constant ();
	      goto done;
	    }

	  if (direct == md_down)
	    found_new_row_in_empties = 1;

	  goto next;
	}

      if (found_new_row_in_empties)
	{
	  found_new_row_in_empties = 0;
	  list[len].direction = md_down;
	}
      else
	list[len].direction = direct;

      list[len].elem = tmp;
      list[len].nr = nr;
      list[len].nc = nc;

      if (all_strings && ! tmp.is_string ())
	all_strings = 0;

      if (! found_complex && tmp.is_complex_type ())
	found_complex = 1;

      len++;

    next:

      ptr = ptr->next;
    }

//  if (all_strings)
//    cerr << "all strings\n";

// Compute size of result matrix, and check to see that the dimensions
// of all the elements will match up properly.

  for (int i = 0; i < len; i++)
    {
      dir direct = list[i].direction;

      int nr = list[i].nr;
      int nc = list[i].nc;

      if (i == 0)
	{
	  row_total = nr;
	  col_total = nc;

	  row_height = nr;
	  cols_this_row = nc;
	}
      else
	{
	  switch (direct)
	    {
	    case md_right:
	      {
		if (nr != row_height)
		  {
		    ::error ("number of rows must match");
		    goto done;
		  }
		else
		  {
		    cols_this_row += nc;
		    if (first_row)
		      col_total = cols_this_row;
		  }
	      }
	      break;

	    case md_down:
	      {
		if (cols_this_row != col_total && ! all_strings)
		  {
		    ::error ("number of columns must match");
		    goto done;
		  }
		first_row = 0;
		row_total += nr;
		row_height = nr;
		cols_this_row = nc;
	      }
	      break;

	    default:
	      panic_impossible ();
	      break;
	    }
	}
    }

// Don\'t forget to check to see if the last element will fit.

  if (cols_this_row != col_total && ! all_strings)
    {
      ::error ("number of columns must match");
      goto done;
    }

// Now, extract the values from the individual elements and insert
// them in the result matrix.

  if (all_strings)
    string.resize (row_total);
  else if (found_complex)
    cm.resize (row_total, col_total, 0.0);
  else
    m.resize (row_total, col_total, 0.0);

  for (int i = 0; i < len; i++)
    {
      tree_constant tmp = list[i].elem;

      int nr = list[i].nr;
      int nc = list[i].nc;

      if (nr == 0 || nc == 0)
	continue;

      if (i == 0)
	{
	  put_row = 0;
	  put_col = 0;
	}
      else
	{
	  switch (list[i].direction)
	    {
	    case md_right:
	      put_col += prev_nc;
	      break;

	    case md_down:
	      put_row += prev_nr;
	      put_col = 0;
	      break;

	    default:
	      panic_impossible ();
	      break;
	    }
	}

      if (found_complex)
	{
	  if (tmp.is_real_scalar ())
	    {
	      cm (put_row, put_col) = tmp.double_value ();
	    }
	  else if (tmp.is_real_matrix () || tmp.is_range ())
	    {
	      cm.insert (tmp.matrix_value (), put_row, put_col);
	    }
	  else if (tmp.is_complex_scalar ())
	    {
	      cm (put_row, put_col) = tmp.complex_value ();
	    }
	  else
	    {
	      ComplexMatrix cm_tmp = tmp.complex_matrix_value ();

	      if (error_state)
		goto done;

	      cm.insert (cm_tmp, put_row, put_col);
	    }
	}
      else
	{
	  if (tmp.is_real_scalar ())
	    {
	      m (put_row, put_col) = tmp.double_value ();
	    }
	  else if (tmp.is_string () && all_strings)
	    {
	      switch (list[i].direction)
		{
		case md_right:
		  if (nr == 1)
		    string.append_right (put_row, tmp.string_value ());
		  else
		    string.append_right (tmp.all_strings ());
		  break;

		case md_none:
		case md_down:
		  string.append_down (put_row, tmp.all_strings ());
		  break;
		  
		default:
		  panic_impossible ();
		  break;
		}
	    }
	  else
	    {
	      Matrix m_tmp = tmp.matrix_value ();

	      if (error_state)
		goto done;

	      m.insert (m_tmp, put_row, put_col);
	    }
	}

      prev_nr = nr;
      prev_nc = nc;
    }

  if (all_strings && string.num_strings () > 0)
    retval = string;
  else if (found_complex)
    retval = cm;
  else
    retval = m;

 done:
  delete [] list;

  return retval;
}

void
tree_matrix::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  os << "[";

  tree_matrix *list = this;

  while (list)
    {
      list->element->print_code (os);

      list = list->next;

      if (list)
	{
	  switch (list->direction)
	    {
	    case md_right:
	      os << ", ";
	      break;

	    case md_down:
	      os << "; ";
	      break;

	    default:
	      break;
	    }
	}
    }

  os << "]";

  if (in_parens)
    os << ")";
}

// A base class for objects that can be return multiple values

tree_constant
tree_multi_val_ret::eval (int print)
{
  panic ("invalid evaluation of generic expression");
  return tree_constant ();
}

// Used internally.

tree_constant
tree_oct_obj::eval (int print)
{
  return values(0);
}

Octave_object
tree_oct_obj::eval (int print, int nargout, const Octave_object& args)
{
  return values;
}

// A base class for objects that can be evaluated with argument lists.

tree_constant
tree_fvc::assign (tree_constant& t, const Octave_object& args)
{
  panic_impossible ();
  return tree_constant ();
}

tree_constant
tree_fvc::lookup_map_element (SLList<char*>& list, int insert, int silent)
{
  static tree_constant retval;
  return retval;
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

// Don't count the output arguments that we create automatically.

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

// Index expressions.

tree_index_expression::~tree_index_expression (void)
{
  delete id;
  delete list;
}

tree_constant
tree_index_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (list)
    {
// Extract the arguments into a simple vector.  Don't pass null args.

      Octave_object args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  int nargin = args.length ();

	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (args))
		{
		  Octave_object tmp = id->eval (print, 1, args);

		  if (error_state)
		    eval_error ();
		  else if (tmp.length () > 0)
		    retval = tmp(0);
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      retval = id->eval (print);

      if (error_state)
	eval_error ();
    }

  return retval;
}

Octave_object
tree_index_expression::eval (int print, int nargout, const Octave_object& args)
{
  Octave_object retval;

  if (error_state)
    return retval;

  if (list)
    {
// Extract the arguments into a simple vector.  Don't pass null args.

      Octave_object args = list->convert_to_const_vector ();

      if (error_state)
	eval_error ();
      else
	{
	  int nargin = args.length ();

	  if (error_state)
	    eval_error ();
	  else
	    {
	      if (all_args_defined (args))
		{
		  retval = id->eval (print, nargout, args);

		  if (error_state)
		    eval_error ();
		}
	      else
		{
		  ::error ("undefined arguments found in index expression");
		  eval_error ();
		}
	    }
	}
    }
  else
    {
      Octave_object tmp_args;

      retval = id->eval (print, nargout, tmp_args);

      if (error_state)
	eval_error ();
    }

  return retval;
}

void
tree_index_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();
      char *fmt;
      if (l != -1 && c != -1)
	{
	  if (list)
	    fmt = "evaluating index expression near line %d, column %d";
	  else
	    fmt = "evaluating expression near line %d, column %d";

	  ::error (fmt, l, c);
	}
      else
	{
	  if (list)
	    ::error ("evaluating index expression");
	  else
	    ::error ("evaluating expression");
	}
    }
}

void
tree_index_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (id)
    id->print_code (os);

  if (list)
    {
      os << " (";
      list->print_code (os);
      os << ")";
    }

  if (in_parens)
    os << ")";
}

// Prefix expressions.

tree_constant
tree_prefix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id)
    {
      id->bump_value (etype);
      if (error_state)
	eval_error ();
      else
	{
	  retval = id->eval (print);
	  if (error_state)
	    {
	      retval = tree_constant ();
	      if (error_state)
		eval_error ();
	    }
	}
    }
  return retval;
}

char *
tree_prefix_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::increment:
      op = "++";
      break;

    case tree_expression::decrement:
      op = "--";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_prefix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating prefix operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_prefix_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  os << oper ();

  if (id)
    id->print_code (os);

  if (in_parens)
    os << ")";
}

// Postfix expressions.

tree_constant
tree_postfix_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (id)
    {
      retval = id->eval (print);
      id->bump_value (etype);
      if (error_state)
	{
	  retval = tree_constant ();
	  if (error_state)
	    eval_error ();
	}
    }
  return retval;
}

char *
tree_postfix_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::increment:
      op = "++";
      break;

    case tree_expression::decrement:
      op = "--";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_postfix_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating postfix operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_postfix_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (id)
    id->print_code (os);

  os << oper ();

  if (in_parens)
    os << ")";
}

// Unary expressions.

tree_constant
tree_unary_expression::eval (int print)
{
  if (error_state)
    return tree_constant ();

  tree_constant retval;

  switch (etype)
    {
    case tree_expression::not:
    case tree_expression::uminus:
    case tree_expression::hermitian:
    case tree_expression::transpose:
      if (op)
	{
	  tree_constant u = op->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (u.is_defined ())
	    {
	      retval = do_unary_op (u, etype);
	      if (error_state)
		{
		  retval = tree_constant ();
		  if (error_state)
		    eval_error ();
		}
	    }
	}
      break;

    default:
      ::error ("unary operator %d not implemented", etype);
      break;
    }

  return retval;
}

char *
tree_unary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::not:
      op = "!";
      break;

    case tree_expression::uminus:
      op = "-";
      break;

    case tree_expression::hermitian:
      op = "'";
      break;

    case tree_expression::transpose:
      op = ".'";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_unary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating unary operator `%s' near line %d, column %d",
	       op, line (), column ());
    }
}

void
tree_unary_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  switch (etype)
    {
    case tree_expression::not:
    case tree_expression::uminus:
      os << oper ();
      if (op)
	op->print_code (os);
      break;

    case tree_expression::hermitian:
    case tree_expression::transpose:
      if (op)
	op->print_code (os);
      os << oper ();
      break;

    default:
      os << oper ();
      if (op)
	op->print_code (os);
      break;
    }

  if (in_parens)
    os << ")";
}

// Binary expressions.
 
tree_constant
tree_binary_expression::eval (int print)
{
  if (error_state)
    return tree_constant ();

  tree_constant retval;

  switch (etype)
    {
    case tree_expression::add:
    case tree_expression::subtract:
    case tree_expression::multiply:
    case tree_expression::el_mul:
    case tree_expression::divide:
    case tree_expression::el_div:
    case tree_expression::leftdiv:
    case tree_expression::el_leftdiv:
    case tree_expression::power:
    case tree_expression::elem_pow:
    case tree_expression::cmp_lt:
    case tree_expression::cmp_le:
    case tree_expression::cmp_eq:
    case tree_expression::cmp_ge:
    case tree_expression::cmp_gt:
    case tree_expression::cmp_ne:
    case tree_expression::and:
    case tree_expression::or:
      if (op1)
	{
	  tree_constant a = op1->eval (0);
	  if (error_state)
	    eval_error ();
	  else if (a.is_defined () && op2)
	    {
	      tree_constant b = op2->eval (0);
	      if (error_state)
		eval_error ();
	      else if (b.is_defined ())
		{
		  retval = do_binary_op (a, b, etype);
		  if (error_state)
		    {
		      retval = tree_constant ();
		      if (error_state)
			eval_error ();
		    }
		}
	    }
	}
      break;

    case tree_expression::and_and:
    case tree_expression::or_or:
      {
	int result = 0;
	if (op1)
	  {
	    tree_constant a = op1->eval (0);
	    if (error_state)
	      {
		eval_error ();
		break;
	      }

	    int a_true = a.is_true ();
	    if (error_state)
	      {
		eval_error ();
		break;
	      }

	    if (a_true)
	      {
		if (etype == tree_expression::or_or)
		  {
		    result = 1;
		    goto done;
		  }
	      }
	    else
	      {
		if (etype == tree_expression::and_and)
		  {
		    result = 0;
		    goto done;
		  }
	      }

	    if (op2)
	      {
		tree_constant b = op2->eval (0);
		if (error_state)
		  {
		    eval_error ();
		    break;
		  }

		result = b.is_true ();
		if (error_state)
		  {
		    eval_error ();
		    break;
		  }
	      }
	  }
      done:
	retval = tree_constant ((double) result);
      }
      break;

    default:
      ::error ("binary operator %d not implemented", etype);
      break;
    }

  return retval;
}

char *
tree_binary_expression::oper (void) const
{
  static char *op;
  switch (etype)
    {
    case tree_expression::add:
      op = "+";
      break;

    case tree_expression::subtract:
      op = "-";
      break;

    case tree_expression::multiply:
      op = "*";
      break;

    case tree_expression::el_mul:
      op = ".*";
      break;

    case tree_expression::divide:
      op = "/";
      break;

    case tree_expression::el_div:
      op = "./";
      break;

    case tree_expression::leftdiv:
      op = "\\";
      break;

    case tree_expression::el_leftdiv:
      op = ".\\";
      break;

    case tree_expression::power:
      op = "^";
      break;

    case tree_expression::elem_pow:
      op = ".^";
      break;

    case tree_expression::cmp_lt:
      op = "<";
      break;

    case tree_expression::cmp_le:
      op = "<=";
      break;

    case tree_expression::cmp_eq:
      op = "==";
      break;

    case tree_expression::cmp_ge:
      op = ">=";
      break;

    case tree_expression::cmp_gt:
      op = ">";
      break;

    case tree_expression::cmp_ne:
      op = "!=";
      break;

    case tree_expression::and_and:
      op = "&&";
      break;

    case tree_expression::or_or:
      op = "||";
      break;

    case tree_expression::and:
      op = "&";
      break;

    case tree_expression::or:
      op = "|";
      break;

    default:
      op = "<unknown>";
      break;
    }
  return op;
}

void
tree_binary_expression::eval_error (void)
{
  if (error_state > 0)
    {
      char *op = oper ();

      ::error ("evaluating binary operator `%s' near line %d, column %d",
	     op, line (), column ());
    }
}

void
tree_binary_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (op1)
    op1->print_code (os);

  os << " " << oper () << " ";

  if (op2)
    op2->print_code (os);

  if (in_parens)
    os << ")";
}

// Simple assignment expressions.

tree_simple_assignment_expression::~tree_simple_assignment_expression (void)
{
  if (! preserve)
    {
      if (lhs_idx_expr)
	delete lhs_idx_expr;
      else
	delete lhs;
    }

  delete rhs;
}

tree_constant
tree_simple_assignment_expression::eval (int print)
{
  assert (etype == tree_expression::assignment);

  tree_constant retval;

  if (error_state)
    return retval;

  if (rhs)
    {
      tree_constant rhs_val = rhs->eval (0);
      if (error_state)
	{
	  eval_error ();
	}
      else if (rhs_val.is_undefined ())
	{
	  error ("value on right hand side of assignment is undefined");
	  eval_error ();
	}
      else if (! index)
	{
	  retval = lhs->assign (rhs_val);
	  if (error_state)
	    eval_error ();
	}
      else
	{
// Extract the arguments into a simple vector.

	  Octave_object args = index->convert_to_const_vector ();

	  if (error_state)
	    eval_error ();
	  else
	    {
	      int nargin = args.length ();

	      if (error_state)
		eval_error ();
	      else if (nargin > 0)
		{
		  retval = lhs->assign (rhs_val, args);
		  if (error_state)
		    eval_error ();
		}
	    }
	}
    }

  if (! error_state && print && retval.is_defined ())
    print_constant (retval, lhs->name ());

  return retval;
}

void
tree_simple_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    {
      int l = line ();
      int c = column ();
      if (l != -1 && c != -1)
	::error ("evaluating assignment expression near line %d, column %d",
		 l, c);
//      else
//	::error ("evaluating assignment expression");
    }
}

void
tree_simple_assignment_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (! is_ans_assign ())
    {
      if (lhs)
	lhs->print_code (os);

      if (index)
	{
	  os << " (";
	  index->print_code (os);
	  os << ")";
	}

      os << " = ";
    }

  if (rhs)
    rhs->print_code (os);

  if (in_parens)
    os << ")";
}

// Multi-valued assignmnt expressions.

tree_multi_assignment_expression::~tree_multi_assignment_expression (void)
{
  if (! preserve)
    delete lhs;

  delete rhs;
}

tree_constant
tree_multi_assignment_expression::eval (int print)
{
  tree_constant retval;

  if (error_state)
    return retval;

  Octave_object tmp_args;
  Octave_object result = eval (print, 1, tmp_args);

  if (result.length () > 0)
    retval = result(0);

  return retval;
}

Octave_object
tree_multi_assignment_expression::eval (int print, int nargout,
					const Octave_object& args)
{
  assert (etype == tree_expression::multi_assignment);

  if (error_state || ! rhs)
    return Octave_object ();

  nargout = lhs->length ();
  Octave_object tmp_args;
  Octave_object results = rhs->eval (0, nargout, tmp_args);

  if (error_state)
    eval_error ();

  int ma_line = line ();
  int ma_column = column ();

  if (results.length () > 0)
    {
      int i = 0;
      int pad_after = 0;
      int last_was_scalar_type = 0;
      for (Pix p = lhs->first (); p != 0; lhs->next (p))
	{
	  tree_index_expression *lhs_expr = lhs->operator () (p);

	  if (i < nargout)
	    {
// XXX FIXME? XXX -- this is apparently the way Matlab works, but
// maybe we should have the option of skipping the assignment instead.

	      tree_constant *tmp = 0;
	      if (results(i).is_undefined ())
		{
		  error ("element number %d undefined in return list", i+1);
		  eval_error ();
		  break;
		}
	      else
		tmp = new tree_constant (results(i));

	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, tmp, 1, 0, ma_line, ma_column);

	      results(i) = tmp_expr.eval (0); // May change

	      if (error_state)
		break;

	      if (print && pad_after)
		{
		  ostrstream output_buf;
		  output_buf << "\n" << ends;
		  maybe_page_output (output_buf);
		}

	      if (print)
		print_constant (results(i), lhs_expr->name (), 0);

	      pad_after++;
	      i++;
	    }
	  else
	    {
	      tree_simple_assignment_expression tmp_expr
		(lhs_expr, 0, 1, 0, ma_line, ma_column);

	      tmp_expr.eval (0);

	      if (error_state)
		break;

	      if (last_was_scalar_type && i == 1)
		pad_after = 0;

	      break;
	    }
	}

      if (print && pad_after)
	{
	  ostrstream output_buf;
	  output_buf << "\n" << ends;
	  maybe_page_output (output_buf);
	}
    }

  return results;
}

void
tree_multi_assignment_expression::eval_error (void)
{
  if (error_state > 0)
    ::error ("evaluating assignment expression near line %d, column %d",
	     line (), column ());
}

void
tree_multi_assignment_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (lhs)
    {
      int len = lhs->length ();

      if (len > 1)
	os << "[";

      lhs->print_code (os);

      if (len > 1)
	os << "]";
    }

  os << " = ";

  if (rhs)
    rhs->print_code (os);

  if (in_parens)
    os << ")";
}

// Colon expressions.

tree_colon_expression *
tree_colon_expression::chain (tree_expression *t)
{
  tree_colon_expression *retval = 0;
  if (! op1 || op3)
    ::error ("invalid colon expression");
  else
    {
      op3 = op2;	// Stupid syntax.
      op2 = t;

      retval = this;
    }
  return retval;
}

tree_constant
tree_colon_expression::eval (int print)
{
  tree_constant retval;

  if (error_state || ! op1 || ! op2)
    return retval;

  tree_constant tmp = op1->eval (0);

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double base = tmp.double_value ();

  if (error_state)
    {
      error ("colon expression elements must be scalars");
      eval_error ("evaluating colon expression");
      return retval;
    }

  tmp = op2->eval (0);

  if (tmp.is_undefined ())
    {
      eval_error ("invalid null value in colon expression");
      return retval;
    }

  double limit = tmp.double_value ();

  if (error_state)
    {
      error ("colon expression elements must be scalars");
      eval_error ("evaluating colon expression");
      return retval;
    }

  double inc = 1.0;
  if (op3)
    {
      tmp = op3->eval (0);

      if (tmp.is_undefined ())
	{
	  eval_error ("invalid null value in colon expression");
	  return retval;
	}

      inc = tmp.double_value ();

      if (error_state)
	{
	  error ("colon expression elements must be scalars");
	  eval_error ("evaluating colon expression");
	  return retval;
	}
    }

  retval = tree_constant (base, limit, inc);

  if (error_state)
    {
      if (error_state)
	eval_error ("evaluating colon expression");
      return tree_constant ();
    }

  return retval;
}

void
tree_colon_expression::eval_error (const char *s)
{
  if (error_state > 0)
    ::error ("%s near line %d column %d", s, line (), column ());
}

void
tree_colon_expression::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (op1)
    op1->print_code (os);

// Stupid syntax.

  if (op3)
    {
      os << ":";
      op3->print_code (os);
    }

  if (op2)
    {
      os << ":";
      op2->print_code (os);
    }

  if (in_parens)
    os << ")";
}

// Builtin functions.

tree_builtin::tree_builtin (const char *nm)
{
  nargin_max = -1;
  nargout_max = -1;
  is_mapper = 0;
  fcn = 0;
  if (nm)
    my_name = strsave (nm);
}

tree_builtin::tree_builtin (int i_max, int o_max, Mapper_fcn& m_fcn,
			    const char *nm)
{
  nargin_max = i_max;
  nargout_max = o_max;
  mapper_fcn = m_fcn;
  is_mapper = 1;
  fcn = 0;
  my_name = nm ? strsave (nm) : 0;
}

tree_builtin::tree_builtin (int i_max, int o_max, Octave_builtin_fcn g_fcn,
			    const char *nm)
{
  nargin_max = i_max;
  nargout_max = o_max;
  is_mapper = 0;
  fcn = g_fcn;
  my_name = nm ? strsave (nm) : 0;
}

tree_constant
tree_builtin::eval (int print)
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
apply_mapper_fcn (const tree_constant& arg, Mapper_fcn& m_fcn, int print)
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
tree_builtin::eval (int print, int nargout, const Octave_object& args)
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
      if (nargin > nargin_max)
	::error ("%s: too many arguments", my_name);
      else if (nargin > 0 && args(0).is_defined ())
	{
	  tree_constant tmp = apply_mapper_fcn (args(0), mapper_fcn, 0);
	  retval(0) = tmp;
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

int
tree_builtin::max_expected_args (void)
{
  int ea = nargin_max;
  if (nargin_max < 0)
    ea = INT_MAX;
  else
    ea = nargin_max;
  return ea;
}

// User defined functions.

void
tree_function::install_nargin_and_nargout (void)
{
  nargin_sr = sym_tab->lookup ("nargin", 1, 0);
  nargout_sr = sym_tab->lookup ("nargout", 1, 0);
}

void
tree_function::bind_nargin_and_nargout (int nargin, int nargout)
{
  tree_constant *tmp;

  tmp = new tree_constant (nargin);
  nargin_sr->define (tmp);

  tmp = new tree_constant (nargout);
  nargout_sr->define (tmp);
}

tree_function::~tree_function (void)
{
  delete param_list;
  delete ret_list;
  delete sym_tab;
  delete cmd_list;
  delete [] file_name;
  delete [] fcn_name;
  delete vr_list;
}

#if 0
tree_function *
tree_function::define (tree statement_list *t)
{
  cmd_list = t;
  return this;
}
#endif

tree_function *
tree_function::define_param_list (tree_parameter_list *t)
{
  param_list = t;

  if (param_list)
    {
      num_named_args = param_list->length ();
      curr_va_arg_number = num_named_args;
    }

  return this;
}

tree_function *
tree_function::define_ret_list (tree_parameter_list *t)
{
  ret_list = t;

  if (ret_list && ret_list->takes_varargs ())
    vr_list = new tree_va_return_list;
 
  return this;
}

void
tree_function::stash_fcn_file_name (void)
{
  delete [] file_name;
  file_name = fcn_name ? fcn_file_in_path (fcn_name) : 0;
}

void
tree_function::mark_as_system_fcn_file (void)
{
  if (file_name)
    {
// We really should stash the whole path to the file we found, when we
// looked it up, to avoid possible race conditions...  XXX FIXME XXX
//
// We probably also don't need to get the library directory every
// time, but since this function is only called when the function file
// is parsed, it probably doesn't matter that much.

      char *oct_lib = octave_lib_dir ();
      int len = strlen (oct_lib);

      char *ff_name = fcn_file_in_path (file_name);

      if (strncmp (oct_lib, ff_name, len) == 0)
	system_fcn_file = 1;

      delete [] ff_name;
    }
  else
    system_fcn_file = 0;
}

int
tree_function::takes_varargs (void) const
{
  return (param_list && param_list->takes_varargs ());
}

tree_constant
tree_function::octave_va_arg (void)
{
  tree_constant retval;

  if (curr_va_arg_number < num_args_passed)
    retval = args_passed (curr_va_arg_number++);
  else
    ::error ("va_arg: error getting arg number %d -- only %d provided",
	     curr_va_arg_number + 1, num_args_passed);

  return retval;
}

Octave_object
tree_function::octave_all_va_args (void)
{
  Octave_object retval;

  retval.resize (num_args_passed - num_named_args);

  int k = 0;
  for (int i = num_named_args; i < num_args_passed; i++)
    retval(k++) = args_passed(i);

  return retval;
}

int
tree_function::takes_var_return (void) const
{
  return (ret_list && ret_list->takes_varargs ());
}

void
tree_function::octave_vr_val (const tree_constant& val)
{
  assert (vr_list);

  vr_list->append (val);
}

void
tree_function::stash_function_name (char *s)
{
  delete [] fcn_name;
  fcn_name = strsave (s);
}

tree_constant
tree_function::eval (int print)
{
  tree_constant retval;

  if (error_state || ! cmd_list)
    return retval;

  Octave_object tmp_args;
  Octave_object tmp = eval (print, 0, tmp_args);

  if (! error_state && tmp.length () > 0)
    retval = tmp(0);

  return retval;
}

// For unwind protect.

static void
pop_symbol_table_context (void *table)
{
  symbol_table *tmp = (symbol_table *) table;
  tmp->pop_context ();
}

static void
delete_vr_list (void *list)
{
  tree_va_return_list *tmp = (tree_va_return_list *) list;
  tmp->clear ();
  delete tmp;
}

static void
clear_symbol_table (void *table)
{
  symbol_table *tmp = (symbol_table *) table;
  tmp->clear ();
}

Octave_object
tree_function::eval (int print, int nargout, const Octave_object& args)
{
  Octave_object retval;

  if (error_state)
    return retval;

  if (! cmd_list)
    return retval;

  int nargin = args.length ();

  begin_unwind_frame ("func_eval");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    {
      sym_tab->push_context ();
      add_unwind_protect (pop_symbol_table_context, (void *) sym_tab);

      if (vr_list)
	{
// Push new vr_list.
	  unwind_protect_ptr (vr_list);
	  vr_list = new tree_va_return_list;

// Clear and delete the new one before restoring the old one.
	  add_unwind_protect (delete_vr_list, (void *) vr_list);
	}
    }

  if (vr_list)
    vr_list->clear ();

// Force symbols to be undefined again when this function exits.

  add_unwind_protect (clear_symbol_table, (void *) sym_tab);

// Save old and set current symbol table context, for eval_undefined_error().

  unwind_protect_ptr (curr_sym_tab);
  curr_sym_tab = sym_tab;

  unwind_protect_ptr (curr_function);
  curr_function = this;

//  unwind_protect_ptr (args_passed);
  args_passed = args;

  unwind_protect_int (num_args_passed);
  num_args_passed = nargin;

  unwind_protect_int (num_named_args);
  unwind_protect_int (curr_va_arg_number);

  if (param_list && ! param_list->varargs_only ())
    {
      param_list->define_from_arg_vector (args);
      if (error_state)
	goto abort;
    }

// The following code is in a separate scope to avoid warnings from
// G++ about `goto abort' crossing the initialization of some
// variables.

  {
    bind_nargin_and_nargout (nargin, nargout);
      
// Evaluate the commands that make up the function.  Always turn on
// printing for commands inside functions.   Maybe this should be
// toggled by a user-leval variable?

    int pf = ! user_pref.silent_functions;
    tree_constant last_computed_value = cmd_list->eval (pf);

    if (returning)
      returning = 0;

    if (breaking)
      breaking--;

    if (error_state)
      {
	traceback_error ();
	goto abort;
      }
    
// Copy return values out.

    if (ret_list)
      {
	if (nargout > 0 && user_pref.define_all_return_values)
	  {
	    tree_constant tmp = builtin_any_variable ("default_return_value");
	    if (tmp.is_defined ())
	      ret_list->initialize_undefined_elements (tmp);
	  }

	retval = ret_list->convert_to_const_vector (vr_list);
      }
    else if (user_pref.return_last_computed_value)
      retval(0) = last_computed_value;
  }

 abort:
  run_unwind_frame ("func_eval");

  return retval;
}

int
tree_function::max_expected_args (void)
{
  if (param_list)
    {
      if (param_list->takes_varargs ())
	return -1;
      else
	return param_list->length ();
    }
  else
    return 1;
}

void
tree_function::traceback_error (void)
{
  if (error_state >= 0)
    error_state = -1;

  if (fcn_name)
    {
      if (file_name)
	::error ("called from `%s' in file `%s'", fcn_name, file_name);
      else 
	::error ("called from `%s'", fcn_name);
    }
  else
    {
      if (file_name)
	::error ("called from file `%s'", file_name);
      else
	::error ("called from `?unknown?'");
    }
}

void
tree_function::print_code (ostream& os)
{
  print_code_reset ();

  print_code_indent (os);

  os << "function ";

  if (ret_list)
    {
      int len = ret_list->length ();

      if (len > 1)
	os << "[";

      ret_list->print_code (os);

      if (len > 1)
	os << "]";

      os << " = ";
    }

  os << (fcn_name ? fcn_name : "(null)") << " ";

  if (param_list)
    {
      int len = param_list->length ();
      if (len > 0)
	os << "(";

      param_list->print_code (os);

      if (len > 0)
	{
	  os << ")";
	  print_code_new_line (os);
	}
    }
  else
    {
      os << "()";
      print_code_new_line (os);
    }

  if (cmd_list)
    {
      increment_indent_level ();
      cmd_list->print_code (os);
    }

  os << "endfunction";

  print_code_new_line (os);
}

DEFUN ("va_arg", Fva_arg, Sva_arg, 0, 1,
  "va_arg (): return next argument in a function that takes a\n\
variable number of parameters")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      if (curr_function)
	{
	  if (curr_function->takes_varargs ())
	    retval = curr_function->octave_va_arg ();
	  else
	    {
	      ::error ("va_arg only valid within function taking variable");
	      ::error ("number of arguments");
	    }
	}
      else
	::error ("va_arg only valid within function body");
    }
  else
    print_usage ("va_arg");

  return retval;
}

DEFUN ("va_start", Fva_start, Sva_start, 0, 0,
  "va_start (): reset the pointer to the list of optional arguments\n\
to the beginning")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      if (curr_function)
	{
	  if (curr_function->takes_varargs ())
	    curr_function->octave_va_start ();
	  else
	    {
	      ::error ("va_start only valid within function taking variable");
	      ::error ("number of arguments");
	    }
	}
      else
	::error ("va_start only valid within function body");
    }
  else
    print_usage ("va_start");

  return retval;
}

DEFUN ("vr_val", Fvr_val, Svr_val, 1, 0,
  "vr_val (X): append X to the list of optional return values for a
function that allows a variable number of return values")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (curr_function)
	{
	  if (curr_function->takes_var_return ())
	    curr_function->octave_vr_val (args(0));
	  else
	    {
	      ::error ("vr_val only valid within function declared to");
	      ::error ("produce a variable number of values");
	    }
	}
      else
	::error ("vr_val only valid within function body");
    }
  else
    print_usage ("vr_val");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
