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

#include "Cell.h"
#include "error.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-idx.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

#include "SLList.cc"

template class SLNode<string_vector>;
template class SLList<string_vector>;

// Index expressions.

tree_index_expression::tree_index_expression (tree_expression *e,
					      tree_argument_list *lst,
					      int l, int c, char t)
  : tree_expression (l, c), expr (e), args (), type (), arg_nm ()
{
  append (lst, t);
}

tree_index_expression::tree_index_expression (tree_expression *e,
					      const std::string& n,
					      int l = -1, int c = -1)
  : tree_expression (l, c), expr (e), args (), type (), arg_nm ()
{
  append (n);
}

void
tree_index_expression::append (tree_argument_list *lst, char t)
{
  args.append (lst);
  type.append (1, t);
  arg_nm.append (lst ? lst->get_arg_names () : string_vector ());
}

void
tree_index_expression::append (const std::string& n)
{
  args.append (static_cast<tree_argument_list *> (0));
  type.append (".");
  arg_nm.append (n);
}

tree_index_expression::~tree_index_expression (void)
{
  delete expr;

  while (! args.empty ())
    {
      tree_argument_list *t = args.remove_front ();
      delete t;
    }
}

// This is useful for printing the name of the variable in an indexed
// assignment.

std::string
tree_index_expression::name (void) const
{
  // ??? FIXME ???
  std::string xname = expr->name ();

  return (type[0] != '.' || xname == "<unknown>")
    ? xname : xname + "." + arg_nm.front ()(0);
}

static Cell
make_subs_cell (tree_argument_list *args, const string_vector& arg_nm)
{
  Cell retval;

  octave_value_list arg_values;

  if (args)
    arg_values = args->convert_to_const_vector ();

  if (! error_state)
    {
      int n = arg_values.length ();

      if (n > 0)
	{
	  arg_values.stash_name_tags (arg_nm);

	  retval.resize (1, n);

	  for (int i = 0; i < n; i++)
	    retval(0,i) = arg_values(i);
	}
    }

  return retval;
}

static inline octave_value_list
make_value_list (tree_argument_list *args, const string_vector& arg_nm)
{
  octave_value_list retval;

  if (args)
    retval = args->convert_to_const_vector ();

  if (! error_state)
    {
      int n = retval.length ();

      if (n > 0)
	retval.stash_name_tags (arg_nm);
    }

  return retval;
}

Octave_map
tree_index_expression::make_arg_struct (void) const
{
  int n = args.length ();

  octave_value_list subs_list (n, octave_value ());
  octave_value_list type_list (n, octave_value ());

  Pix p_args = args.first ();
  Pix p_arg_nm = arg_nm.first ();

  Octave_map m;

  for (int i = 0; i < n; i++)
    {
      switch (type[i])
	{
	case '(':
	  subs_list(i) = make_subs_cell (args(p_args), arg_nm(p_arg_nm));
	  break;

	case '{':
	  subs_list(i) = make_subs_cell (args(p_args), arg_nm(p_arg_nm));
	  break;

	case '.':
	  subs_list(i) = arg_nm(p_arg_nm)(0);
	  break;

	default:
	  panic_impossible ();
	}

      if (error_state)
	return m;

      args.next (p_args);
      arg_nm.next (p_arg_nm);
    }

  m ["subs"] = subs_list;
  m ["type"] = type_list;

  return m;
}

octave_value_list
tree_index_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  octave_value tmp = expr->rvalue ();

  if (! error_state)
    {
      SLList<octave_value_list> idx;

      int n = args.length ();

      Pix p_args = args.first ();
      Pix p_arg_nm = arg_nm.first ();

      for (int i = 0; i < n; i++)
	{
	  switch (type[i])
	    {
	    case '(':
	      idx.append (make_value_list (args(p_args), arg_nm(p_arg_nm)));
	      break;

	    case '{':
	      idx.append (make_value_list (args(p_args), arg_nm(p_arg_nm)));
	      break;

	    case '.':
	      idx.append (arg_nm(p_arg_nm)(0));
	      break;

	    default:
	      panic_impossible ();
	    }

	  if (error_state)
	    break;

	  args.next (p_args);
	  arg_nm.next (p_arg_nm);
	}

      if (! error_state)
	retval = tmp.subsref (type, idx, nargout);
    }

  return retval;
}

octave_value
tree_index_expression::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_lvalue
tree_index_expression::lvalue (void)
{
  octave_lvalue retval;

  SLList<octave_value_list> idx;

  int n = args.length ();

  Pix p_args = args.first ();
  Pix p_arg_nm = arg_nm.first ();

  for (int i = 0; i < n; i++)
    {
      switch (type[i])
	{
	case '(':
	  idx.append (make_value_list (args(p_args), arg_nm(p_arg_nm)));
	  break;

	case '{':
	  idx.append (make_value_list (args(p_args), arg_nm(p_arg_nm)));
	  break;

	case '.':
	  idx.append (arg_nm(p_arg_nm)(0));
	  break;

	default:
	  panic_impossible ();
	}

      if (error_state)
	break;

      args.next (p_args);
      arg_nm.next (p_arg_nm);
    }

  if (! error_state)
    {
      retval = expr->lvalue ();

      if (! error_state)
	retval.set_index (type, idx);
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

      const char *type_str;

      if (type[0] == '.')
	type_str = "structure reference operator";
      else if (args.front ())
	type_str = "index expression";
      else
	type_str = "expression";

      if (l != -1 && c != -1)
	::error ("evaluating %s near line %d, column %d", type_str, l, c);
      else
	::error ("evaluating %s", type_str);
    }
}

void
tree_index_expression::accept (tree_walker& tw)
{
  tw.visit_index_expression (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
