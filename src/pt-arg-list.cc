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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-usr-fcn.h"
#include "pt-arg-list.h"
#include "pt-exp.h"
#include "pt-pr-code.h"
#include "pt-walk.h"
#include "toplev.h"
#include "unwind-prot.h"

// Argument lists.

tree_argument_list::~tree_argument_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

bool
tree_argument_list::has_magic_end (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_expression *elt = *p;

      if (elt && elt->has_magic_end ())
	return true;
    }

  return false;
}

void
tree_argument_list::append (const element_type& s)
{
  octave_base_list<tree_expression *>::append (s);

  if (! list_includes_magic_end && s && s->has_magic_end ())
    list_includes_magic_end = true;
}

int
tree_argument_list::nargout_count (void) const
{
  int retval = 0;

  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_expression *elt = *p;

      // XXX FIXME XXX -- need to be able to determine whether elt is
      // an expression that could evaluate to a cs-list object, and if
      // so, how many elements are in that list.  Ugly!

      retval++;
    }

  return retval;
}

bool
tree_argument_list::all_elements_are_constant (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_expression *elt = *p;

      if (! elt->is_constant ())
	return false;
    }

  return true;
}

static const octave_value *indexed_object = 0;
static int index_position = 0;

DEFCONSTFUN (__end__, , ,
  "internal function")
{
  octave_value retval;

  if (indexed_object)
    {
      dim_vector dv = indexed_object->dims ();

      switch (index_position)
	{
	case -1:
	  {
	    int numel = dv.numel ();

	    if (numel < 0)
	      {
		std::string dv_str = dv.str ();
		::error ("invalid use of end: (index 1, dims %s)",
			 dv_str.c_str ());
	      }
	    else
	      retval = numel;
	  }
	  break;

	default:
	  {

	    if (index_position < dv.length ())
	      retval = dv(index_position);
	    else
	      {
		std::string dv_str = dv.str ();
		::error ("invalid use of end: (index %d, dims %s)",
			 index_position+1, dv_str.c_str ());
	      }
	  }
	  break;
	}
    }
  else
    ::error ("invalid use of end");

  return retval;
}

octave_value_list
tree_argument_list::convert_to_const_vector (const octave_value *object)
{
  // END doesn't make sense for functions.  Maybe we need a different
  // way of asking an octave_value object this question?

  bool stash_object = (list_includes_magic_end
		       && object
		       && ! (object->is_function ()
			     || object->is_function_handle ()));

  if (stash_object)
    {
      unwind_protect::begin_frame ("convert_to_const_vector");

      unwind_protect_ptr (indexed_object);

      indexed_object = object;
    }

  int len = length ();

  // XXX FIXME XXX -- would be nice to know in advance how largs args
  // needs to be even when we have a list containing an all_va_args
  // token.

  octave_value_list args;
  int args_len = len;
  args.resize (args_len);

  iterator p = begin ();
  int j = 0;
  for (int k = 0; k < len; k++)
    {
      if (stash_object)
	{
	  unwind_protect_int (index_position);

	  index_position = (len == 1) ? -1 : k;
	}

      tree_expression *elt = *p++;

      if (elt)
	{
	  octave_value tmp = elt->rvalue ();

	  if (error_state)
	    {
	      ::error ("evaluating argument list element number %d", k+1);
	      args = octave_value_list ();
	      break;
	    }
	  else
	    {
	      if (tmp.is_all_va_args ())
		{
		  if (curr_function)
		    {
		      octave_value_list tva;
		      tva = curr_function->octave_all_va_args ();
		      int n = tva.length ();
		      args_len += n - 1;
		      args.resize (args_len);
		      for (int i = 0; i < n; i++)
			args(j++) = tva(i);
		    }
		  else
		    {
		      ::error ("all_va_args is only valid inside functions");
		      args = octave_value_list ();
		      break;
		    }
		}
	      else if (tmp.is_cs_list ())
		{
		  octave_value_list tl = tmp.list_value ();
		  int n = tl.length ();
		  args_len += n - 1;
		  args.resize (args_len);
		  for (int i = 0; i < n; i++)
		    args(j++) = tl(i);
		}
	      else
		args(j++) = tmp;
	    }
	}
      else
	{
	  args(j++) = octave_value ();
	  break;
	}
    }

  args.resize (j);

  if (stash_object)
    unwind_protect::run_frame ("convert_to_const_vector");

  return args;
}

string_vector
tree_argument_list::get_arg_names (void) const
{
  int len = length ();

  string_vector retval (len);

  int k = 0;

  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_expression *elt = *p;

      retval(k++) = elt->str_print_code ();
    }

  return retval;
}

void
tree_argument_list::accept (tree_walker& tw)
{
  tw.visit_argument_list (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
