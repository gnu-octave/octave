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

#include <iostream>
#include <strstream>
#include <string>

#include "str-vec.h"

#include "error.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-usr-fcn.h"
#include "pt-arg-list.h"
#include "pt-exp.h"
#include "pt-pr-code.h"
#include "pt-walk.h"
#include "toplev.h"

// Argument lists.

tree_argument_list::~tree_argument_list (void)
{
  while (! empty ())
    {
      tree_expression *t = remove_front ();
      delete t;
    }
}

bool
tree_argument_list::all_elements_are_constant (void) const
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_expression *elt = this->operator () (p);

      if (! elt->is_constant ())
	return false;
    }

  return true;
}

octave_value_list
tree_argument_list::convert_to_const_vector (void)
{
  int len = length ();

  // XXX FIXME XXX -- would be nice to know in advance how largs args
  // needs to be even when we have a list containing an all_va_args
  // token.

  octave_value_list args;
  args.resize (len);

  Pix p = first ();
  int j = 0;
  for (int k = 0; k < len; k++)
    {
      tree_expression *elt = this->operator () (p);

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
	      else
		args(j++) = tmp;
	    }
	  next (p);
	}
      else
	{
	  args(j++) = octave_value ();
	  break;
	}
    }

  args.resize (j);

  return args;
}

string_vector
tree_argument_list::get_arg_names (void) const
{
  int len = length ();

  string_vector retval (len);

  int k = 0;

  for (Pix p = first (); p; next (p))
    {
      tree_expression *elt = this->operator () (p);

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
