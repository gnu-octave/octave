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

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SLList.h>

#include "error.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-walk.h"

// Parameter lists.

tree_parameter_list::~tree_parameter_list (void)
{
  while (! empty ())
    {
      tree_identifier *t = remove_front ();
      delete t;
    }
}

void
tree_parameter_list::mark_as_formal_parameters (void)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);
      elt->mark_as_formal_parameter ();
    }
}

void
tree_parameter_list::initialize_undefined_elements (octave_value& val)
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (! elt->is_defined ())
	{
	  octave_lvalue tmp = elt->lvalue ();

	  tmp.assign (octave_value::op_asn_eq, val);
	}
    }
}

void
tree_parameter_list::define_from_arg_vector (const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin <= 0)
    return;

  int expected_nargin = length ();

  Pix p = first ();

  for (int i = 0; i < expected_nargin; i++)
    {
      tree_identifier *elt = this->operator () (p);

      octave_lvalue ref = elt->lvalue ();

      if (i < nargin)
	{
	  if (args(i).is_defined () && args(i).is_magic_colon ())
	    {
	      ::error ("invalid use of colon in function argument list");
	      return;
	    }

	  ref.assign (octave_value::op_asn_eq, args(i));
	}
      else
	ref.assign (octave_value::op_asn_eq, octave_value ());

      next (p);
    }
}

void
tree_parameter_list::clear (void)
{
  int len = length ();

  Pix p = first ();

  for (int i = 0; i < len; i++)
    {
      tree_identifier *elt = this->operator () (p);

      octave_lvalue ref = elt->lvalue ();

      ref.assign (octave_value::op_asn_eq, octave_value ());

      next (p);
    }
}

octave_value_list
tree_parameter_list::convert_to_const_vector (tree_va_return_list *vr_list)
{
  int nout = length ();

  if (vr_list)
    nout += vr_list->length ();

  octave_value_list retval;
  retval.resize (nout);

  int i = 0;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (elt->is_defined ())
	retval(i) = elt->rvalue ();

      i++;
    }

  if (vr_list)
    {
      for (Pix p = vr_list->first (); p != 0; vr_list->next (p))
	{
	  retval(i) = vr_list->operator () (p);
	  i++;
	}
    }

  return retval;
}

bool
tree_parameter_list::is_defined (void)
{
  bool status = true;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_identifier *elt = this->operator () (p);

      if (! elt->is_defined ())
	{
	  status = false;
	  break;
	}
    }

  return status;
}

void
tree_parameter_list::accept (tree_walker& tw)
{
  tw.visit_parameter_list (*this);
}

// Return lists.

tree_return_list::~tree_return_list (void)
{
  while (! empty ())
    {
      tree_index_expression *t = remove_front ();
      delete t;
    }
}

void
tree_return_list::accept (tree_walker& tw)
{
  tw.visit_return_list (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
