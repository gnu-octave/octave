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
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "pt-indir.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

// Indirect references to values (structure elements).

tree_indirect_ref::~tree_indirect_ref (void)
{
  delete expr;
}

string
tree_indirect_ref::name (void) const
{
  // ??? FIXME ???
  std::string xname = expr->name ();

  return (xname == "<unknown>") ? xname : xname + "." + nm;
}

octave_value_list
tree_indirect_ref::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for structure reference");
  else
    {
      octave_value_list tmp = expr->rvalue (nargout);

      if (tmp.empty ())
	eval_error ();
      else
	{
	  octave_value val = tmp(0).do_struct_elt_index_op (nm);

	  if (print_result () && nargout == 0 && val.is_defined ())
	    {
	      // ??? FIXME ???

	      std::string xname = name ();

	      if (xname == "<unknown>")
		bind_ans (val, true);
	      else
		val.print_with_name (octave_stdout, xname);
	    }

	  retval = val;
	}
    }

  return retval;
}

octave_value
tree_indirect_ref::rvalue (void)
{
  octave_value retval;

  octave_value_list tmp = rvalue (1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_lvalue
tree_indirect_ref::lvalue (void)
{
  octave_lvalue tmp = expr->lvalue ();

  if (tmp.is_undefined () || ! tmp.is_map ())
    tmp.define (Octave_map ());

  return tmp.struct_elt_ref (nm);
}

void
tree_indirect_ref::accept (tree_walker& tw)
{
  tw.visit_indirect_ref (*this);
}

void
tree_indirect_ref::eval_error (void) const
{
  if (error_state > 0)
    ::error ("evaluating structure reference operator near line %d, column %d",
	     line (), column ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
