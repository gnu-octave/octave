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
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-sym.h"
#include "oct-var-ref.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-id.h"
#include "pt-indir.h"
#include "pt-walk.h"
#include "symtab.h"
#include "utils.h"

// Indirect references to values (structure elements).

tree_indirect_ref::~tree_indirect_ref (void)
{
  if (! preserve_ident)
    delete id;

  if (! preserve_indir)
    delete indir;
}

void
tree_indirect_ref::mark_for_possible_ans_assign (void)
{
  maybe_do_ans_assign = true;

  if (is_identifier_only ())
    id->mark_for_possible_ans_assign ();
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
	    retval.print_with_name (octave_stdout, name ());
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

octave_variable_reference
tree_indirect_ref::reference (void)
{
  if (is_identifier_only ())
    return id->reference ();
  else
    {
      octave_variable_reference tmp;

      if (id)
	tmp = id->reference ();
      else if (indir)
	tmp = indir->reference ();
      else
	panic_impossible ();

      if (tmp.is_undefined ())
	tmp.define (Octave_map ());

      return tmp.struct_elt_ref (nm);
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
