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

#include "error.h"
#include "oct-lvalue.h"
#include "ov-struct.h"
#include "unwind-prot.h"
#include "variables.h"

DEFINE_OCTAVE_ALLOCATOR(octave_struct);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(octave_struct, "struct");

octave_value
octave_struct::do_struct_elt_index_op (const string& nm,
				       const octave_value_list& idx,
				       bool silent)
{
  // XXX DO_ME XXX
}

octave_value
octave_struct::do_struct_elt_index_op (const string& nm, bool silent)
{
  octave_value retval;

  Pix idx = map.seek (nm);

  if (idx)
    retval = map.contents (idx);
  else if (! silent)
    error ("structure has no member `%s'", nm.c_str ());

  return retval;
}

octave_lvalue
octave_struct::struct_elt_ref (octave_value *, const string& nm)
{
  return octave_lvalue (&map [nm]);
}

void
octave_struct::print (ostream& os, bool) const
{
  print_raw (os);
}

void
octave_struct::print_raw (ostream& os, bool) const
{
  // XXX FIXME XXX -- would be nice to print the output in some
  // standard order.  Maybe all substructures first, maybe
  // alphabetize entries, etc.

  unwind_protect::begin_frame ("octave_struct_print");

  unwind_protect_int (Vstruct_levels_to_print);

  if (Vstruct_levels_to_print-- > 0)
    {
      indent (os);
      os << "{";
      newline (os);

      increment_indent_level ();

      for (Pix p = map.first (); p; map.next (p))
	{
	  string key = map.key (p);
	  octave_value val = map.contents (p);

	  val.print_with_name (os, key);
	}

      decrement_indent_level ();

      indent (os);
      os << "}";
      newline (os);
    }
  else
    {
      os << " <structure>";
      newline (os);
    }

  unwind_protect::run_frame ("octave_struct_print");
}

bool
octave_struct::print_name_tag (ostream& os, const string& name) const
{
  indent (os);
  os << name << " =";
  newline (os);
  return false;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
