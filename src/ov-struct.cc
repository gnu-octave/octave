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

#include "error.h"
#include "ov-struct.h"
#include "unwind-prot.h"

int octave_struct::t_id = -1;

const string octave_struct::t_name ("struct");


octave_value
octave_struct::struct_elt_val (const string& nm, bool silent) const
{
  octave_value retval;

  Pix idx = map.seek (nm);

  if (idx)
    retval = map.contents (idx);
  else if (! silent)
    error ("structure has no member `%s'", nm.c_str ());

  return retval;
}

octave_value&
octave_struct::struct_elt_ref (const string& nm)
{
  return map [nm];
}

void
octave_struct::print (ostream& os, bool)
{
  // XXX FIXME XXX -- would be nice to print the output in some
  // standard order.  Maybe all substructures first, maybe
  // alphabetize entries, etc.

  begin_unwind_frame ("octave_struct_print");

  unwind_protect_int (struct_indent);
  unwind_protect_int (Vstruct_levels_to_print);

  if (Vstruct_levels_to_print-- > 0)
    {
      os.form ("\n%*s{\n", struct_indent, "");

      increment_struct_indent ();

      Pix p = map.first ();

      while (p)
	{
	  bool pad_after = false;

	  string key = map.key (p);
	  octave_value val = map.contents (p);

	  map.next (p);

	  os.form ("%*s%s =", struct_indent, "", key.c_str ());

	  if (val.print_as_scalar ())
	    os << " ";
	  else if (val.is_map ())
	    {
	      if (p)
		pad_after = true;
	    }
	  else
	    {
	      if (p)
		pad_after = true;

	      os << "\n\n";
	    }

	  val.print (os);

	  if (pad_after)
	    os << "\n";
	}

      decrement_struct_indent ();

      os.form ("%*s%s", struct_indent, "", "}\n");
    }
  else
    os << " <structure>\n";

  run_unwind_frame ("octave_struct_print");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
