/*

Copyright (C) 1999 John W. Eaton

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

#include "lo-utils.h"

#include "defun.h"
#include "error.h"
#include "ov-cell.h"
#include "unwind-prot.h"

DEFINE_OCTAVE_ALLOCATOR (octave_cell);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cell, "cell");

octave_value
octave_cell::do_index_op (const octave_value_list& idx)
{
  octave_value retval;

#if 0
  if (idx.length () == 1)
    {
      idx_vector i = idx (0).index_vector ();

      retval = octave_value_list (lst.index (i));
    }
  else
    error ("lists may only be indexed by a single scalar");
#endif

  return retval;
}

void
octave_cell::assign (const octave_value_list& idx, const octave_value& rhs)
{
#if 0
  if (idx.length () == 1)
    {
      int i = idx(0).int_value (true);

      if (! error_state)
	{
	  int n = lst.length ();

	  if (i > 0 && (Vresize_on_range_error || i <= n))
	    lst(i-1) = rhs;
	  else
	    error ("list index = %d out of range", i);
	}
      else
	error ("list index must be an integer");
    }
  else
    error ("lists may only be indexed by a single scalar");
#endif
}

void
octave_cell::print (ostream& os, bool) const
{
  print_raw (os);
}

void
octave_cell::print_raw (ostream& os, bool) const
{
  unwind_protect::begin_frame ("octave_cell_print");

  int nr = cell_val.rows ();
  int nc = cell_val.columns();

  if (nr > 0 && nc > 0)
    {
      indent (os);
      os << "{";
      newline (os);

      increment_indent_level ();

      for (int j = 0; j < nc; j++)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      ostrstream buf;
	      buf << "[" << i+1 << "," << j+1 << "]" << ends;
	      const char *nm = buf.str ();

	      octave_value val = cell_val(i,j);

	      val.print_with_name (os, nm);

	      delete [] nm;
	    }
	}

      decrement_indent_level ();

      indent (os);

      os << "}";
    }
  else
    os << "{}";

  newline (os);

  unwind_protect::run_frame ("octave_cell_print");
}

bool
octave_cell::print_name_tag (ostream& os, const string& name) const
{
  indent (os);
  if (is_empty ())
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
    }
  return false;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
