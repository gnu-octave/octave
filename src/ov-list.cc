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

#include <iostream.h>
#include <strstream.h>

#include "lo-utils.h"

#include "defun.h"
#include "error.h"
#include "help.h"
#include "ov-list.h"
#include "unwind-prot.h"

octave_allocator
octave_list::allocator (sizeof (octave_list));

int
octave_list::t_id (-1);

const string
octave_list::t_name ("list");

octave_value
octave_list::do_index_op (const octave_value_list& idx)
{
  octave_value retval;

  if (idx.length () == 1)
    {
      double d = idx(0).double_value ();

      if (! error_state)
	{
	  if (D_NINT (d) == d)
	    {
	      int n = lst.length ();

	      int i = static_cast<int> (d);

	      if (i > 0 && i <= n)
		retval = lst(i-1);
	      else
		error ("list index = %d out of range", i);
	    }
	  else
	    error ("list index must be an integer");
	}
    }
  else
    error ("lists may only be indexed by a single scalar");

  return retval;
}

void
octave_list::print (ostream& os, bool) const
{
  print_raw (os);
}

void
octave_list::print_raw (ostream& os, bool) const
{
  unwind_protect::begin_frame ("octave_list_print");

  indent (os);
  os << "(";
  newline (os);

  increment_indent_level ();

  int n = lst.length ();

  for (int i = 0; i < n; i++)
    {
      ostrstream buf;
      buf << "[" << i+1 << "]" << ends;
      const char *nm = buf.str ();

      octave_value val = lst(i);

      val.print_with_name (os, nm);

      delete [] nm;
    }

  decrement_indent_level ();

  indent (os);
  os << ")";
  newline (os);

  unwind_protect::run_frame ("octave_list_print");
}

bool
octave_list::print_name_tag (ostream& os, const string& name) const
{
  indent (os);
  os << name << " =";
  newline (os);
  return false;
}

DEFUN (make_list, args, ,
  "make_list (ARGS)\n\
\n\
Create a new list from ARGS.")
{
  return octave_value (args);
}

DEFUN (append, args, ,
  "append (LIST, ARGS)\n\
\n\
Return a new list created by appending ARGS to LIST")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      octave_value_list tmp = args(0).list_value ();

      if (! error_state)
	{
	  for (int i = 1; i < nargin; i++)
	    tmp.append (args(i));

	  retval = tmp;
	}
    }
  else
    print_usage ("append");

  return retval;
}

DEFUN (reverse, args, ,
  "reverse (LIST)\n\
\n\
Return a new list created by reversing the elements of LIST")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value_list tmp = args(0).list_value ();

      if (! error_state)
	  retval = tmp.reverse ();
    }
  else
    print_usage ("reverse");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
