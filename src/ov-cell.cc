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

#include <iostream>
#include <strstream>

#include "lo-utils.h"

#include "defun.h"
#include "error.h"
#include "ov-cell.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_cell);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cell, "cell");

octave_value
octave_cell::do_index_op (const octave_value_list& idx)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = cell_val.index (i, j);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = cell_val.index (i);
      }
      break;

    default:
      {
	std::string n = type_name ();

	error ("invalid number of indices (%d) for %s value",
	       len, n.c_str ());
      }
      break;
    }

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
octave_cell::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_cell::print_raw (std::ostream& os, bool) const
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
	      std::ostrstream buf;
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
octave_cell::print_name_tag (std::ostream& os, const std::string& name) const
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

DEFUN (iscell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscell (@var{x})\n\
Return true if @var{x} is a cell array object.  Otherwise, return\n\
false.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_cell ();
  else
    print_usage ("iscell");

  return retval;
}

DEFUN (cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cell (@var{x})\n\
@deftypefnx {Built-in Function} {} cell (@var{n}, @var{m})\n\
Create a new cell array object.  If invoked with a single scalar\n\
argument, @code{cell} returns a square cell array with the dimension\n\
specified.  If you supply two scalar arguments, @code{cell} takes\n\
them to be the number of rows and columns.  If given a vector with two\n\
elements, @code{cell} uses the values of the elements as the number of\n\
rows and columns, respectively.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 1:
      {
	int nr, nc;
	get_dimensions (args(0), "cell", nr, nc);

	if (! error_state)
	  retval = Cell (nr, nc, Matrix ());
      }
      break;

    case 2:
      {
	int nr, nc;
	get_dimensions (args(0), args(1), "cell", nr, nc);

	if (! error_state)
	  retval = Cell (nr, nc, Matrix ());
      }
      break;

    default:
      print_usage ("cell");
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
