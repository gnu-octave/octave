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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-sstream.h"
#include "lo-utils.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ov-cell.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"

template class octave_base_matrix<Cell>;

DEFINE_OCTAVE_ALLOCATOR (octave_cell);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cell, "cell");

octave_value
octave_cell::subsref (const std::string& type,
		      const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
      {
	octave_value tmp = do_index_op (idx.front ());

	Cell tcell = tmp.cell_value ();

	if (tcell.length () == 1)
	  retval = tcell(0,0);
	else
	  {
	    int nr = tcell.rows ();
	    int nc = tcell.columns ();
	    octave_value_list lst (nr * nc, octave_value ());
	    int k = 0;
	    for (int j = 0; j < nc; j++)
	      for (int i = 0; i < nr; i++)
		{
		  OCTAVE_QUIT;
		  lst(k++) = tcell(i,j);
		}
	    retval = octave_value (lst, true);
	  }
      }
      break;

    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

octave_value
octave_cell::subsasgn (const std::string& type,
		       const std::list<octave_value_list>& idx,
		       const octave_value& rhs)
{
  octave_value retval;

  int n = type.length ();

  octave_value t_rhs = rhs;

  if (n > 1)
    {
      switch (type[0])
	{
	case '(':
	  {
	    octave_value tmp = do_index_op (idx.front (), true);

	    if (! tmp.is_defined ())
	      tmp = octave_value::empty_conv (type.substr (1), rhs);

	    if (! error_state)
	      {
		std::list<octave_value_list> next_idx (idx);

		next_idx.erase (next_idx.begin ());

		tmp.make_unique ();

		t_rhs = tmp.subsasgn (type.substr (1), next_idx, rhs);
	      }
	  }
	  break;

	case '{':
	  {
	    octave_value tmp = do_index_op (idx.front (), true);

	    if (! tmp.is_defined ())
	      tmp = octave_value::empty_conv (type.substr (1), rhs);

	    if (! error_state)
	      {
		Cell tcell = tmp.cell_value ();

		if (! error_state && tcell.length () == 1)
		  {
		    tmp = tcell(0,0);

		    std::list<octave_value_list> next_idx (idx);

		    next_idx.erase (next_idx.begin ());

		    tmp.make_unique ();

		    if (! tmp.is_defined () || tmp.is_empty ())
		      tmp = octave_value::empty_conv (type.substr (1), rhs);

		    if (! error_state)
		      t_rhs = tmp.subsasgn (type.substr (1), next_idx, rhs);
		  }
	      }
	  }
	  break;

	case '.':
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
	  break;

	default:
	  panic_impossible ();
	}
    }

  if (! error_state)
    {
      switch (type[0])
	{
	case '(':
	  {
	    octave_value_list i = idx.front ();

	    if (t_rhs.is_cell ())
	      octave_base_matrix<Cell>::assign (i, t_rhs.cell_value ());
	    else
	      octave_base_matrix<Cell>::assign (i, Cell (t_rhs));

	    retval = octave_value (this, count + 1);
	  }
	  break;

	case '{':
	  {
	    octave_value_list i = idx.front ();

	    octave_base_matrix<Cell>::assign (i, Cell (t_rhs));

	    retval = octave_value (this, count + 1);
	  }
	  break;

	case '.':
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
	  break;

	default:
	  panic_impossible ();
	}
    }

  return retval;
}

void
octave_cell::assign (const octave_value_list& idx, const octave_value& rhs)
{
  if (rhs.is_cell ())
    octave_base_matrix<Cell>::assign (idx, rhs.cell_value ());
  else
    octave_base_matrix<Cell>::assign (idx, Cell (rhs));
}

octave_value_list
octave_cell::list_value (void) const
{
  octave_value_list retval;

  int nr = rows ();
  int nc = columns ();

  if (nr == 1 && nc > 0)
    {
      retval.resize (nc);

      for (int i = 0; i < nc; i++)
	retval(i) = matrix(0,i);
    }
  else if (nc == 1 && nr > 0)
    {
      retval.resize (nr);

      for (int i = 0; i < nr; i++)
	retval(i) = matrix(i,0);
    }
  else
    error ("invalid conversion from cell array to list");

  return retval;
}

string_vector
octave_cell::all_strings (bool pad, bool) const
{
  string_vector retval;

  int nr = rows ();
  int nc = columns ();

  int n_elts = 0;

  int max_len = 0;

  if (pad)
    {
      for (int j = 0; j < nc; j++)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      string_vector s = matrix(i,j).all_strings ();

	      if (error_state)
		return retval;

	      n_elts += s.length ();

	      int s_max_len = s.max_length ();

	      if (s_max_len > max_len)
		max_len = s_max_len;
	    }
	}
    }

  retval.resize (n_elts);

  int k = 0;

  for (int j = 0; j < nc; j++)
    {
      for (int i = 0; i < nr; i++)
	{
	  string_vector s = matrix(i,j).all_strings ();

	  int n = s.length ();

	  for (int ii = 0; ii < n; ii++)
	    {
	      std::string t = s[ii];
	      int t_len = t.length ();

	      if (pad && max_len > t_len)
		t += std::string (max_len - t_len, ' ');

	      retval[k++] = t;
	    }
	}
    }



  return retval;
}

void
octave_cell::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_cell::print_raw (std::ostream& os, bool) const
{
  int ndims = matrix.ndims ();

  if (ndims == 2)
    {
      int nr = rows ();
      int nc = columns ();

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
		  OCTAVE_QUIT;

		  OSSTREAM buf;
		  buf << "[" << i+1 << "," << j+1 << "]" << OSSTREAM_ENDS;

		  octave_value val = matrix(i,j);

		  val.print_with_name (os, OSSTREAM_STR (buf));

		  OSSTREAM_FREEZE (buf);
		}
	    }

	  decrement_indent_level ();

	  indent (os);
	  os << "}";
	  newline (os);
	}
      else
	{
	  os << "{}";
	  if (nr > 0 || nc > 0)
	    os << "(" << nr << "x" << nc << ")";
	  os << "\n";
	}
    }
  else
    {
      indent (os);
      os << "{";
      dim_vector dv = matrix.dims ();
      for (int i = 0; i < ndims; i++)
	{
	  if (i > 0)
	    os << "x";
	  os << dv(i);
	}
      os << " Cell Array}";
      newline (os);
    }
}

bool
octave_cell::print_name_tag (std::ostream& os, const std::string& name) const
{
  indent (os);

  int nr = rows ();
  int nc = columns ();

  if (nr > 0 && nc > 0)
    {
      os << name << " =";
      newline (os);
    }
  else
    os << name << " = ";

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

DEFUN (cellstr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cellstr (@var{string})\n\
Create a new cell array object from the elements of the string\n\
array @var{string}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      string_vector s = args(0).all_strings ();

      if (! error_state)
	retval = Cell (s);
      else
	error ("cellstr: expecting argument to be a string");
    }
  else
    print_usage ("cellstr");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
