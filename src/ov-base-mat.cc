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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "Cell.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "pr-output.h"

template <class MT>
octave_value
octave_base_matrix<MT>::subsref (const std::string& type,
				 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
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

template <class MT>
octave_value
octave_base_matrix<MT>::subsasgn (const std::string& type,
				  const std::list<octave_value_list>& idx,
				  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
	if (type.length () == 1)
	  retval = numeric_assign (type, idx, rhs);
	else if (is_empty ())
	  {
	    // Allow conversion of empty matrix to some other type in
	    // cases like
	    //
	    //  x = []; x(i).f = rhs

	    octave_value tmp = octave_value::empty_conv (type, rhs);

	    retval = tmp.subsasgn (type, idx, rhs);
	  }
	else
	  {
	    std::string nm = type_name ();
	    error ("in indexed assignment of %s, last lhs index must be ()",
		   nm.c_str ());
	  }
      }
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class MT>
octave_value
octave_base_matrix<MT>::do_index_op (const octave_value_list& idx,
				     int resize_ok)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = MT (matrix.index (i, j, resize_ok, MT::resize_fill_value ()));
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = MT (matrix.index (i, resize_ok, MT::resize_fill_value ()));
      }
      break;

    default:
      {
	Array<idx_vector> idx_vec (len);

	for (int i = 0; i < len; i++)
	  idx_vec(i) = idx(i).index_vector ();

	retval = MT (matrix.index (idx_vec, resize_ok,
				   MT::resize_fill_value ()));
      }
      break;
    }

  return retval;
}

template <class MT>
void
octave_base_matrix<MT>::assign (const octave_value_list& idx, const MT& rhs)
{
  int len = idx.length ();

  for (int i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, rhs, MT::resize_fill_value ());
}

template <class MT>
bool
octave_base_matrix<MT>::is_true (void) const
{
  bool retval = false;

  if (rows () > 0 && columns () > 0)
    {
      boolMatrix m = (matrix.all () . all ());

      retval = (m.rows () == 1 && m.columns () == 1 && m(0,0));
    }

  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::print_as_scalar (void) const
{
  int nr = rows ();
  int nc = columns ();

  return (nr == 1 && nc == 1 || (nr == 0 || nc == 0));
}

template <class MT>
void
octave_base_matrix<MT>::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class MT>
void
octave_base_matrix<MT>::print_raw (std::ostream& os,
				   bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

template <class MT>
bool
octave_base_matrix<MT>::print_name_tag (std::ostream& os,
					const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (print_as_scalar ())
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      newline (os);
      retval = true;
    }

  return retval;
}

template <class MT>
void
octave_base_matrix<MT>::print_info (std::ostream& os,
				    const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
