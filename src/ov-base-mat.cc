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

#include "oct-obj.h"
#include "ov-base.h"
#include "ov-base-mat.h"

template <class MT>
octave_value
octave_base_matrix<MT>::do_index_op (const octave_value_list& idx)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = MT (matrix.index (i, j));
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = MT (matrix.index (i));
      }
      break;

    default:
      {
	string n = type_name ();

	error ("invalid number of indices (%d) for %s value",
	       len, n.c_str ());
      }
      break;
    }

  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::is_true (void) const
{
  bool retval = false;

  if (rows () == 0 || columns () == 0)
    {
      int flag = Vpropagate_empty_matrices;

      if (flag < 0)
	warning ("empty matrix used in conditional expression");
      else if (flag == 0)
	error ("empty matrix used in conditional expression");
    }
  else
    {
      boolMatrix m = (matrix.all ()) . all ();

      retval = (m.rows () == 1 && m.columns () == 1 && m (0, 0) != 0.0);
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
octave_base_matrix<MT>::print (ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class MT>
void
octave_base_matrix<MT>::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level ());
}

template <class MT>
bool
octave_base_matrix<MT>::print_name_tag (ostream& os, const string& name) const
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
