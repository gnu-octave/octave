/*

Copyright (C) 2000 John W. Eaton

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

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "oct-obj.h"
#include "ov-base.h"
#include "ov-base-nd-array.h"
#include "pr-output.h"

static inline Array<idx_vector>
idx_list_to_idx_array (const octave_value_list& idx)
{
  int n = idx.length ();

  Array<idx_vector> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = idx(i).index_vector ();

  return retval;
}

template <class AT>
octave_value
octave_base_nd_array<AT>::do_index_op (const octave_value_list& idx,
				       int resize_ok)
{
  octave_value retval;

  int len = idx.length ();

  if (len > 1)
    {
      Array<idx_vector> i = idx_list_to_idx_array (idx);

      retval
	= octave_value (new octave_base_nd_array<AT> (AT (array.index (i, resize_ok))));
    }
  else if (len == 1)
    {
      idx_vector i = idx(0).index_vector ();

      retval
	= octave_value (new octave_base_nd_array<AT> (AT (array.index (i, resize_ok))));
    }
  else
    {
      std::string n = type_name ();

      error ("invalid number of indices (%d) for %s value",
	     len, n.c_str ());
    }

  return retval;
}

template <class AT>
bool
octave_base_nd_array<AT>::is_true (void) const
{
  // XXX FIXME XXX
  return false;
}

template <class AT>
bool
octave_base_nd_array<AT>::print_as_scalar (void) const
{
  // XXX FIXME XXX
  return false;
}

template <class AT>
void
octave_base_nd_array<AT>::print (std::ostream& os,
				 bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class AT>
void
octave_base_nd_array<AT>::print_raw (std::ostream& os,
				     bool pr_as_read_syntax) const
{
  // XXX FIXME XX
  os << array;
#if 0
  octave_print_internal (os, array, pr_as_read_syntax,
			 current_print_indent_level ());
#endif
}

template <class AT>
bool
octave_base_nd_array<AT>::print_name_tag (std::ostream& os,
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
