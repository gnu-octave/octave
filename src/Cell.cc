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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "idx-vector.h"

#include "Cell.h"
#include "error.h"

Cell::Cell (const string_vector& sv)
  : ArrayN<octave_value> ()
{
  octave_idx_type n = sv.length ();

  if (n > 0)
    {
      resize (dim_vector (n, 1));

      for (octave_idx_type i = 0; i < n; i++)
	elem(i,0) = sv[i];
    }
}

Cell
Cell::index (const octave_value_list& idx_arg, bool resize_ok) const
{
  Cell retval;

  octave_idx_type n = idx_arg.length ();

  switch (n)
    {
    case 0:
      retval = *this;
      break;

    case 1:
      {
	idx_vector i = idx_arg(0).index_vector ();

	if (! error_state)
	  retval = index (i, resize_ok);
      }
      break;

    case 2:
      {
	idx_vector i = idx_arg(0).index_vector ();

	if (! error_state)
	  {
	    idx_vector j = idx_arg(1).index_vector ();

	    if (! error_state)
	      retval = index (i, j, resize_ok);
	  }
      }
      break;

    default:
      {
	Array<idx_vector> iv (n);

	for (octave_idx_type i = 0; i < n; i++)
	  {
	    iv(i) = idx_arg(i).index_vector ();

	    if (error_state)
	      break;
	  }

	if (!error_state)
	  retval = index (iv, resize_ok);
      }
      break;
    }

  return retval;
}

Cell&
Cell::assign (const octave_value_list& idx_arg, const Cell& rhs,
	      const octave_value& fill_val)

{
  for (octave_idx_type i = 0; i < idx_arg.length (); i++)
    set_index (idx_arg(i).index_vector ());

  ::assign (*this, rhs, fill_val);

  return *this;
}

Cell
Cell::concat (const Cell& rb, const Array<octave_idx_type>& ra_idx)
{
  return insert (rb, ra_idx);
}

Cell&
Cell::insert (const Cell& a, octave_idx_type r, octave_idx_type c)
{
  Array<octave_value>::insert (a, r, c);
  return *this;
}

Cell&
Cell::insert (const Cell& a, const Array<octave_idx_type>& ra_idx)
{
  Array<octave_value>::insert (a, ra_idx);
  return *this;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
