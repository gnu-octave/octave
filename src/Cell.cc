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

#include "idx-vector.h"

#include "Cell.h"

Cell::Cell (const string_vector& sv)
  : ArrayN<octave_value> ()
{
  int n = sv.length ();

  if (n > 0)
    {
      resize_no_fill (n, 1);

      for (int i = 0; i < n; i++)
	elem(i,0) = sv[i];
    }
}

Cell
Cell::index (const octave_value_list& idx, bool resize_ok) const
{
  Cell retval;

  int n = idx.length ();

  switch (n)
    {
    case 1:
      {
	idx_vector i = idx(0).index_vector ();

	retval = index (i, resize_ok);
      }
      break;

    case 2:
      {
	idx_vector i = idx(0).index_vector ();
	idx_vector j = idx(1).index_vector ();

	retval = index (i, j, resize_ok);
      }
      break;

    default:
      {
	Array<idx_vector> iv (n);

	for (int i = 0; i < n; i++)
	  iv(i) = idx(i).index_vector ();

	retval = index (iv, resize_ok);
      }
      break;
    }

  return retval;
}

Cell&
Cell::assign (const octave_value_list& idx, const Cell& rhs,
	      const octave_value& fill_val)

{
  for (int i = 0; i < idx.length (); i++)
    set_index (idx(i).index_vector ());

  ::assign (*this, rhs, fill_val);

  return *this;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
