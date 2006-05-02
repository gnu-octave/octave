// Template array classes
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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#include "Range.h"
#include "idx-vector.h"
#include "lo-error.h"

#if 0

template <class T>
ArrayN<T>
ArrayN<T>::value (void)
{
  ArrayN<T> retval;

  int n_idx = index_count ();

  if (n_idx > 1)
    {
      Array<idx_vector> ra_idx (n_idx);

      idx_vector *tmp = get_idx ();

      for (int i = 0; i < n_idx; i++)
	ra_idx(i) = tmp[i];

      return index (ra_idx);
    }
  else if (n_idx == 1)
    {
      idx_vector *tmp = get_idx ();

      idx_vector ra_idx = tmp[0];

      return index (ra_idx);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of indices for array expression");

  clear_index ();

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
