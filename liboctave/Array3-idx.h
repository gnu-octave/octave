// Template array classes
/*

Copyright (C) 1996 John W. Eaton

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

#include "Array-flags.h"
#include "idx-vector.h"
#include "lo-error.h"

template <class T>
void
Array3<T>::maybe_delete_elements (idx_vector&, idx_vector&, idx_vector&)
{
  assert (0);
}

template <class T>
Array3<T>
Array3<T>::value (void)
{
  Array3<T> retval;
  assert (0);
  return retval;
}

template <class LT, class RT>
int
assign (Array3<LT>&, const Array3<RT>&)
{
  assert (0);
  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
