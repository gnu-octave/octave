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

#if !defined (Cell_h)
#define Cell_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <string>

#include "Array2.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "oct-obj.h"

class
Cell : public Array2<octave_value>
{
public:

  Cell (void)
    : Array2<octave_value> () { }

  Cell (const octave_value& val)
    : Array2<octave_value> (1, 1, val) { }

  Cell (int n, int m, const octave_value& val = resize_fill_value ())
    : Array2<octave_value> (n, m, val) { }

  Cell (const Array2<octave_value>& c)
    : Array2<octave_value> (c) { }

  Cell (const Array<octave_value>& c, int nr, int nc)
    : Array2<octave_value> (c, nr, nc) { }

  Cell (const string_vector& sv);

  Cell (const Cell& c)
    : Array2<octave_value> (c) { }

  // XXX FIXME XXX
  boolMatrix all (int dim = 0) const { return boolMatrix (); }

  // XXX FIXME XXX
  boolMatrix any (int dim = 0) const { return boolMatrix (); }

  // XXX FIXME XXX
  bool is_true (void) const { return false; }

  static octave_value resize_fill_value (void) { return Matrix (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
