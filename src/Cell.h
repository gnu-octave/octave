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

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "Array2.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "ov.h"

class
Cell
{
public:

  Cell (void)
    : data () { }

  Cell (int n, int m, const octave_value& val = octave_value ())
    : data (n, m, val) { }

  Cell (const Cell& c)
    : data (c.data) { }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  Cell& operator = (const Cell& c)
    {
      if (this != &c)
	data = c.data;

      return *this;
    }

  int rows (void) const { return data.rows (); }

  int columns (void) const { return data.columns (); }

  octave_value& operator () (int i, int j) { return elem (i, j); }

  octave_value operator () (int i, int j) const { return elem (i, j); }

  octave_value& elem (int i, int j) { return data.elem (i, j); }

  octave_value elem (int i, int j) const { return data.elem (i, j); }

private:

  static octave_allocator allocator;

  Array2<octave_value> data;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
