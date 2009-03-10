/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_FEGrid_h)
#define octave_FEGrid_h 1

#include <iosfwd>

#include "dColVector.h"

class
FEGrid
{
public:

  FEGrid (void)
    : elem () { }

  FEGrid (const ColumnVector& elbnds)
    : elem (elbnds) { check_grid (); }

  FEGrid (octave_idx_type nel, double width);

  FEGrid (octave_idx_type nel, double left, double right);

  FEGrid (const FEGrid& a)
    : elem (a.elem) { }

  FEGrid& operator = (const FEGrid& a)
    {
      if (this != &a)
	elem = a.elem;

      return *this;
    }

  ~FEGrid (void) { }

  octave_idx_type element (double x) const;

  double left (void) const { return elem.elem (0); }

  double right (void) const { return elem.elem (elem.capacity () - 1); }

  int in_bounds (double x) const { return (x >= left () && x <= right ()); }

  ColumnVector element_boundaries (void) const { return elem; }

  friend std::ostream& operator << (std::ostream&, const FEGrid&);

protected:

  ColumnVector elem;

private:

  void error (const char* msg) const;
  void nel_error (void) const;

  void check_grid (void) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
