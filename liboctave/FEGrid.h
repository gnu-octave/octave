// FEGrid.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_FEGrid_h)
#define octave_FEGrid_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dColVector.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class FEGrid
{
private:

  void error (const char* msg) const;
  void nel_error (void) const;

  void check_grid (void) const;

public:

  FEGrid (void) {}
  FEGrid (const Vector& elbnds) { elem = elbnds; check_grid (); }
  FEGrid (int nel, double width);
  FEGrid (int nel, double left, double right);

  int element (double x) const;

  double left (void) const { return elem.elem (0); }
  double right (void) const { return elem.elem (elem.capacity () - 1); }

  int in_bounds (double x) const
    { return (x >= left () && x <= right ()); }

  Vector element_boundaries (void) const { return elem; }

  friend ostream& operator << (ostream&, const FEGrid&);

protected:

  Vector elem;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
