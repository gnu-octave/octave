// FEGrid.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (_FEGrid_h)
#define _FEGrid_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "Matrix.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class FEGrid
{
public:

  FEGrid (void);
  FEGrid (const Vector& elbnds);
  FEGrid (int nel, double width);
  FEGrid (int nel, double left, double right);

  int in_bounds (double x) const;

  int element (double x) const;

  double left (void) const;
  double right (void) const;

  Vector element_boundaries (void) const;

  friend ostream& operator << (ostream&, const FEGrid&);

protected:

  Vector elem;

private:

  void error (const char* msg) const;
  void nel_error (void) const;

  void check_grid (void) const;
};

inline FEGrid::FEGrid (void) {}

inline FEGrid::FEGrid (const Vector& elbnds)
  { elem = elbnds; check_grid (); }

inline int FEGrid::in_bounds (double x) const
  { return (x >= left () && x <= right ()); }

inline double FEGrid::left (void) const
  { return elem.elem (0); }

inline double FEGrid::right (void) const
  { return elem.elem (elem.capacity () - 1); }

inline Vector FEGrid::element_boundaries (void) const
  { return elem; }

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
