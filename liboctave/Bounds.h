// Bounds.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_Bounds_h)
#define octave_Bounds_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dColVector.h"

extern "C++" {

#ifndef Vector
#define Vector ColumnVector
#endif

class Bounds
{
public:

  Bounds (void);
  Bounds (int n);
  Bounds (const Vector lb, const Vector ub);
  Bounds (const Bounds& a);

  Bounds& operator = (const Bounds& a);

  Bounds& resize (int n);

  double lower_bound (int index) const;
  double upper_bound (int index) const;

  Vector lower_bounds (void) const;
  Vector upper_bounds (void) const;

  int size (void) const;

  Bounds& set_bound (int index, double low, double high);

  Bounds& set_bounds (double low, double high);
  Bounds& set_bounds (const Vector lb, const Vector ub);

  Bounds& set_lower_bound (int index, double low);
  Bounds& set_upper_bound (int index, double high);

  Bounds& set_lower_bounds (double low);
  Bounds& set_upper_bounds (double high);

  Bounds& set_lower_bounds (const Vector lb);
  Bounds& set_upper_bounds (const Vector ub);

  friend ostream& operator << (ostream& os, const Bounds& b);

protected:

  Vector lb;
  Vector ub;

  int nb;

private:

  void error (const char *msg);

};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
