// Bounds.h                                                -*- C++ -*-
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

#if !defined (octave_Bounds_h)
#define octave_Bounds_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dColVector.h"

class Bounds
{
public:

  Bounds (void) { nb = 0; }

  Bounds (int n) : lb (nb, 0.0), ub (nb, 0.0) { nb = n; }

  Bounds (const ColumnVector lb, const ColumnVector ub);

  Bounds (const Bounds& a)
    {
      nb = a.size ();
      lb = a.lower_bounds ();
      ub = a.upper_bounds ();
    }

  Bounds& operator = (const Bounds& a)
    {
      nb = a.size ();
      lb = a.lower_bounds ();
      ub = a.upper_bounds ();

      return *this;
    }

  Bounds& resize (int n)
    {
      nb = n;
      lb.resize (nb);
      ub.resize (nb);

      return *this;
    }

  double lower_bound (int index) const { return lb.elem (index); }
  double upper_bound (int index) const { return ub.elem (index); }

  ColumnVector lower_bounds (void) const { return lb; }
  ColumnVector upper_bounds (void) const { return ub; }

  int size (void) const { return nb; }

  Bounds& set_bound (int index, double low, double high)
    {
      lb.elem (index) = low;
      ub.elem (index) = high;
      return *this;
    }

  Bounds& set_bounds (double low, double high)
    {
      lb.fill (low);
      ub.fill (high);
      return *this;
    }

  Bounds& set_bounds (const ColumnVector lb, const ColumnVector ub);

  Bounds& set_lower_bound (int index, double low)
    {
      lb.elem (index) = low;
      return *this;
    }

  Bounds& set_upper_bound (int index, double high)
    {
      ub.elem (index) = high;
      return *this;
    }

  Bounds& set_lower_bounds (double low)
    {
      lb.fill (low);
      return *this;
    }

  Bounds& set_upper_bounds (double high)
    {
      ub.fill (high);
      return *this;
    }

  Bounds& set_lower_bounds (const ColumnVector lb);
  Bounds& set_upper_bounds (const ColumnVector ub);

  friend ostream& operator << (ostream& os, const Bounds& b);

protected:

  ColumnVector lb;
  ColumnVector ub;

  int nb;

private:

  void error (const char *msg);

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
