/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_CollocWt_h)
#define octave_CollocWt_h 1

#include <iostream>

#include "dMatrix.h"
#include "dColVector.h"

class
CollocWt
{
public:

  CollocWt (void)
    : n (0), inc_left (0), inc_right (0), lb (0.0), rb (1.0),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (0) { }

  CollocWt (int nc, int il, int ir)
    : n (nc), inc_left (il), inc_right (ir), lb (0.0), rb (1.0),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (0) { }

  CollocWt (int nc, int il, int ir, double l, double rr)
    : n (nc), inc_left (il), inc_right (ir), lb (l), rb (rr),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (0) { }

  CollocWt (int nc, double a, double b, int il, int ir)
    : n (nc), inc_left (il), inc_right (ir), lb (0.0), rb (1.0),
      Alpha (a), Beta (b), initialized (0) { }

  CollocWt (int nc, double a, double b, int il, int ir,
		      double ll, double rr)  
    : n (nc), inc_left (il), inc_right (ir), lb (ll), rb (rr),
      Alpha (a), Beta (b), r (), q (), A (), B (), initialized (0) { }

  CollocWt (const CollocWt& a)
    : n (a.n), inc_left (a.inc_left), inc_right (a.inc_right),
      lb (a.lb), rb (a.rb), Alpha (a.Alpha), Beta (a.Beta),
      r (a.r), q (a.q), A (a.A), B (a.B),
      initialized (a.initialized) { } 

  CollocWt& operator = (const CollocWt& a)
    {
      if (this != &a)
	{
	  n = a.n;
	  inc_left = a.inc_left;
	  inc_right = a.inc_right;
	  lb = a.lb;
	  rb = a.rb;
	  r = a.r;
	  q = a.q;
	  A = a.A;
	  B = a.B;
	  initialized = a.initialized;
	}
      return *this;
    }

  ~CollocWt (void) { }

  CollocWt& resize (int nc)
    {
      n = nc;
      initialized = 0;
      return *this;
    }

  CollocWt& add_left (void)
    {
      inc_left = 1;
      initialized = 0;
      return *this;
    }

  CollocWt& delete_left (void)
    {
      inc_left = 0;
      initialized = 0;
      return *this;
    }

  CollocWt& set_left (double val);

  CollocWt& add_right (void)
    {
      inc_right = 1;
      initialized = 0;
      return *this;
    }

  CollocWt& delete_right (void)
    {
      inc_right = 0;
      initialized = 0;
      return *this;
    }

  CollocWt& set_right (double val);

  CollocWt& set_alpha (double val)
    {
      Alpha = val;
      initialized = 0;
      return *this;
    }

  CollocWt& set_beta (double val)
    {
      Beta = val;
      initialized = 0;
      return *this;
    }

  int ncol (void) const { return n; }

  int left_included (void) const { return inc_left; }
  int right_included (void) const { return inc_right; }

  double left (void) const { return lb; }
  double right (void) const { return rb; }

  double width (void) const { return rb - lb; }

  double alpha (void) const { return Alpha; }
  double beta (void) const { return Beta; }

  ColumnVector roots (void) { if (!initialized) init (); return r; }
  ColumnVector quad (void) { if (!initialized) init (); return q; }

  ColumnVector quad_weights (void) { return quad (); }

  Matrix first (void) { if (!initialized) init (); return A; }

  Matrix second (void) { if (!initialized) init (); return B; }

  friend std::ostream& operator << (std::ostream&, const CollocWt&);

protected:

  int n;

  int inc_left;
  int inc_right;

  double lb;
  double rb;

  double Alpha;
  double Beta;

  ColumnVector r;
  ColumnVector q;

  Matrix A;
  Matrix B;

  int initialized;

  void init (void);

  void error (const char *msg);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
