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

#if !defined (octave_base_de_h)
#define octave_base_de_h 1

#include "dColVector.h"
#include "dMatrix.h"

class
base_diff_eqn
{
public:

  base_diff_eqn (void) : x (), t (0.0) { }

  base_diff_eqn (const ColumnVector& xx, double tt) : x (xx), t (tt) { }

  base_diff_eqn (const base_diff_eqn& a) : x (a.x), t (a.t) { }

  virtual ~base_diff_eqn (void) { }

  base_diff_eqn& operator = (const base_diff_eqn& a)
    {
      if (this != &a)
	{
	  x = a.x;
	  t = a.t;
	}
      return *this;
    }

  // Derived classes must provide functions to actually do the
  // integration.

  // Return the vector of states at output time t.
  virtual ColumnVector do_integrate (double tt) = 0;

  // Return a matrix of states at each output time specified by t.
  // The rows of the result matrix should each correspond to a new
  // output time.
  virtual Matrix do_integrate (const ColumnVector& tt) = 0;

  // There must also be a way for us to force the integration to
  // restart.
  virtual void force_restart (void) = 0;

  // Lots of ways to call the single function and optionally set and
  // get additional information.

  // Integrate to t from current point.
  virtual ColumnVector integrate (double tt)
    { return do_integrate (tt); }

  // Set new x0, t0 and integrate to t.
  virtual ColumnVector integrate (const ColumnVector& x0, double t0, double tt)
    {
      initialize (x0, t0);
      return do_integrate (tt);
    }

  // Integrate from current point and return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& tt)
    { return do_integrate (tt); }

  // Set new x0, t0 and integrate to return output at all points
  // specified by t.
  virtual Matrix integrate (const ColumnVector& x0, double t0,
			    const ColumnVector tt)
    {
      initialize (x0, t0);
      return do_integrate (tt);
    }

  virtual void initialize (const ColumnVector& x0, double t0)
    {
      x = x0;
      t = t0;
      force_restart ();
    }

  int size (void) const { return x.capacity (); }

  ColumnVector state (void) const { return x; }

  double time (void) const { return t; }

protected:

  ColumnVector x;
  double t;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
