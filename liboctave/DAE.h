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

#if !defined (octave_DAE_h)
#define octave_DAE_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "DAEFunc.h"
#include "base-de.h"

class
DAE : public base_diff_eqn, public DAEFunc
{
public:

  DAE (void)
    : base_diff_eqn (), DAEFunc (), xdot () { }

  DAE (const ColumnVector& x, double t, DAEFunc& f)
    : base_diff_eqn (x, t), DAEFunc (f), xdot (x.capacity (), 0.0) { }

  DAE (const ColumnVector& x, const ColumnVector& xxdot,
       double t, DAEFunc& f);

  DAE (const DAE& a)
    : base_diff_eqn (a), DAEFunc (a), xdot (a.xdot) { }

  DAE& operator = (const DAE& a)
    {
      if (this != &a)
	{
	  base_diff_eqn::operator = (a);
	  DAEFunc::operator = (a);

	  xdot = a.xdot;
	}
      return *this;
    }

  ~DAE (void) { }

  ColumnVector state_derivative (void) { return xdot; }

  void initialize (const ColumnVector& x, double t);

  void initialize (const ColumnVector& x, const ColumnVector& xxdot,
		   double t);

protected:

  ColumnVector xdot;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
