/*

Copyright (C) 2002 John W. Eaton

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

#if !defined (octave_ODESFunc_h)
#define octave_ODESFunc_h 1

#include "dMatrix.h"

class
ODESFunc
{
public:

  struct DAEJac
    {
      Matrix *dfdxdot;
      Matrix *dfdx;
    };

  typedef ColumnVector (*ODES_fsub) (double, const ColumnVector& x,
				     const ColumnVector& theta); 

  typedef ColumnVector (*ODES_bsub) (double, const ColumnVector& x,
				     const ColumnVector& theta, int column);

  typedef Matrix (*ODES_jsub) (double, const ColumnVector& x,
			       const ColumnVector& theta);

  ODESFunc (void)
    : fsub (0), bsub (0), jsub (0) { }

  ODESFunc (ODES_fsub f)
    : fsub (f), bsub (0), jsub (0) { }

  ODESFunc (ODES_fsub f, ODES_bsub b)
    : fsub (f), bsub (b), jsub (0) { }

  ODESFunc (ODES_fsub f, ODES_bsub b, ODES_jsub j)
    : fsub (f), bsub (b), jsub (j) { }

  ODESFunc (const ODESFunc& a)
    : fsub (a.fsub), bsub (a.bsub), jsub (a.jsub) { }

  ODESFunc& operator = (const ODESFunc& a)
    {
      if (this != &a)
	{
	  fsub = a.fsub;
	  bsub = a.bsub;
	  jsub = a.jsub;
	}
      return *this;
    }

  ~ODESFunc (void) { }

  ODES_fsub fsub_function (void) const { return fsub; }

  ODESFunc& set_fsub_function (ODES_fsub f)
    {
      fsub = f;
      return *this;
    }

  ODES_bsub bsub_function (void) const { return bsub; }

  ODESFunc& set_bsub_function (ODES_bsub b)
    {
      bsub = b;
      return *this;
    }

  ODES_jsub jsub_function (void) const { return jsub; }

  ODESFunc& set_jsub_function (ODES_jsub j)
    {
      jsub = j;
      return *this;
    }

protected:

  ODES_fsub fsub;
  ODES_bsub bsub;
  ODES_jsub jsub;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/




