/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_LP_h)
#define octave_LP_h 1

#include "dColVector.h"
#include "Bounds.h"
#include "LinConst.h"
#include "base-min.h"

class LP : public base_minimizer
{
public:

  LP (void)
    : base_minimizer (), c (), bnds (), lc () { }

  LP (const ColumnVector& c_arg)
    : base_minimizer (), c (c_arg), bnds (), lc () { }

  LP (const ColumnVector& c_arg, const Bounds& b)
    : base_minimizer (), c (c_arg), bnds (b), lc () { }

  LP (const ColumnVector& c_arg, const Bounds& b, const LinConst& l)
    : base_minimizer (), c (c_arg), bnds (b), lc (l) { }

  LP (const ColumnVector& c_arg, const LinConst& l)
    : base_minimizer (), c (c_arg), bnds (), lc (l) { }

  LP (const LP& a)
    : base_minimizer (a), c (a.c), bnds (a.bnds), lc (a.lc) { }

  LP& operator = (const LP& a)
    {
      if (this != &a)
	{
	  base_minimizer::operator = (a);

	  c = a.c;
	  bnds = a.bnds;
	  lc = a.lc;
	}
      return *this;
    }

  ~LP (void) { }

  ColumnVector linear_obj_coeff (void) const { return c; }

  Bounds bounds (void) const { return bnds; }

  LinConst linear_constraints (void) const { return lc; }

protected:

  ColumnVector c;
  Bounds bnds;
  LinConst lc;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
