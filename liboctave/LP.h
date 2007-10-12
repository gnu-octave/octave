/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_LP_h)
#define octave_LP_h 1

#include "dColVector.h"
#include "Bounds.h"
#include "LinConst.h"
#include "base-min.h"

class
octave_LP : public base_minimizer
{
public:

  octave_LP (void)
    : base_minimizer (), cvec (), bnds (), lin_constr () { }

  octave_LP (const ColumnVector& c)
    : base_minimizer (), cvec (c), bnds (), lin_constr () { }

  octave_LP (const ColumnVector& c, const Bounds& b)
    : base_minimizer (), cvec (c), bnds (b), lin_constr () { }

  octave_LP (const ColumnVector& c, const Bounds& b, const LinConst& l)
    : base_minimizer (), cvec (c), bnds (b), lin_constr (l) { }

  octave_LP (const ColumnVector& c, const LinConst& l)
    : base_minimizer (), cvec (c), bnds (), lin_constr (l) { }

  octave_LP (const octave_LP& a)
    : base_minimizer (a), cvec (a.cvec), bnds (a.bnds), lin_constr (a.lin_constr) { }

  octave_LP& operator = (const octave_LP& a)
    {
      if (this != &a)
	{
	  base_minimizer::operator = (a);

	  cvec = a.cvec;
	  bnds = a.bnds;
	  lin_constr = a.lin_constr;
	}
      return *this;
    }

  ~octave_LP (void) { }

  ColumnVector linear_obj_coeff (void) const { return cvec; }

  Bounds bounds (void) const { return bnds; }

  LinConst linear_constraints (void) const { return lin_constr; }

protected:

  ColumnVector cvec;
  Bounds bnds;
  LinConst lin_constr;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
