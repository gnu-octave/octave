/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2002, 2004, 2005, 2007
              John W. Eaton

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

#if !defined (octave_NLConst_h)
#define octave_NLConst_h 1

class ColumnVector;

#include "Bounds.h"
#include "NLFunc.h"

class
NLConst : public Bounds, public NLFunc
{
public:

  NLConst (void)
    : Bounds (), NLFunc () { }

  NLConst (octave_idx_type n)
    : Bounds (n), NLFunc () { }

  NLConst (const ColumnVector& lb, const NLFunc f, const ColumnVector& ub)
    : Bounds (lb, ub), NLFunc (f) { }

  NLConst (const NLConst& a)
    : Bounds (a.lb, a.ub), NLFunc (a.fun, a.jac) { }

  NLConst& operator = (const NLConst& a)
    {
      if (this != &a)
	{
	  Bounds::operator = (a);
	  NLFunc::operator = (a);
	}
      return *this;
    }

  ~NLConst (void) { }

private:

  void error (const char *msg);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
