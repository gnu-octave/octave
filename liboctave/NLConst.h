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

  NLConst (int n)
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
