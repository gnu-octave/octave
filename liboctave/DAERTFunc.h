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

#if !defined (octave_DAERTFunc_h)
#define octave_DAERTFunc_h 1

#include "dMatrix.h"

class
DAERTFunc : DAEFunc
{
public:

  typedef ColumnVector (*DAERTConstrFunc) (const ColumnVector& x, double t);

  DAERTFunc (void)
    : DAEFunc (), constr (0) { }

  DAERTFunc (DAERHSFunc f)
    : DAEFunc (f), constr (0) { }

  DAERTFunc (DAERHSFunc f, DAEJacFunc j)
    : DAEFunc (f, j), constr (0) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf)
    : DAEFunc (f), constr (cf) { }

  DAERTFunc (DAERHSFunc f, DAERTConstrFunc cf, DAEJacFunc j)
    : DAEFunc (f, j), constr (cf) { }

  DAERTFunc (const DAERTFunc& a)
    : DAEFunc (a), constr (a.constr) { }

  DAERTFunc& operator = (const DAERTFunc& a)
    {
      if (this != &a)
	{
	  DAEFunc::operator = (a);
	  constr = a.constr;
	}
      return *this;
    }

  ~DAERTFunc (void) { }

  DAERTConstrFunc constraint_function (void) const { return constr; }

  DAERTFunc& set_constraint_function (DAERTConstrFunc cf)
    {
      constr = cf;
      return *this;
    }

protected:

  DAERTConstrFunc constr;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
