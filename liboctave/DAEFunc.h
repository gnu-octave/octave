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

#if !defined (octave_DAEFunc_h)
#define octave_DAEFunc_h 1

class Matrix;
class ColumnVector;

class
DAEFunc
{
public:

  struct DAEJac
    {
      Matrix *dfdxdot;
      Matrix *dfdx;
    };

  typedef ColumnVector (*DAERHSFunc) (const ColumnVector& x,
				      const ColumnVector& xdot,
				      double, int&); 

  typedef DAEJac (*DAEJacFunc) (const ColumnVector& x,
				const ColumnVector& xdot, double);

  DAEFunc (void)
    : fun (0), jac (0) { }

  DAEFunc (DAERHSFunc f)
    : fun (f), jac (0) { }

  DAEFunc (DAERHSFunc f, DAEJacFunc j)
    : fun (f), jac (j) { }

  DAEFunc (const DAEFunc& a)
    : fun (a.fun), jac (a.jac) { }

  DAEFunc& operator = (const DAEFunc& a)
    {
      if (this != &a)
	{
	  fun = a.fun;
	  jac = a.jac;
	}
      return *this;
    }

  ~DAEFunc (void) { }

  DAERHSFunc function (void) const { return fun; }

  DAEFunc& set_function (DAERHSFunc f)
    {
      fun = f;
      return *this;
    }

  DAEJacFunc jacobian_function (void) const { return jac; }

  DAEFunc& set_jacobian_function (DAEJacFunc j)
    {
      jac = j;
      return *this;
    }

protected:

  DAERHSFunc fun;
  DAEJacFunc jac;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
