// ODEFunc.h                                             -*- C++ -*-
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

#if !defined (octave_ODEFunc_h)
#define octave_ODEFunc_h 1

class Matrix;
class ColumnVector;

class
ODEFunc
{
public:

  typedef ColumnVector (*ODERHSFunc) (const ColumnVector&, double);
  typedef Matrix (*ODEJacFunc) (const ColumnVector&, double);

  ODEFunc (void)
    : fun (0), jac (0) { }

  ODEFunc (ODERHSFunc f)
    : fun (f), jac (0) { }

  ODEFunc (ODERHSFunc f, ODEJacFunc j)
    : fun (f), jac (j) { }

  ODEFunc (const ODEFunc& a)
    : fun (a.fun), jac (a.jac) { }

  ODEFunc& operator = (const ODEFunc& a)
    {
      if (this != &a)
	{
	  fun = a.fun;
	  jac = a.jac;
	}
      return *this;
    }

  ~ODEFunc (void) { }

  ODERHSFunc function (void) const { return fun; }

  ODEFunc& set_function (ODERHSFunc f)
    {
      fun = f;
      return *this;
    }

  ODEJacFunc jacobian_function (void) const { return jac; }

  ODEFunc& set_jacobian_function (ODEJacFunc j)
    {
      jac = j;
      return *this;
    }

protected:

  ODERHSFunc fun;
  ODEJacFunc jac;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
