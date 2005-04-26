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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_NLFunc_h)
#define octave_NLFunc_h 1

class ColumnVector;
class Matrix;

class
NLFunc
{
public:

  typedef ColumnVector (*nonlinear_fcn) (const ColumnVector&);
  typedef Matrix (*jacobian_fcn) (const ColumnVector&);

  NLFunc (void)
    : fun (0), jac (0) { }

  NLFunc (const nonlinear_fcn f)
    : fun (f), jac (0) { }

  NLFunc (const nonlinear_fcn f, const jacobian_fcn j)
    : fun (f), jac (j) { }

  NLFunc (const NLFunc& a)
    : fun (a.fun), jac (a.jac) { }

  NLFunc& operator = (const NLFunc& a)
    {
      if (this != &a)
	{
	  fun = a.fun;
	  jac = a.jac;
	}
      return *this;
    }

  ~NLFunc (void) { }

  nonlinear_fcn function (void) const { return fun; }

  NLFunc& set_function (const nonlinear_fcn f)
    {
      fun = f;
      return *this;
    }

  jacobian_fcn jacobian_function (void) const { return jac; }

  NLFunc& set_jacobian_function (const jacobian_fcn j)
    {
      jac = j;
      return *this;
    }

protected:

  nonlinear_fcn fun;
  jacobian_fcn jac;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
