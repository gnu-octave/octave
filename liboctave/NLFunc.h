// NLFunc.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_NLFunc_h)
#define octave_NLFunc_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ColumnVector;
class Matrix;

typedef ColumnVector (*nonlinear_fcn) (const ColumnVector&);
typedef Matrix (*jacobian_fcn) (const ColumnVector&);

class NLFunc
{
public:

  NLFunc (void)
    {
      fun = 0;
      jac = 0;
    }

  NLFunc (const nonlinear_fcn f)
    {
      fun = f;
      jac = 0;
    }

  NLFunc (const nonlinear_fcn f, const jacobian_fcn j)
    {
      fun = f;
      jac = j;
    }

  NLFunc (const NLFunc& a)
    {
      fun = a.function ();
      jac = a.jacobian_function ();
    }

  NLFunc& operator = (const NLFunc& a)
    {
      fun = a.function ();
      jac = a.jacobian_function ();

      return *this;
    }

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
