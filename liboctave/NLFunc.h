// NLFunc.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_NLFunc_h)
#define octave_NLFunc_h 1

class ColumnVector;
class Matrix;

extern "C++" {

#ifndef Vector
#define Vector ColumnVector
#endif

typedef Vector (*nonlinear_fcn) (const Vector&);
typedef Matrix (*jacobian_fcn) (const Vector&);

class NLFunc
{
public:

  NLFunc (void);
  NLFunc (const nonlinear_fcn);
  NLFunc (const nonlinear_fcn, const jacobian_fcn);

  NLFunc (const NLFunc& a);

  NLFunc& operator = (const NLFunc& a);

  nonlinear_fcn function (void) const;

  NLFunc& set_function (const nonlinear_fcn f);

  jacobian_fcn jacobian_function (void) const;

  NLFunc& set_jacobian_function (const jacobian_fcn j);

protected:

  nonlinear_fcn fun;
  jacobian_fcn jac;
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
