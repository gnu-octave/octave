// DAEFunc.h                                             -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_DAEFunc_h)
#define octave_DAEFunc_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class Matrix;
class ColumnVector;

#ifndef Vector
#define Vector ColumnVector
#endif

#if !defined (octave_DAEFunc_typedefs)
#define octave_DAEFunc_typedefs 1

#endif

class DAEFunc
{
public:

  struct DAEJac
    {
      Matrix *dfdxdot;
      Matrix *dfdx;
    };

  typedef Vector (*DAERHSFunc) (const Vector& x,
				const Vector& xdot, double); 

  typedef DAEJac (*DAEJacFunc) (const Vector& x,
				const Vector& xdot, double);

  DAEFunc (void);
  DAEFunc (DAERHSFunc f);
  DAEFunc (DAERHSFunc f, DAEJacFunc j);

  DAEFunc (const DAEFunc& a);

  DAEFunc& operator = (const DAEFunc& a);

  DAERHSFunc function (void) const;

  DAEFunc& set_function (DAERHSFunc f);

  DAEJacFunc jacobian_function (void) const;

  DAEFunc& set_jacobian_function (DAEJacFunc f);

protected:

  DAERHSFunc fun;

  DAEJacFunc jac;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
