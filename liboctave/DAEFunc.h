// DAEFunc.h                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_DAEFunc_h)
#define _DAEFunc_h 1

#include "Matrix.h"

#ifndef Vector
#define Vector ColumnVector
#endif

#ifndef _DAEFunc_typedefs
#define _DAEFunc_typedefs 1

typedef struct DAEJac
{
  Matrix *dfdxdot;
  Matrix *dfdx;
};

typedef Vector (*DAERHSFunc) (const Vector& x, const Vector& xdot, double);
typedef DAEJac (*DAEJacFunc) (const Vector& x, const Vector& xdot, double);

#endif

class DAEFunc
{
public:

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
