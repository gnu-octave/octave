// ODEFunc.h                                             -*- C++ -*-
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

#if !defined (_ODEFunc_h)
#define _ODEFunc_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <iostream.h>
#include "Matrix.h"

#ifndef Vector
#define Vector ColumnVector
#endif

#ifndef _ODEFunc_typedefs
#define _ODEFunc_typedefs 1

typedef Vector (*ODERHSFunc) (const Vector&, double);
typedef Matrix (*ODEJacFunc) (const Vector&, double);

#endif

class ODEFunc
{
public:

  ODEFunc (void);
  ODEFunc (ODERHSFunc f);
  ODEFunc (ODERHSFunc f, ODEJacFunc j);

  ODEFunc (const ODEFunc& a);

  ODEFunc& operator = (const ODEFunc& a);

  ODERHSFunc function (void) const;

  ODEFunc& set_function (ODERHSFunc f);

  ODEJacFunc jacobian_function (void) const;

  ODEFunc& set_jacobian_function (ODEJacFunc j);

protected:

  ODERHSFunc fun;

  ODEJacFunc jac;

private:
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
