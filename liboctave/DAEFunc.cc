// DAEFunc.cc                                             -*- C++ -*-
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "DAEFunc.h"

DAEFunc::DAEFunc (void)
{
  fun = 0;
  jac = 0;
}

DAEFunc::DAEFunc (DAERHSFunc f)
{
  fun = f;
  jac = 0;
}

DAEFunc::DAEFunc (DAERHSFunc f, DAEJacFunc j)
{
  fun = f;
  jac = j;
}

DAEFunc::DAEFunc (const DAEFunc& a)
{
  fun = a.fun;
  jac = a.jac;
}

DAEFunc&
DAEFunc::operator = (const DAEFunc& a)
{
  fun = a.fun;
  jac = a.jac;

  return *this;
}

DAEFunc::DAERHSFunc
DAEFunc::function (void) const
{
  return fun;
}

DAEFunc&
DAEFunc::set_function (DAERHSFunc f)
{
  fun = f;
  return *this;
}

DAEFunc::DAEJacFunc
DAEFunc::jacobian_function (void) const
{
  return jac;
}

DAEFunc&
DAEFunc::set_jacobian_function (DAEJacFunc j)
{
  jac = j;
  return *this;
}

