// NLFunc.cc                                             -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "NLFunc.h"

NLFunc::NLFunc (void)
{
  fun = 0;
  jac = 0;
}

NLFunc::NLFunc (const nonlinear_fcn f)
{
  fun = f;
  jac = 0;
}

NLFunc::NLFunc (const nonlinear_fcn f, const jacobian_fcn j)
{
  fun = f;
  jac = j;
}

NLFunc::NLFunc (const NLFunc& a)
{
  fun = a.function ();
  jac = a.jacobian_function ();
}

NLFunc&
NLFunc::operator = (const NLFunc& a)
{
  fun = a.function ();
  jac = a.jacobian_function ();

  return *this;
}

nonlinear_fcn
NLFunc::function (void) const
{
  return fun;
}

NLFunc&
NLFunc::set_function (const nonlinear_fcn f)
{
  fun = f;

  return *this;
}

jacobian_fcn
NLFunc::jacobian_function (void) const
{
  return jac;
}

NLFunc&
NLFunc::set_jacobian_function (const jacobian_fcn j)
{
  jac = j;

  return *this;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
