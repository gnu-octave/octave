// LP.cc                                                -*- C++ -*-
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

#include "LP.h"

LP::LP (void) {}

LP::LP (const Vector& c_arg) : c (c_arg) { }

LP::LP (const Vector& c_arg, const Bounds& b) : c (c_arg), bnds (b) { }

LP::LP (const Vector& c_arg, const LinConst& l) : c (c_arg), lc (l) { }

LP::LP (const Vector& c_arg, const Bounds& b, const LinConst& l)
  : c (c_arg), bnds (b), lc (l) { }

Vector
LP::minimize (void)
{
  double objf;
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
LP::minimize (double& objf)
{
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
LP::minimize (double& objf, int& inform)
{
  Vector lambda;
  return minimize (objf, inform, lambda);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
