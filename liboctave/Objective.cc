// Objective.cc                                             -*- C++ -*-
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
#include <config.h>
#endif

#include "Objective.h"

Objective::Objective (void)
{
  phi = 0;
  grad = 0;
}

Objective::Objective (const objective_fcn obj)
{
  phi = obj;
  grad = 0;
}

Objective::Objective (const objective_fcn obj, const gradient_fcn g)
{
  phi = obj;
  grad = g;
}

Objective::Objective (const Objective& a)
{
  phi = a.phi;
  grad = a.grad;
}

Objective&
Objective::operator = (const Objective& a)
{
  phi = a.phi;
  grad = a.grad;
  return *this;
}

objective_fcn
Objective::objective_function (void) const
{
  return phi;
}

Objective&
Objective::set_objective_function (const objective_fcn obj)
{
  phi = obj;
  return *this;
}

gradient_fcn
Objective::gradient_function (void) const
{
  return grad;
}

Objective&
Objective::set_gradient_function (const gradient_fcn g)
{
  grad = g;
  return *this;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
