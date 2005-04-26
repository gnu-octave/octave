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

#if !defined (octave_Objective_h)
#define octave_Objective_h 1

#include "dColVector.h"

class
Objective
{
public:

  typedef double (*objective_fcn) (const ColumnVector&);
  typedef ColumnVector (*gradient_fcn) (const ColumnVector&);

  Objective (void)
    : phi (0), grad (0) { }

  Objective (const objective_fcn obj)
    : phi (obj), grad (0) { }

  Objective (const objective_fcn obj, const gradient_fcn g)
    : phi (obj), grad (g) { }

  Objective (const Objective& a)
    : phi (a.phi), grad (a.grad) { }

  Objective& operator = (const Objective& a)
    {
      if (this != &a)
	{
	  phi = a.phi;
	  grad = a.grad;
	}
      return *this;
    }

  ~Objective (void) { }

  objective_fcn objective_function (void) const { return phi; }

  Objective& set_objective_function (const objective_fcn obj)
    {
      phi = obj;
      return *this;
    }

  gradient_fcn gradient_function (void) const { return grad; }

  Objective& set_gradient_function (const gradient_fcn g)
    {
      grad = g;
      return *this;
    }

private:

  objective_fcn phi;
  gradient_fcn grad;

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
