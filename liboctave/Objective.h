// Objective.h                                          -*- C++ -*-
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

#if !defined (octave_Objective_h)
#define octave_Objective_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "dColVector.h"

extern "C++" {

#ifndef Vector
#define Vector ColumnVector
#endif

typedef double (*objective_fcn) (Vector&);
typedef Vector (*gradient_fcn) (Vector&);

class Objective
{
 public:

  Objective (void);
  Objective (const objective_fcn);
  Objective (const objective_fcn, const gradient_fcn);

  Objective (const Objective& a);

  Objective& operator = (const Objective& a);

  objective_fcn objective_function (void) const;

  Objective& set_objective_function (const objective_fcn);

  gradient_fcn gradient_function (void) const;

  Objective& set_gradient_function (const gradient_fcn);

 private:

  objective_fcn phi;
  gradient_fcn grad;

};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
