// LP.h                                                -*- C++ -*-
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

#if !defined (_LP_h)
#define _LP_h 1

#include "Matrix.h"
#include "Bounds.h"
#include "LinConst.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class LP
{
 public:

  LP (void);
  LP (const Vector& c);
  LP (const Vector& c, const Bounds& b);
  LP (const Vector& c, const Bounds& b, const LinConst& lc);
  LP (const Vector& c,                  const LinConst& lc);

  virtual Vector minimize (void);
  virtual Vector minimize (double& objf);
  virtual Vector minimize (double& objf, int& inform);
  virtual Vector minimize (double& objf, int& inform, Vector& lambda) = 0;

 protected:

  Vector c;
  Bounds bnds;
  LinConst lc;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
