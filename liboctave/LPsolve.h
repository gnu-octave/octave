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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_LPsolve_h)
#define octave_LPsolve_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ColumnVector;

#include "LP.h"

class
LPsolve : public LP
{
public:

  LPsolve (void)
    : LP () { }

  LPsolve (const ColumnVector& c)
    : LP (c) { }

  LPsolve (const ColumnVector& c, const Bounds& b)
    : LP (c, b) { }

  LPsolve (const ColumnVector& c, const Bounds& b, const LinConst& lc)
    : LP (c, b, lc) { }

  LPsolve (const ColumnVector& c, const LinConst& lc)
    : LP (c, lc) { }

  LPsolve (const LPsolve& a)
    : LP (a) { }

  LPsolve& operator = (const LPsolve& a)
    {
      if (this != &a)
	LP::operator = (a);

      return *this;
    }

  ~LPsolve (void) { }

  ColumnVector do_minimize (double& objf, int& inform, ColumnVector& lambda);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
