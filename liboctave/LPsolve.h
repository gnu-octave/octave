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

#if !defined (octave_LPsolve_h)
#define octave_LPsolve_h 1

class ColumnVector;

#include "LP.h"

class
LPsolve : public octave_LP
{
public:

  LPsolve (void)
    : octave_LP () { }

  LPsolve (const ColumnVector& c)
    : octave_LP (c) { }

  LPsolve (const ColumnVector& c, const Bounds& b)
    : octave_LP (c, b) { }

  LPsolve (const ColumnVector& c, const Bounds& b, const LinConst& lc)
    : octave_LP (c, b, lc) { }

  LPsolve (const ColumnVector& c, const LinConst& lc)
    : octave_LP (c, lc) { }

  LPsolve (const LPsolve& a)
    : octave_LP (a) { }

  LPsolve& operator = (const LPsolve& a)
    {
      if (this != &a)
	octave_LP::operator = (a);

      return *this;
    }

  ~LPsolve (void) { }

  ColumnVector do_minimize (double& objf, octave_idx_type& inform, ColumnVector& lambda);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
