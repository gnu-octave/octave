// NLEqn.h                                                -*- C++ -*-
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

#if !defined (_NLEqn_h)
#define _NLEqn_h 1

#include "Matrix.h"
#include "NLFunc.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NLEqn : public NLFunc
{
 public:

  NLEqn (void);
  NLEqn (const Vector&, const NLFunc);

  NLEqn (const NLEqn &);

  NLEqn& operator = (const NLEqn& a);

  void resize (int);

  void set_states (const Vector&);

  Vector states (void) const;

  int size (void) const;

  Vector solve (void);
  Vector solve (const Vector&);

  Vector solve (int& info);
  Vector solve (const Vector&, int& info);

 private:

  int n;
  Vector x;

  void error (const char* msg);

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
