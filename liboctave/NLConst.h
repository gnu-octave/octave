// NLConst.h                                              -*- C++ -*-
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

#if !defined (_NLConst_h)
#define _NLConst_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <Bounds.h>
#include "Matrix.h"
#include "NLFunc.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NLConst : public Bounds, public NLFunc
{
public:

  NLConst (void);
  NLConst (int n);
  NLConst (const Vector lb, const NLFunc f, const ColumnVector ub);
  NLConst (const NLConst& a);

  NLConst& operator = (const NLConst& a);

private:

  void error (const char *msg);

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
