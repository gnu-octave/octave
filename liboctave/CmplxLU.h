//                                  -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_ComplexLU_h)
#define octave_Complex_LU_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include "dMatrix.h"
#include "CMatrix.h"

class
ComplexLU
{
public:

  ComplexLU (void) : l (), u (), p (), ipvt (0), pvt (0) { }

  ComplexLU (const ComplexMatrix& a);

  ComplexLU (const ComplexLU& a) : l (a.l), u (a.u), p (a.p) { }

  ComplexLU& operator = (const ComplexLU& a)
    {
      if (this != &a)
	{
	  l = a.l;
	  u = a.u;
	  p = a.p;
	}
      return *this;
    }

  ~ComplexLU (void)
    {
      delete [] ipvt;
      delete [] pvt;
    }

  ComplexMatrix L (void) const { return l; }
  ComplexMatrix U (void) const { return u; }

  Matrix P (void) const { return p; }

  friend ostream&  operator << (ostream& os, const ComplexLU& a);

private:

  ComplexMatrix l;
  ComplexMatrix u;
  Matrix p;

  int *ipvt;
  int *pvt;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
