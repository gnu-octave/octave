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

#include "base-lu.h"
#include "dMatrix.h"
#include "CMatrix.h"

class
ComplexLU : public base_lu <ComplexMatrix, Matrix>
{
public:

  ComplexLU (void) : base_lu <ComplexMatrix, Matrix> () { }

  ComplexLU (const ComplexMatrix& a);

  ComplexLU (const ComplexLU& a) : base_lu <ComplexMatrix, Matrix> (a) { }

  ComplexLU& operator = (const ComplexLU& a)
    {
      if (this != &a)
	base_lu <ComplexMatrix, Matrix> :: operator = (a);

      return *this;
    }

  ~ComplexLU (void) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
