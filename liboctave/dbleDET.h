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

#if !defined (octave_DET_h)
#define octave_DET_h 1

#include <iostream>

class
DET
{
friend class Matrix;
friend class SparseMatrix;

public:

  DET (void) { }

  DET (const DET& a)
    {
      det[0] = a.det[0];
      det[1] = a.det[1];
    }

  DET& operator = (const DET& a)
    {
      if (this != &a)
	{
	  det[0] = a.det[0];
	  det[1] = a.det[1];
	}
      return *this;
    }

  int value_will_overflow (void) const;
  int value_will_underflow (void) const;

  double coefficient (void) const;

  int exponent (void) const;

  double value (void) const;

  friend std::ostream&  operator << (std::ostream& os, const DET& a);

private:

  DET (const double *d)
    {
      det[0] = d[0];
      det[1] = d[1];
    }

  double det [2];
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
