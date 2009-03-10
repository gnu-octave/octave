/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_LinConst_h)
#define octave_LinConst_h 1

#include <iosfwd>

class ColumnVector;

#include <cfloat>

#include "dMatrix.h"
#include "Bounds.h"

class
LinConst : public Bounds
{
public:

  LinConst (void)
    : Bounds (), A () { }

  LinConst (const ColumnVector& l, const Matrix& amat, const ColumnVector& u)
    : Bounds (l, u), A (amat)
      {
	if (Bounds::size () != amat.rows ())
	  error ("nonconformant constraint matrix and bounds vectors");
      }

  LinConst (const LinConst& a)
    : Bounds (a.lb, a.ub), A (a.A) { }

  LinConst& operator = (const LinConst& a)
    {
      if (this != &a)
	{
	  Bounds::operator = (a);

	  A  = a.A;
	}
      return *this;
    }

  ~LinConst (void) { }

  Matrix constraint_matrix (void) const { return A; }

  LinConst& set_constraint_matrix (const Matrix& amat)
    {
      if (lb.capacity () != amat.rows ())
	error ("inconsistent size for new linear constraint matrix");

      A = amat;

      return *this;
    }

  friend std::ostream& operator << (std::ostream& os, const LinConst& b);

protected:

  Matrix A;

private:

  void error (const char *msg);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
