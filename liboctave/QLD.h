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

#if !defined (octave_QLD_h)
#define octave_QLD_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class Matrix;
class ColumnVector;

#include "QP.h"

class
QLD : public QP
{
public:

  QLD (void)
    : QP (), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H)
    : QP (x, H), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c)
    : QP (x, H, c), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const Bounds& b)
    : QP (x, H, b), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const LinConst& lc)
    : QP (x, H, lc), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const Bounds& b)
    : QP (x, H, c, b), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const LinConst& lc)
    : QP (x, H, c, lc), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const Bounds& b,
       const LinConst& lc)
    : QP (x, H, b, lc), iprint (0) { }

  QLD (const ColumnVector& x, const Matrix& H, const ColumnVector& c,
       const Bounds& b, const LinConst& lc)
    : QP (x, H, c, b, lc), iprint (0) { }

  QLD (const QLD& a)
    : QP (a.x, a.H, a.c, a.bnds, a.lc), iprint (0) { }

  QLD& operator = (const QLD& a)
    {
      if (this != &a)
	{
	  QP::operator = (a);

	  iprint = a.iprint;
	}
      return *this;
    }

  ~QLD (void) { }

  ColumnVector minimize (double& objf, int& inform);

private:

  int iprint;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
