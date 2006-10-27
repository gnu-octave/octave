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

#if !defined (octave_QRP_h)
#define octave_QRP_h 1

#include <iostream>

#include "dbleQR.h"

class
OCTAVE_API
QRP : public QR
{
public:

  QRP (void) : QR (), p () { }

  QRP (const Matrix&, QR::type = QR::std);

  QRP (const QRP& a) : QR (a), p (a.p) { }

  QRP& operator = (const QRP& a)
    {
      if (this != &a)
	{
	  QR::operator = (a);
	  p = a.p;
	}

      return *this;
    }

  ~QRP (void) { }

  void init (const Matrix&, QR::type = QR::std);

  Matrix P (void) const { return p; }

  friend std::ostream&  operator << (std::ostream&, const QRP&);

protected:

  Matrix p;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
