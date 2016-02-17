/*

Copyright (C) 1994-2015 John W. Eaton

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

#if ! defined (octave_floatQRP_h)
#define octave_floatQRP_h 1

#include "octave-config.h"

#include <iosfwd>

#include "PermMatrix.h"
#include "fColVector.h"
#include "fMatrix.h"
#include "fRowVector.h"
#include "qr.h"

class
OCTAVE_API
FloatQRP : public qr<FloatMatrix>
{
public:

  typedef qr<FloatMatrix>::type type;

  FloatQRP (void) : qr<FloatMatrix> (), p () { }

  FloatQRP (const FloatMatrix&, type = qr<FloatMatrix>::std);

  FloatQRP (const FloatQRP& a) : qr<FloatMatrix> (a), p (a.p) { }

  FloatQRP& operator = (const FloatQRP& a)
  {
    if (this != &a)
      {
        qr<FloatMatrix>::operator = (a);
        p = a.p;
      }

    return *this;
  }

  ~FloatQRP (void) { }

  void init (const FloatMatrix&, type = qr<FloatMatrix>::std);

  PermMatrix P (void) const { return p; }

  FloatRowVector Pvec (void) const;

  friend std::ostream&  operator << (std::ostream&, const FloatQRP&);

protected:

  PermMatrix p;
};

#endif
