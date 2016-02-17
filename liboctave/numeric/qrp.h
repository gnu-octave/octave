/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if ! defined (octave_qrp_h)
#define octave_qrp_h 1

#include "octave-config.h"

#include "PermMatrix.h"
#include "qr.h"

template <typename T>
class
qrp : public qr<T>
{
public:

  typedef typename T::real_row_vector_type RV_T;

  typedef typename qr<T>::type type;

  qrp (void) : qr<T> (), p () { }

  qrp (const T&, type = qr<T>::std);

  qrp (const qrp& a) : qr<T> (a), p (a.p) { }

  qrp& operator = (const qrp& a)
  {
    if (this != &a)
      {
        qr<T>::operator = (a);
        p = a.p;
      }

    return *this;
  }

  ~qrp (void) { }

  void init (const T&, type = qr<T>::std);

  PermMatrix P (void) const { return p; }

  RV_T Pvec (void) const;

private:

  PermMatrix p;
};

#endif
