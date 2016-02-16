/*

Copyright (C) 1996-2015 John W. Eaton
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

#if ! defined (octave_lu_h)
#define octave_lu_h 1

#include "octave-config.h"

#include "PermMatrix.h"

template <typename T>
class
lu
{
public:

  typedef typename T::column_vector_type VT;
  typedef typename T::element_type ELT_T;

  lu (void)
    : a_fact (), l_fact (), ipvt () { }

  lu (const T& a);

  lu (const lu& a)
    : a_fact (a.a_fact), l_fact (a.l_fact), ipvt (a.ipvt) { }

  lu (const T& l, const T& u, const PermMatrix& p);

  lu& operator = (const lu& a)
  {
    if (this != &a)
      {
        a_fact = a.a_fact;
        l_fact = a.l_fact;
        ipvt = a.ipvt;
      }

    return *this;
  }

  virtual ~lu (void) { }

  bool packed (void) const;

  void unpack (void);

  T L (void) const;

  T U (void) const;

  T Y (void) const;

  PermMatrix P (void) const;

  ColumnVector P_vec (void) const;

  bool regular (void) const;

  void update (const VT& u, const VT& v);

  void update (const T& u, const T& v);

  void update_piv (const VT& u, const VT& v);

  void update_piv (const T& u, const T& v);

protected:

  Array<octave_idx_type> getp (void) const;

  T a_fact;
  T l_fact;

  Array<octave_idx_type> ipvt;
};

#endif
