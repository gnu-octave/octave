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

#if !defined (octave_base_lu_h)
#define octave_base_lu_h 1

#include "MArray.h"

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
class
base_lu
{
public:

  base_lu (void) { }

  base_lu (const base_lu& a) : a_fact (a.a_fact), ipvt (a.ipvt) { }

  base_lu& operator = (const base_lu& a)
    {
      if (this != &a)
	{
	  a_fact = a.a_fact;
	  ipvt = a.ipvt;
	}
      return *this;
    }

  ~base_lu (void) { }

  lu_type L (void) const;

  lu_type U (void) const;

  p_type P (void) const;

protected:

  lu_type a_fact;
  MArray<int> ipvt;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
