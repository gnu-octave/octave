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

#if defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "base-lu.h"

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
lu_type
base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: L (void) const
{
  int n = ipvt.length ();

  lu_type l (n, n, lu_elt_type (0.0));

  for (int i = 0; i < n; i++)
    {
      l.xelem (i, i) = 1.0;
      for (int j = 0; j < i; j++)
	l.xelem (i, j) = a_fact.xelem (i, j);
    }

  return l;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
lu_type
base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: U (void) const
{
  int n = ipvt.length ();

  lu_type u (n, n, lu_elt_type (0.0));

  for (int i = 0; i < n; i++)
    {
      for (int j = i; j < n; j++)
	u.xelem (i, j) = a_fact.xelem (i, j);
    }

  return u;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
p_type
base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: P (void) const
{
  int n = ipvt.length ();

  Array<int> pvt (n);

  for (int i = 0; i < n; i++)
    pvt.xelem (i) = i;

  for (int i = 0; i < n - 1; i++)
    {
      int k = ipvt.xelem (i);

      if (k != i)
	{
	  int tmp = pvt.xelem (k);
	  pvt.xelem (k) = pvt.xelem (i);
	  pvt.xelem (i) = tmp;
	}
    }

  p_type p (n, n, p_elt_type (0.0));

  for (int i = 0; i < n; i++)
    p.xelem (i, pvt.xelem (i)) = 1.0;

  return p;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
