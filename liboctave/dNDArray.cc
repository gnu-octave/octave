//N-D Array  manipulations.
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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "NDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"

bool
NDArray::any_element_is_negative (bool neg_zero) const
{
  int n = length (); 
  if (neg_zero)
    {
      for (int i = 0; i < n; i++)
	if (lo_ieee_signbit (Array<double>::elem (i)))
	  return true;
    }
  else
    {
      for (int i = 0; i < n; i++)
	if (Array<double>::elem (i) < 0)
	  return true;
    }
 
 return false;
}

bool
NDArray::all_integers (double& max_val, double& min_val) const
{
  int n = length ();

  if (n > 0)
    {
      max_val = Array<double>::elem (0);
      min_val = Array<double>::elem (0);
    }
  else 
    return false;

  for (int i = 0; i < n; i++)
    {
      double val = Array<double>::elem (0);
      
      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}
