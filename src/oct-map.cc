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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "str-vec.h"

#include "pt-const.h"
#include "oct-map.h"
#include "utils.h"

string_vector
Octave_map::make_name_list (void)
{
  int len = length ();

  string_vector names (len);

  int i = 0;
  for (Pix p = first (); p != 0; next (p))
    names[i++] = key (p);

  return names;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
