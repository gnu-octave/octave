/*

Copyright (C) 2002 John W. Eaton

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

// Instantiate Maps of octave_value_lists.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Map.h"
#include "Map.cc"

#include "oct-obj.h"

template class Map<octave_value_list>;
template class CHNode<octave_value_list>;
template class CHMap<octave_value_list>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
