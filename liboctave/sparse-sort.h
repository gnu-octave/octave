/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_sparse_sort_h)
#define octave_sparse_sort_h

#include "oct-sort.h"

class
octave_sparse_sort_idxl
{
 public:
  unsigned int r;
  unsigned int c;
  unsigned int idx; 
};

bool octave_sparse_sidxl_comp (octave_sparse_sort_idxl* i,
			       octave_sparse_sort_idxl* j);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
