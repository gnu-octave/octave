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
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include "oct-sort.cc"
#include "quit.h"

#include "sparse-sort.h"

// A simple class and instantiation of the octave merge sort class
// to sort sparse data before matrix creation. This is significantly
// faster than using octave_qsort.

bool
octave_sparse_sidxl_comp (octave_sparse_sort_idxl* i, 
			  octave_sparse_sort_idxl* j)
{
  int tmp = i->c - j->c;
  if (tmp < 0)
    return true;
  else if (tmp > 0)
    return false;
  return  (i->r < j->r);
}

// Instantiate the sparse sorting class
template class octave_sort<octave_sparse_sort_idxl *>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
