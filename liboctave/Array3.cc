// Template array classes
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include <iostream.h>

#include "Array3.h"

#if defined (HEAVYWEIGHT_INDEXING)
#include "idx-vector.h"
#include "Array3-idx.h"
#endif

#include "lo-error.h"

// Three dimensional array class.

template <class T>
void
Array3<T>::resize (int r, int c, int p)
{
  if (r < 0 || c < 0 || p < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 () && p == dim3 ())
    return;

  ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_d3 = dim3 ();
  int old_len = length ();

  int ts = get_size (get_size (r, c), p);

  rep = new ArrayRep (ts);

  d1 = r;
  d2 = c;
  d3 = p;

  if (old_data && old_len > 0)
    {
      int min_r = old_d1 < r ? old_d1 : r;
      int min_c = old_d2 < c ? old_d2 : c;
      int min_p = old_d3 < p ? old_d3 : p;

      for (int k = 0; k < min_p; k++)
	for (int j = 0; j < min_c; j++)
	  for (int i = 0; i < min_r; i++)
	    xelem (i, j, k) = old_data[old_d1*(old_d2*k+j)+i];
    }

  if (--old_rep->count <= 0)
    delete old_rep;
}

template <class T>
void
Array3<T>::resize (int r, int c, int p, const T& val)
{
  if (r < 0 || c < 0 || p < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 () && p == dim3 ())
    return;

  ArrayRep *old_rep = rep;
  const T *old_data = data ();

  int old_d1 = dim1 ();
  int old_d2 = dim2 ();
  int old_d3 = dim3 ();

  int old_len = length ();

  int ts = get_size (get_size (r, c), p);

  rep = new ArrayRep (ts);

  d1 = r;
  d2 = c;
  d3 = p;

  int min_r = old_d1 < r ? old_d1 : r;
  int min_c = old_d2 < c ? old_d2 : c;
  int min_p = old_d3 < p ? old_d3 : p;

  if (old_data && old_len > 0)
    for (int k = 0; k < min_p; k++)
      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  xelem (i, j, k) = old_data[old_d1*(old_d2*k+j)+i];

  // If the copy constructor is expensive, this may win.  Otherwise,
  // it may make more sense to just copy the value everywhere when
  // making the new ArrayRep.

  for (int k = 0; k < min_p; k++)
    for (int j = min_c; j < c; j++)
      for (int i = 0; i < min_r; i++)
	xelem (i, j, k) = val;

  for (int k = 0; k < min_p; k++)
    for (int j = 0; j < c; j++)
      for (int i = min_r; i < r; i++)
	xelem (i, j, k) = val;

  for (int k = min_p; k < p; k++)
    for (int j = 0; j < c; j++)
      for (int i = 0; i < r; i++)
	xelem (i, j, k) = val;

  if (--old_rep->count <= 0)
    delete old_rep;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
