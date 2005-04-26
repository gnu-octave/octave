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

#include "sparse-base-lu.h"

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
p_type
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pr (void) const
{
  octave_idx_type nr = Lfact.rows ();

  p_type Pout (nr, nr, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (P (i)) = i;
      Pout.data (i) = 1;
    }
  Pout.cidx (nr) = nr;

  return Pout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
p_type
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pc (void) const
{
  octave_idx_type nc = Ufact.cols ();

  p_type Pout (nc, nc, nc);

  for (octave_idx_type i = 0; i < nc; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (i) = Q (i);
      Pout.data (i) = 1;
    }
  Pout.cidx (nc) = nc;

  return Pout;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
