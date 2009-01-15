/*

Copyright (C) 2008 Jaroslav Hajek

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "PermMatrix.h"
#include "idx-vector.h"
#include "error.h"
#include "Array-util.h"

static void
gripe_invalid_permutation (void)
{
  (*current_liboctave_error_handler)
    ("PermMatrix: invalid permutation vector");
}

PermMatrix::PermMatrix (const Array<octave_idx_type>& p, bool colp, bool check)
  : Array<octave_idx_type> (p), _colp(colp)
{
  if (check)
    {
      if (! idx_vector (p).is_permutation (p.length ()))
        {
          gripe_invalid_permutation ();
          Array<octave_idx_type>::operator = (Array<octave_idx_type> ());
        }
    }
}

PermMatrix::PermMatrix (const idx_vector& idx, bool colp, octave_idx_type n)
  : Array<octave_idx_type> (), _colp(colp)
{
  octave_idx_type len = idx.length (n);
  if (! idx.is_permutation (len))
    gripe_invalid_permutation ();
  else
    {
      Array<octave_idx_type> idxa (len);
      for (octave_idx_type i = 0; i < len; i++) idxa(i) = idx(i);
      Array<octave_idx_type>::operator = (idxa);
    }
}

PermMatrix::PermMatrix (octave_idx_type n)
  : Array<octave_idx_type> (n), _colp (false)
{
  for (octave_idx_type i = 0; i < n; i++) xelem (i) = i;
}

octave_idx_type 
PermMatrix::checkelem (octave_idx_type i, octave_idx_type j) const
{
  octave_idx_type len = Array<octave_idx_type>::length ();
  if (i < 0 || j < 0 || i > len || j > len)
    {
      (*current_liboctave_error_handler) ("index out of range");
      return 0;
    }
  else
    return elem (i, j);
}


PermMatrix 
PermMatrix::transpose (void) const
{
  PermMatrix retval (*this);
  retval._colp = ! retval._colp;
  return retval;
}

PermMatrix 
PermMatrix::inverse (void) const
{
  return transpose ();
}

octave_idx_type 
PermMatrix::determinant (void) const
{
  Array<octave_idx_type> pa = *this;
  octave_idx_type len = pa.length (), *p = pa.fortran_vec ();
  bool neg = false;
  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_idx_type j = p[i];
      if (j != i)
        {
          p[i] = p[j];
          p[j] = j;
          neg = ! neg;
        }
    }
  
  return neg ? -1 : 1;
}

PermMatrix 
operator *(const PermMatrix& a, const PermMatrix& b)
{
  const Array<octave_idx_type> ia = a.pvec (), ib = b.pvec ();
  PermMatrix r;
  octave_idx_type n = a.columns ();
  if (n != b.rows ())
    gripe_nonconformant ("operator *", n, n, b.rows (), b.rows ());
  else if (a._colp == b._colp)
    {
      r = PermMatrix ((a._colp 
                       ? ia.index (idx_vector (ib)) 
                       : ib.index (idx_vector (ia))), a._colp, false);
    }
  else
    {
      Array<octave_idx_type> ra (n);
      if (a._colp)
        ra.assign (idx_vector (ib), ia);
      else
        ra.assign (idx_vector (ia), ib);
      r = PermMatrix (ra, a._colp, false);
    }

  return r;
}
