// Matrix manipulations.
/*

Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <iostream>

#include "Array-util.h"
#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"

// boolMatrix class.

bool
boolMatrix::operator == (const boolMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
boolMatrix::operator != (const boolMatrix& a) const
{
  return !(*this == a);
}

boolMatrix&
boolMatrix::insert (const boolMatrix& a, octave_idx_type r, octave_idx_type c)
{
  Array<bool>::insert (a, r, c);
  return *this;
}

// unary operations

boolMatrix
boolMatrix::operator ! (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  boolMatrix b (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// other operations

boolMatrix
boolMatrix::diag (octave_idx_type k) const
{
  return Array<bool>::diag (k);
}

// FIXME Do these really belong here?  Maybe they should be
// in a base class?

boolMatrix
boolMatrix::all (int dim) const
{
  return do_mx_red_op<bool, bool> (*this, dim, mx_inline_all);
}

boolMatrix
boolMatrix::any (int dim) const
{
  return do_mx_red_op<bool, bool> (*this, dim, mx_inline_any);
}

MM_BOOL_OPS (boolMatrix, boolMatrix)
MS_BOOL_OPS (boolMatrix, bool)
SM_BOOL_OPS (bool, boolMatrix)
MM_CMP_OPS (boolMatrix, boolMatrix)
