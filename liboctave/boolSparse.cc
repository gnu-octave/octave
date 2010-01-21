/*

Copyright (C) 2004, 2005, 2006, 2007, 2008 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

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
#include <vector>

#include "config.h"
#include "quit.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

#include "boolSparse.h"

// SparseBoolMatrix class.

bool
SparseBoolMatrix::operator == (const SparseBoolMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nzmax ();
  octave_idx_type nr_a = a.rows ();
  octave_idx_type nc_a = a.cols ();
  octave_idx_type nz_a = a.nzmax ();

  if (nr != nr_a || nc != nc_a || nz != nz_a)
    return false;

  for (octave_idx_type i = 0; i < nc + 1; i++)
    if (cidx(i) != a.cidx(i))
	return false;

  for (octave_idx_type i = 0; i < nz; i++)
    if (data(i) != a.data(i) || ridx(i) != a.ridx(i))
      return false;

  return true;
}

bool
SparseBoolMatrix::operator != (const SparseBoolMatrix& a) const
{
  return !(*this == a);
}

SparseBoolMatrix&
SparseBoolMatrix::insert (const SparseBoolMatrix& a, octave_idx_type r, octave_idx_type c)
{
  Sparse<bool>::insert (a, r, c);
  return *this;
}

SparseBoolMatrix&
SparseBoolMatrix::insert (const SparseBoolMatrix& a, const Array<octave_idx_type>& indx)
{
  Sparse<bool>::insert (a, indx);
  return *this;
}

SparseBoolMatrix
SparseBoolMatrix::concat (const SparseBoolMatrix& rb, const Array<octave_idx_type>& ra_idx)
{
  // Don't use numel to avoid all possiblity of an overflow
  if (rb.rows () > 0 && rb.cols () > 0)
    insert (rb, ra_idx(0), ra_idx(1));
  return *this;
}

// unary operations

SparseBoolMatrix
SparseBoolMatrix::operator ! (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz1 = nzmax ();
  octave_idx_type nz2 = nr*nc - nz1;
   
  SparseBoolMatrix r (nr, nc, nz2);
   
  octave_idx_type ii = 0;
  octave_idx_type jj = 0;
  r.cidx (0) = 0;
  for (octave_idx_type i = 0; i < nc; i++)
    {
      for (octave_idx_type j = 0; j < nr; j++)
	{
	  if (jj < cidx(i+1) && ridx(jj) == j)
	    jj++;
	  else
	    {
	      r.data(ii) = true;
	      r.ridx(ii++) = j;
	    }
	}
      r.cidx (i+1) = ii;
    }

  return r;
}

// other operations

// FIXME Do these really belong here?  Maybe they should be
// in a base class?

SparseBoolMatrix
SparseBoolMatrix::all (int dim) const
{
  SPARSE_ALL_OP (dim);
}

SparseBoolMatrix
SparseBoolMatrix::any (int dim) const
{
  SPARSE_ANY_OP (dim);
}

SparseBoolMatrix
SparseBoolMatrix::diag (octave_idx_type k) const
{
  return Sparse<bool>::diag (k);
}

boolMatrix
SparseBoolMatrix::matrix_value (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  boolMatrix retval (nr, nc, false);
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = cidx(j); i < cidx(j+1); i++)
      retval.elem (ridx(i), j) = data (i);

  return retval;
}

std::ostream&
operator << (std::ostream& os, const SparseBoolMatrix& a)
{
  octave_idx_type nc = a.cols ();

   // add one to the printed indices to go from
   //  zero-based to one-based arrays
   for (octave_idx_type j = 0; j < nc; j++)  
     {
       octave_quit ();
       for (octave_idx_type i = a.cidx(j); i < a.cidx(j+1); i++)
	 os << a.ridx(i) + 1 << " "  << j + 1 << " " << a.data(i) << "\n";
     }
   
  return os;
}

std::istream&
operator >> (std::istream& is, SparseBoolMatrix& a)
{
  typedef SparseBoolMatrix::element_type elt_type;

  return read_sparse_matrix<elt_type> (is, a, octave_read_value<bool>);
}

SparseBoolMatrix
SparseBoolMatrix::squeeze (void) const 
{ 
  return Sparse<bool>::squeeze (); 
}

SparseBoolMatrix
SparseBoolMatrix::index (idx_vector& i, int resize_ok) const 
{ 
  return Sparse<bool>::index (i, resize_ok); 
}

SparseBoolMatrix
SparseBoolMatrix::index (idx_vector& i, idx_vector& j, int resize_ok) const 
{ 
  return Sparse<bool>::index (i, j, resize_ok); 
}
  
SparseBoolMatrix
SparseBoolMatrix::index (Array<idx_vector>& ra_idx, int resize_ok) const 
{ 
  return Sparse<bool>::index (ra_idx, resize_ok); 
}

SparseBoolMatrix
SparseBoolMatrix::reshape (const dim_vector& new_dims) const
{
  return Sparse<bool>::reshape (new_dims);
}

SparseBoolMatrix
SparseBoolMatrix::permute (const Array<octave_idx_type>& vec, bool inv) const
{
  return Sparse<bool>::permute (vec, inv);
}

SparseBoolMatrix
SparseBoolMatrix::ipermute (const Array<octave_idx_type>& vec) const
{
  return Sparse<bool>::ipermute (vec);
}

SPARSE_SMS_EQNE_OPS (SparseBoolMatrix, false, , bool, false, )
SPARSE_SMS_BOOL_OPS (SparseBoolMatrix, bool, false)

SPARSE_SSM_EQNE_OPS (bool, false, , SparseBoolMatrix, false, )
SPARSE_SSM_BOOL_OPS (bool, SparseBoolMatrix, false)

SPARSE_SMSM_EQNE_OPS (SparseBoolMatrix, false, , SparseBoolMatrix, false, )
SPARSE_SMSM_BOOL_OPS (SparseBoolMatrix, SparseBoolMatrix, false)
