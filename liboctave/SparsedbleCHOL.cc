/*

Copyright (C) 2005 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#include "SparsedbleCHOL.h"

// Instantiate the base CHOL class for the type we need
#define OCTAVE_CHOLMOD_TYPE CHOLMOD_REAL
#include "sparse-base-chol.h"
#include "sparse-base-chol.cc"
template class sparse_base_chol <SparseMatrix, double, SparseMatrix>;

// Compute the inverse of a matrix using the Cholesky factorization.
SparseMatrix
chol2inv (const SparseMatrix& r)
{
  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();
  SparseMatrix retval;

  if (r_nr == r_nc)
    {
      SparseType mattype (r);
      int typ = mattype.type (false);
      double rcond;
      octave_idx_type info;
      SparseMatrix rinv;

      if (typ == SparseType::Upper)
	{
	  rinv = r.inverse(mattype, info, rcond, true, false);
	  retval = rinv.transpose() * rinv;
	}
      else if (typ == SparseType::Lower)
	{
	  rinv = r.transpose().inverse(mattype, info, rcond, true, false);
	  retval = rinv.transpose() * rinv;
	}
      else
	(*current_liboctave_error_handler) 
	  ("spchol2inv requires triangular matrix");
    }
  else
    (*current_liboctave_error_handler) ("spchol2inv requires square matrix");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
