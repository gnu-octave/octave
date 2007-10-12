/*

Copyright (C) 2005, 2006, 2007 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 Andy Adler

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

#if !defined (octave_sparse_complex_CHOL_h)
#define octave_sparse_complex_CHOL_h 1

#include "sparse-base-chol.h"
#include "dSparse.h"
#include "CSparse.h"

class
OCTAVE_API
SparseComplexCHOL : 
  public sparse_base_chol <SparseComplexMatrix, Complex, SparseMatrix>
{
public:

  SparseComplexCHOL (void) : 
    sparse_base_chol<SparseComplexMatrix, Complex, SparseMatrix> () { }

  SparseComplexCHOL (const SparseComplexMatrix& a, bool natural = true) : 
    sparse_base_chol<SparseComplexMatrix, Complex, SparseMatrix> 
  (a, natural) { }

  SparseComplexCHOL (const SparseComplexMatrix& a, octave_idx_type& info, 
		     bool natural = true) :
    sparse_base_chol<SparseComplexMatrix, Complex, SparseMatrix> 
  (a, info, natural) { }

  SparseComplexCHOL (const SparseComplexCHOL& a) : 
    sparse_base_chol<SparseComplexMatrix, Complex, SparseMatrix> (a) { }

  ~SparseComplexCHOL (void) { }

  SparseComplexCHOL& operator = (const SparseComplexCHOL& a)
    {
      if (this != &a)
	sparse_base_chol <SparseComplexMatrix, Complex, SparseMatrix> ::
	  operator = (a);

      return *this;
    }

  SparseComplexMatrix chol_matrix (void) const { return R(); }

  SparseComplexMatrix L (void) const 
    { return sparse_base_chol<SparseComplexMatrix, Complex, 
	SparseMatrix>:: L (); }

  SparseComplexMatrix R (void) const 
    { return sparse_base_chol<SparseComplexMatrix, Complex,
	SparseMatrix>:: R (); }

  octave_idx_type P (void) const 
   { return sparse_base_chol<SparseComplexMatrix, Complex, 
        SparseMatrix>:: P (); }

  ColumnVector perm (void) const 
    { return sparse_base_chol<SparseComplexMatrix, Complex, 
	SparseMatrix>:: perm (); }

  SparseMatrix Q (void) const 
    { return sparse_base_chol<SparseComplexMatrix, Complex, 
	SparseMatrix>:: Q (); }

  double rcond (void) const
    { return sparse_base_chol<SparseComplexMatrix, Complex, 
	SparseMatrix>:: rcond (); }

  // Compute the inverse of a matrix using the Cholesky factorization.
  SparseComplexMatrix inverse (void) const
    { return sparse_base_chol<SparseComplexMatrix, Complex, 
	SparseMatrix>:: inverse (); }
};

SparseComplexMatrix OCTAVE_API chol2inv (const SparseComplexMatrix& r);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
