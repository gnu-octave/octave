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

#if !defined (octave_sparse_LU_h)
#define octave_sparse_LU_h 1

#include "sparse-base-lu.h"
#include "dSparse.h"

class
SparseLU : public sparse_base_lu <SparseMatrix, double, SparseMatrix, double>
{
public:

  SparseLU (void) 
    : sparse_base_lu <SparseMatrix, double, SparseMatrix, double> () { }

  SparseLU (const SparseMatrix& a, double piv_thres = -1.0);

  SparseLU (const SparseMatrix& a, const ColumnVector& Qinit, 
	    double piv_thres = -1.0, bool FixedQ = false);

  SparseLU (const SparseLU& a) 
    : sparse_base_lu <SparseMatrix, double, SparseMatrix, double> (a) { }

  SparseLU& operator = (const SparseLU& a)
    {
      if (this != &a)
	sparse_base_lu <SparseMatrix, double, SparseMatrix, double> 
	  :: operator = (a);

      return *this;
    }

  ~SparseLU (void) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
