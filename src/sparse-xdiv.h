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

#if !defined (octave_sparse_xdiv_h)
#define octave_sparse_xdiv_h 1

#include "oct-cmplx.h"
#include "MatrixType.h"

class SparseMatrix;
class SparseComplexMatrix;

extern Matrix xdiv (const Matrix& a, const SparseMatrix& b, MatrixType &typ);
extern ComplexMatrix xdiv (const Matrix& a, const SparseComplexMatrix& b,
			   MatrixType &typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const SparseMatrix& b,
			   MatrixType &typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, 
			   const SparseComplexMatrix& b, MatrixType &typ);

extern SparseMatrix xdiv (const SparseMatrix& a, const SparseMatrix& b,
			  MatrixType &typ);
extern SparseComplexMatrix xdiv (const SparseMatrix& a, 
				 const SparseComplexMatrix& b, MatrixType &typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a, 
				 const SparseMatrix& b, MatrixType &typ);
extern SparseComplexMatrix xdiv (const SparseComplexMatrix& a, 
				 const SparseComplexMatrix& b, MatrixType &typ);

extern Matrix x_el_div (double a, const SparseMatrix& b);
extern ComplexMatrix x_el_div (double a, const SparseComplexMatrix& b);
extern ComplexMatrix x_el_div (const Complex a, const SparseMatrix& b);
extern ComplexMatrix x_el_div (const Complex a, 
			       const SparseComplexMatrix& b);

extern Matrix xleftdiv (const SparseMatrix& a, const Matrix& b, 
			MatrixType& typ);
extern ComplexMatrix xleftdiv (const SparseMatrix& a, const ComplexMatrix& b,
			       MatrixType &typ);
extern ComplexMatrix xleftdiv (const SparseComplexMatrix& a, const Matrix& b,
			       MatrixType &typ);
extern ComplexMatrix xleftdiv (const SparseComplexMatrix& a, 
			       const ComplexMatrix& b, MatrixType &typ);

extern SparseMatrix xleftdiv (const SparseMatrix& a, const SparseMatrix& b,
			      MatrixType &typ);
extern SparseComplexMatrix xleftdiv (const SparseMatrix& a, 
				     const SparseComplexMatrix& b, MatrixType &typ);
extern SparseComplexMatrix xleftdiv (const SparseComplexMatrix& a, 
				     const SparseMatrix& b, MatrixType &typ);
extern SparseComplexMatrix xleftdiv (const SparseComplexMatrix& a, 
				     const SparseComplexMatrix& b, MatrixType &typ);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
