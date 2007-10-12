/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_xdiv_h)
#define octave_xdiv_h 1

#include "oct-cmplx.h"
#include "MatrixType.h"

class Matrix;
class ComplexMatrix;

class NDArray;
class ComplexNDArray;

extern Matrix xdiv (const Matrix& a, const Matrix& b, MatrixType &typ);
extern ComplexMatrix xdiv (const Matrix& a, const ComplexMatrix& b,
			   MatrixType &typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const Matrix& b,
			   MatrixType &typ);
extern ComplexMatrix xdiv (const ComplexMatrix& a, const ComplexMatrix& b,
			   MatrixType &typ);

extern Matrix x_el_div (double a, const Matrix& b);
extern ComplexMatrix x_el_div (double a, const ComplexMatrix& b);
extern ComplexMatrix x_el_div (const Complex a, const Matrix& b);
extern ComplexMatrix x_el_div (const Complex a, const ComplexMatrix& b);

extern NDArray x_el_div (double a, const NDArray& b);
extern ComplexNDArray x_el_div (double a, const ComplexNDArray& b);
extern ComplexNDArray x_el_div (const Complex a, const NDArray& b);
extern ComplexNDArray x_el_div (const Complex a, const ComplexNDArray& b);

extern Matrix xleftdiv (const Matrix& a, const Matrix& b, MatrixType &typ);
extern ComplexMatrix xleftdiv (const Matrix& a, const ComplexMatrix& b,
			       MatrixType &typ);
extern ComplexMatrix xleftdiv (const ComplexMatrix& a, const Matrix& b,
			       MatrixType &typ);
extern ComplexMatrix xleftdiv (const ComplexMatrix& a, const ComplexMatrix& b,
			       MatrixType &typ);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
