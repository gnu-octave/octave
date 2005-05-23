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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_pr_output_h)
#define octave_pr_output_h 1

#include <iostream>

#include "oct-cmplx.h"

template <typename T> class ArrayN;
class ComplexMatrix;
class ComplexNDArray;
class Matrix;
class NDArray;
class Range;
class boolMatrix;
class boolNDArray;
class charMatrix;
class charNDArray;
class Cell;

#include "intNDArray.h"
#include "oct-inttypes.h"

extern void
octave_print_internal (std::ostream& os, double d,
		       bool pr_as_read_syntax = false);

extern void
octave_print_internal (std::ostream& os, const Matrix& m,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const NDArray& nda,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const Complex& c,
		       bool pr_as_read_syntax = false);

extern void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const ComplexNDArray& nda,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const Range& r,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const boolMatrix& m,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const boolNDArray& m,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const charMatrix& chm,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0,
		       bool pr_as_string = false);

extern void
octave_print_internal (std::ostream& os, const charNDArray& nda,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0,
		       bool pr_as_string = false);

extern void
octave_print_internal (std::ostream& os, const std::string& s,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (std::ostream& os, const ArrayN<std::string>& sa,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

template <class T>
extern void
octave_print_internal (std::ostream& os, const intNDArray<T>& sa,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

template <class T>
extern void
octave_print_internal (std::ostream& os, const octave_int<T>& sa,
		       bool pr_as_read_syntax = false);

extern void
octave_print_internal (std::ostream& os, const Cell& cell,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0,
		       bool pr_as_string = false);

// TRUE means that the dimensions of empty objects should be printed
// like this: x = [](2x0).
extern bool Vprint_empty_dimensions;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
