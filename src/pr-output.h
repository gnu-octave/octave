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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_pr_output_h)
#define octave_pr_output_h 1

class ostream;

#include "oct-cmplx.h"

class ComplexMatrix;
class Matrix;
class Range;
class boolMatrix;
class charMatrix;

extern void
octave_print_internal (ostream& os, double d,
		       bool pr_as_read_syntax = false);

extern void
octave_print_internal (ostream& os, const Matrix& m,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (ostream& os, const Complex& c,
		       bool pr_as_read_syntax = false);

extern void
octave_print_internal (ostream& os, const ComplexMatrix& cm,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (ostream& os, const Range& r,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (ostream& os, const boolMatrix& m,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0);

extern void
octave_print_internal (ostream& os, const charMatrix& chm,
		       bool pr_as_read_syntax = false,
		       int extra_indent = 0,
		       bool pr_as_string = false);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
