// pr-output.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (_pr_output_h)
#define _pr_output_h 1

#ifdef __GNUG__
#pragma interface
#endif

class ostrstream;

class Matrix;
class Complex;
class ComplexMatrix;
class Range;

extern void octave_print_internal (ostrstream& os, double d);
extern void octave_print_internal (ostrstream& os, Matrix& m);
extern void octave_print_internal (ostrstream& os, Complex& c);
extern void octave_print_internal (ostrstream& os, ComplexMatrix& cm);
extern void octave_print_internal (ostrstream& os, Range& r);

extern void set_format_style (int argc, char **argv);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
