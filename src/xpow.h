// xpow.h                                               -*- C++ -*-
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

#if !defined (_xpow_h)
#define _xpow_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <Complex.h>

#include "Matrix.h"

#include "tree-const.h"

extern tree_constant xpow (double a, double b);
extern tree_constant xpow (double a, Matrix& b);
extern tree_constant xpow (double a, Complex& b);
extern tree_constant xpow (double a, ComplexMatrix& b);

extern tree_constant xpow (Matrix& a, double b);
extern tree_constant xpow (Matrix& a, Complex& b);

extern tree_constant xpow (Complex& a, double b);
extern tree_constant xpow (Complex& a, Matrix& b);
extern tree_constant xpow (Complex& a, Complex& b);
extern tree_constant xpow (Complex& a, ComplexMatrix& b);

extern tree_constant xpow (ComplexMatrix& a, double b);
extern tree_constant xpow (ComplexMatrix& a, Complex& b);

extern tree_constant elem_xpow (double a, Matrix& b);
extern tree_constant elem_xpow (double a, ComplexMatrix& b);

extern tree_constant elem_xpow (Matrix& a, double b);
extern tree_constant elem_xpow (Matrix& a, Matrix& b);
extern tree_constant elem_xpow (Matrix& a, Complex& b);
extern tree_constant elem_xpow (Matrix& a, ComplexMatrix& b);

extern tree_constant elem_xpow (Complex& a, Matrix& b);
extern tree_constant elem_xpow (Complex& a, ComplexMatrix& b);

extern tree_constant elem_xpow (ComplexMatrix& a, double b);
extern tree_constant elem_xpow (ComplexMatrix& a, Matrix& b);
extern tree_constant elem_xpow (ComplexMatrix& a, Complex& b);
extern tree_constant elem_xpow (ComplexMatrix& a, ComplexMatrix& b);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
