// Helper functions for arithmetic operations.            -*- C++ -*-
// Used by the tree class.                    
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

#if !defined (_arith_ops_h)
#define _arith_ops_h 1

#include "tree-base.h"

class Complex;
class Matrix;
class ComplexMatrix;
class tree_constant;

extern tree_constant do_unary_op (double d, tree::expression_type t);

extern tree_constant do_unary_op (const Matrix& a, tree::expression_type t);

extern tree_constant do_unary_op (const Complex& c, tree::expression_type t);

extern tree_constant do_unary_op (const ComplexMatrix& a,
				  tree::expression_type t);

extern tree_constant do_binary_op (double a, double b,
				   tree::expression_type t);

extern tree_constant do_binary_op (double a, const Matrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (double a, const Complex& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (double a, const ComplexMatrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Matrix& a, double b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Matrix& a, const Matrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Matrix& a, const Complex& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Matrix& a, const ComplexMatrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Complex& a, double b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Complex& a, const Matrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Complex& a, const Complex& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const Complex& a, const ComplexMatrix& b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const ComplexMatrix& a, double b,
				   tree::expression_type t);

extern tree_constant do_binary_op (const ComplexMatrix& a, const Matrix& b,
				   tree::expression_type t); 

extern tree_constant do_binary_op (const ComplexMatrix& a, const Complex& b,
				   tree::expression_type t); 

extern tree_constant do_binary_op (const ComplexMatrix& a,
				   const ComplexMatrix& b,
				   tree::expression_type t); 

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
