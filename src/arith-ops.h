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

#ifdef __GNUG__
#pragma interface
#endif

#include "tree-const.h"

extern tree_constant
do_unary_op (double d, tree::expression_type t);

extern tree_constant
do_unary_op (Matrix& a, tree::expression_type t);

extern tree_constant
do_unary_op (Complex& c, tree::expression_type t);

extern tree_constant
do_unary_op (ComplexMatrix& a, tree::expression_type t);

extern tree_constant
do_binary_op (double a, double b, tree::expression_type t);

extern tree_constant
do_binary_op (double a, Matrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (double a, Complex& b, tree::expression_type t);

extern tree_constant
do_binary_op (double a, ComplexMatrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (Matrix& a, double b, tree::expression_type t);

extern tree_constant
do_binary_op (Matrix& a, Matrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (Matrix& a, Complex& b, tree::expression_type t);

extern tree_constant
do_binary_op (Matrix& a, ComplexMatrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (Complex& a, double b, tree::expression_type t);

extern tree_constant
do_binary_op (Complex& a, Matrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (Complex& a, Complex& b, tree::expression_type t);

extern tree_constant
do_binary_op (Complex& a, ComplexMatrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (ComplexMatrix& a, double b, tree::expression_type t);

extern tree_constant
do_binary_op (ComplexMatrix& a, Matrix& b, tree::expression_type t);

extern tree_constant
do_binary_op (ComplexMatrix& a, Complex& b, tree::expression_type t);

extern tree_constant
do_binary_op (ComplexMatrix& a, ComplexMatrix& b, tree::expression_type t);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
