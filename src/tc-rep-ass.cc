// tc-rep-ass.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <ctype.h>
#include <string.h>
#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#include "mx-base.h"
#include "Range.h"

#include "arith-ops.h"
#include "variables.h"
#include "sysdep.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"
#include "utils.h"
#include "pager.h"
#include "pr-output.h"
#include "tree-const.h"
#include "idx-vector.h"
#include "oct-map.h"

#include "tc-inlines.h"

// Top-level tree-constant function that handles assignments.  Only
// decide if the left-hand side is currently a scalar or a matrix and
// hand off to other functions to do the real work.

void
TC_REP::assign (tree_constant& rhs, const Octave_object& args)
{
  tree_constant rhs_tmp = rhs.make_numeric ();

// This is easier than actually handling assignments to strings.
// An assignment to a range will normally require a conversion to a
// vector since it will normally destroy the equally-spaced property
// of the range elements.

  if (is_defined () && ! is_numeric_type ())
    force_numeric ();

  if (error_state)
    return;

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
    case unknown_constant:
      do_scalar_assignment (rhs_tmp, args);
      break;

    case complex_matrix_constant:
    case matrix_constant:
      do_matrix_assignment (rhs_tmp, args);
      break;

    default:
      ::error ("invalid assignment to %s", type_as_string ());
      break;
    }
}

// Assignments to scalars.  If resize_on_range_error is true,
// this can convert the left-hand side to a matrix.

void
TC_REP::do_scalar_assignment (const tree_constant& rhs,
			      const Octave_object& args) 
{
  assert (type_tag == unknown_constant
	  || type_tag == scalar_constant
	  || type_tag == complex_scalar_constant);

  int nargin = args.length ();

  if ((rhs.is_scalar_type () || rhs.is_zero_by_zero ())
      && valid_scalar_indices (args))
    {
      if (rhs.is_zero_by_zero ())
	{
	  if (type_tag == complex_scalar_constant)
	    delete complex_scalar;

	  matrix = new Matrix (0, 0);
	  type_tag = matrix_constant;
	}
      else if (type_tag == unknown_constant || type_tag == scalar_constant)
	{
	  if (rhs.const_type () == scalar_constant)
	    {
	      scalar = rhs.double_value ();
	      type_tag = scalar_constant;
	    }
	  else if (rhs.const_type () == complex_scalar_constant)
	    {
	      complex_scalar = new Complex (rhs.complex_value ());
	      type_tag = complex_scalar_constant;
	    }
	  else
	    {
	      ::error ("invalid assignment to scalar");
	      return;
	    }
	}
      else
	{
	  if (rhs.const_type () == scalar_constant)
	    {
	      delete complex_scalar;
	      scalar = rhs.double_value ();
	      type_tag = scalar_constant;
	    }
	  else if (rhs.const_type () == complex_scalar_constant)
	    {
	      *complex_scalar = rhs.complex_value ();
	      type_tag = complex_scalar_constant;
	    }
	  else
	    {
	      ::error ("invalid assignment to scalar");
	      return;
	    }
	}
    }
  else if (user_pref.resize_on_range_error)
    {
      TC_REP::constant_type old_type_tag = type_tag;

      if (type_tag == complex_scalar_constant)
	{
	  Complex *old_complex = complex_scalar;
	  complex_matrix = new ComplexMatrix (1, 1, *complex_scalar);
	  type_tag = complex_matrix_constant;
	  delete old_complex;
	}
      else if (type_tag == scalar_constant)
	{
	  matrix = new Matrix (1, 1, scalar);
	  type_tag = matrix_constant;
	}

// If there is an error, the call to do_matrix_assignment should not
// destroy the current value.
// TC_REP::eval(int) will take
// care of converting single element matrices back to scalars.

      do_matrix_assignment (rhs, args);

// I don't think there's any other way to revert back to unknown
// constant types, so here it is.

      if (old_type_tag == unknown_constant && error_state)
	{
	  if (type_tag == matrix_constant)
	    delete matrix;
	  else if (type_tag == complex_matrix_constant)
	    delete complex_matrix;

	  type_tag = unknown_constant;
	}
    }
  else if (nargin > 2 || nargin < 1)
    ::error ("invalid index expression for scalar type");
  else
    ::error ("index invalid or out of range for scalar type");
}

// Assignments to matrices (and vectors).
//
// For compatibility with Matlab, we allow assignment of an empty
// matrix to an expression with empty indices to do nothing.

void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      const Octave_object& args)
{
  assert (type_tag == unknown_constant
	  || type_tag == matrix_constant
	  || type_tag == complex_matrix_constant);

  if (type_tag == matrix_constant && rhs.is_complex_type ())
    {
      Matrix *old_matrix = matrix;
      complex_matrix = new ComplexMatrix (*matrix);
      type_tag = complex_matrix_constant;
      delete old_matrix;
    }
  else if (type_tag == unknown_constant)
    {
      if (rhs.is_complex_type ())
	{
	  complex_matrix = new ComplexMatrix ();
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  matrix = new Matrix ();
	  type_tag = matrix_constant;
	}
    }

  int nargin = args.length ();

// The do_matrix_assignment functions can't handle empty matrices, so
// don't let any pass through here.
  switch (nargin)
    {
    case 1:
      {
	tree_constant arg = args(0);

	if (arg.is_undefined ())
	  ::error ("matrix index is undefined");
	else
	  do_matrix_assignment (rhs, arg);
      }
      break;

    case 2:
      {
	tree_constant arg_a = args(0);
	tree_constant arg_b = args(1);

	if (arg_a.is_undefined ())
	  ::error ("first matrix index is undefined");
	else if (arg_b.is_undefined ())
	  ::error ("second matrix index is undefined");
	else if (arg_a.is_empty () || arg_b.is_empty ())
	  {
	    if (! rhs.is_empty ())
	      {
		::error ("in assignment expression, a matrix index is empty");
		::error ("but the right hand side is not an empty matrix");
	      }
// XXX FIXME XXX -- to really be correct here, we should probably
// check to see if the assignment conforms, but that seems like more
// work than it's worth right now...
	  }
	else
	  do_matrix_assignment (rhs, arg_a, arg_b);
      }
      break;

    default:
      if (nargin == 0)
	::error ("matrix indices expected, but none provided");
      else
	::error ("too many indices for matrix expression");
      break;
    }
}

// Matrix assignments indexed by a single value.

void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      const tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  if (user_pref.do_fortran_indexing || nr <= 1 || nc <= 1)
    {
      if (i_arg.is_empty ())
	{
	  if (! rhs.is_empty ())
	    {
	      ::error ("in assignment expression, matrix index is empty but");
	      ::error ("right hand side is not an empty matrix");
	    }
// XXX FIXME XXX -- to really be correct here, we should probably
// check to see if the assignment conforms, but that seems like more
// work than it's worth right now...

// The assignment functions can't handle empty matrices, so don't let
// any pass through here.
	  return;
	}

// We can't handle the case of assigning to a vector first, since even
// then, the two operations are not equivalent.  For example, the
// expression V(:) = M is handled differently depending on whether the
// user specified do_fortran_indexing = "true".

      if (user_pref.do_fortran_indexing)
	fortran_style_matrix_assignment (rhs, i_arg);
      else if (nr <= 1 || nc <= 1)
	vector_assignment (rhs, i_arg);
      else
	panic_impossible ();
    }
  else
    ::error ("single index only valid for row or column vector");
}

// Fortran-style assignments.  Matrices are assumed to be stored in
// column-major order and it is ok to use a single index for
// multi-dimensional matrices.

void
TC_REP::fortran_style_matrix_assignment (const tree_constant& rhs,
					 const tree_constant& i_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  TC_REP::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = NINT (tmp_i.double_value ());
	int idx = i - 1;

	if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (idx < nr * nc)
	      {
		convert_to_row_or_column_vector ();

		nr = rows ();
		nc = columns ();

		if (nr == 1)
		  delete_column (idx);
		else if (nc == 1)
		  delete_row (idx);
		else
		  panic_impossible ();
	      }
	    return;
	  }

	if (index_check (idx, "") < 0)
	  return;

	if (nr <= 1 || nc <= 1)
	  {
	    maybe_resize (idx);
	    if (error_state)
	      return;
	  }
	else if (range_max_check (idx, nr * nc) < 0)
	  return;

	nr = rows ();
	nc = columns ();

	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    ::error ("for A(int) = X: X must be a scalar");
	    return;
	  }
	int ii = fortran_row (i, nr) - 1;
	int jj = fortran_column (i, nr) - 1;
	do_matrix_assignment (rhs, ii, jj);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	int len = nr * nc;
	idx_vector ii (mi, 1, "", len);  // Always do fortran indexing here...
	if (! ii)
	  return;

	if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    ii.sort_uniq ();
	    int num_to_delete = 0;
	    for (int i = 0; i < ii.length (); i++)
	      {
		if (ii.elem (i) < len)
		  num_to_delete++;
		else
		  break;
	      }

	    if (num_to_delete > 0)
	      {
		if (num_to_delete != ii.length ())
		  ii.shorten (num_to_delete);

		convert_to_row_or_column_vector ();

		nr = rows ();
		nc = columns ();

		if (nr == 1)
		  delete_columns (ii);
		else if (nc == 1)
		  delete_rows (ii);
		else
		  panic_impossible ();
	      }
	    return;
	  }

	if (nr <= 1 || nc <= 1)
	  {
	    maybe_resize (ii.max ());
	    if (error_state)
	      return;
	  }
	else if (range_max_check (ii.max (), len) < 0)
	  return;

	int ilen = ii.capacity ();

	if (ilen != rhs_nr * rhs_nc)
	  {
	    ::error ("A(matrix) = X: X and matrix must have the same number");
	    ::error ("of elements");
	  }
	else if (ilen == 1 && rhs.is_scalar_type ())
	  {
	    int nr = rows ();
	    int idx = ii.elem (0);
	    int ii = fortran_row (idx + 1, nr) - 1;
	    int jj = fortran_column (idx + 1, nr) - 1;

	    if (rhs.const_type () == scalar_constant)
	      matrix->elem (ii, jj) = rhs.double_value ();
	    else if (rhs.const_type () == complex_scalar_constant)
	      complex_matrix->elem (ii, jj) = rhs.complex_value ();
	    else
	      panic_impossible ();
	  }
	else
	  fortran_style_matrix_assignment (rhs, ii);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      gripe_range_invalid ();
      break;

    case magic_colon:
// a(:) = [] is equivalent to a(:,:) = [].
      if (rhs_nr == 0 && rhs_nc == 0)
	do_matrix_assignment (rhs, magic_colon, magic_colon);
      else
	fortran_style_matrix_assignment (rhs, magic_colon);
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Fortran-style assignment for vector index.

void
TC_REP::fortran_style_matrix_assignment (const tree_constant& rhs,
					 idx_vector& i)
{
  assert (rhs.is_matrix_type ());

  int ilen = i.capacity ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  int len = rhs_nr * rhs_nc;

  if (len == ilen)
    {
      int nr = rows ();
      if (rhs.const_type () == matrix_constant)
	{
	  double *cop_out = rhs_m.fortran_vec ();
	  for (int k = 0; k < len; k++)
	    {
	      int ii = fortran_row (i.elem (k) + 1, nr) - 1;
	      int jj = fortran_column (i.elem (k) + 1, nr) - 1;

	      matrix->elem (ii, jj) = *cop_out++;
	    }
	}
      else
	{
	  Complex *cop_out = rhs_cm.fortran_vec ();
	  for (int k = 0; k < len; k++)
	    {
	      int ii = fortran_row (i.elem (k) + 1, nr) - 1;
	      int jj = fortran_column (i.elem (k) + 1, nr) - 1;

	      complex_matrix->elem (ii, jj) = *cop_out++;
	    }
	}
    }
  else
    ::error ("number of rows and columns must match for indexed assignment");
}

// Fortran-style assignment for colon index.

void
TC_REP::fortran_style_matrix_assignment (const tree_constant& rhs,
					 TC_REP::constant_type mci)
{
  assert (rhs.is_matrix_type () && mci == TC_REP::magic_colon);

  int nr = rows ();
  int nc = columns ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  int rhs_size = rhs_nr * rhs_nc;
  if (rhs_size == 0)
    {
      if (rhs.const_type () == matrix_constant)
	{
	  delete matrix;
	  matrix = new Matrix (0, 0);
	  return;
	}
      else
	panic_impossible ();
    }
  else if (nr*nc != rhs_size)
    {
      ::error ("A(:) = X: X and A must have the same number of elements");
      return;
    }

  if (rhs.const_type () == matrix_constant)
    {
      double *cop_out = rhs_m.fortran_vec ();
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  matrix->elem (i, j) = *cop_out++;
    }
  else
    {
      Complex *cop_out = rhs_cm.fortran_vec ();
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  complex_matrix->elem (i, j) = *cop_out++;
    }
}

// Assignments to vectors.  Hand off to other functions once we know
// what kind of index we have.  For a colon, it is the same as
// assignment to a matrix indexed by two colons.

void
TC_REP::vector_assignment (const tree_constant& rhs,
			   const tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  assert ((nr == 1 || nc == 1 || (nr == 0 && nc == 0))
	  && ! user_pref.do_fortran_indexing);

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "") < 0)
	  return;
	do_vector_assign (rhs, i);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	int len = nr * nc;
	idx_vector iv (mi, user_pref.do_fortran_indexing, "", len);
	if (! iv)
	  return;

	do_vector_assign (rhs, iv);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	int len = nr * nc;
	if (len == 2 && is_zero_one (ri))
	  {
	    do_vector_assign (rhs, 1);
	  }
	else if (len == 2 && is_one_zero (ri))
	  {
	    do_vector_assign (rhs, 0);
	  }
	else
	  {
	    if (index_check (ri, "") < 0)
	      return;
	    do_vector_assign (rhs, ri);
	  }
      }
      break;

    case magic_colon:
      {
	int rhs_nr = rhs.rows ();
	int rhs_nc = rhs.columns ();

	if (! indexed_assign_conforms (nr, nc, rhs_nr, rhs_nc))
	  {
	    ::error ("A(:) = X: X and A must have the same dimensions");
	    return;
	  }
	do_matrix_assignment (rhs, magic_colon, magic_colon);
      }
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Check whether an indexed assignment to a vector is valid.

void
TC_REP::check_vector_assign (int rhs_nr, int rhs_nc, int ilen, const char *rm)
{
  int nr = rows ();
  int nc = columns ();

  if ((nr == 1 && nc == 1) || nr == 0 || nc == 0)  // No orientation.
    {
      if (! (ilen == rhs_nr || ilen == rhs_nc))
	{
	  ::error ("A(%s) = X: X and %s must have the same number of elements",
		 rm, rm);
	}
    }
  else if (nr == 1)  // Preserve current row orientation.
    {
      if (! (rhs_nr == 1 && rhs_nc == ilen))
	{
	  ::error ("A(%s) = X: where A is a row vector, X must also be a", rm);
	  ::error ("row vector with the same number of elements as %s", rm);
	}
    }
  else if (nc == 1)  // Preserve current column orientation.
    {
      if (! (rhs_nc == 1 && rhs_nr == ilen))
	{
	  ::error ("A(%s) = X: where A is a column vector, X must also be", rm);
	  ::error ("a column vector with the same number of elements as %s", rm);
	}
    }
  else
    panic_impossible ();
}

// Assignment to a vector with an integer index.

void
TC_REP::do_vector_assign (const tree_constant& rhs, int i)
{
  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  if (indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
    {
      maybe_resize (i);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      if (nr == 1)
	{
	  REP_ELEM_ASSIGN (0, i, rhs.double_value (), rhs.complex_value (),
			   rhs.is_real_type ());
	}
      else if (nc == 1)
	{
	  REP_ELEM_ASSIGN (i, 0, rhs.double_value (), rhs.complex_value (),
			   rhs.is_real_type ());
	}
      else
	panic_impossible ();
    }
  else if (rhs_nr == 0 && rhs_nc == 0)
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      if (i < 0 || i >= len)
	{
	  ::error ("A(int) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_column (i);
      else if (nc == 1)
	delete_row (i);
      else
	panic_impossible ();
    }
  else
    {
      ::error ("for A(int) = X: X must be a scalar");
      return;
    }
}

// Assignment to a vector with a vector index.

void
TC_REP::do_vector_assign (const tree_constant& rhs, idx_vector& iv)
{
  if (rhs.is_zero_by_zero ())
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      if (iv.max () >= len)
	{
	  ::error ("A(matrix) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_columns (iv);
      else if (nc == 1)
	delete_rows (iv);
      else
	panic_impossible ();
    }
  else if (rhs.is_scalar_type ())
    {
      int nr = rows ();
      int nc = columns ();

      if (iv.capacity () == 1)
	{
	  int idx = iv.elem (0);

	  if (nr == 1)
	    {
	      REP_ELEM_ASSIGN (0, idx, rhs.double_value (),
			       rhs.complex_value (), rhs.is_real_type ());
	    }
	  else if (nc == 1)
	    {
	      REP_ELEM_ASSIGN (idx, 0, rhs.double_value (),
			       rhs.complex_value (), rhs.is_real_type ());
	    }
	  else
	    panic_impossible ();
	}
      else
	{
	  if (nr == 1)
	    {
	      ::error ("A(matrix) = X: where A is a row vector, X must also be a");
	      ::error ("row vector with the same number of elements as matrix");
	    }
	  else if (nc == 1)
	    {
	      ::error ("A(matrix) = X: where A is a column vector, X must also be a");
	      ::error ("column vector with the same number of elements as matrix");
	    }
	  else
	    panic_impossible ();
	}
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int ilen = iv.capacity ();
      check_vector_assign (rhs_nr, rhs_nc, ilen, "matrix");
      if (error_state)
	return;

      force_orient f_orient = no_orient;
      if (rhs_nr == 1 && rhs_nc != 1)
	f_orient = row_orient;
      else if (rhs_nc == 1 && rhs_nr != 1)
	f_orient = column_orient;

      maybe_resize (iv.max (), f_orient);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      if (nr == 1 && rhs_nr == 1)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    REP_ELEM_ASSIGN (0, iv.elem (i), rhs_m.elem (0, i),
			     rhs_cm.elem (0, i), rhs.is_real_type ());
	}
      else if (nc == 1 && rhs_nc == 1)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    REP_ELEM_ASSIGN (iv.elem (i), 0, rhs_m.elem (i, 0),
			     rhs_cm.elem (i, 0), rhs.is_real_type ());
	}
      else
	::error ("A(vector) = X: X must be the same size as vector");
    }
  else
    panic_impossible ();
}

// Assignment to a vector with a range index.

void
TC_REP::do_vector_assign (const tree_constant& rhs, Range& ri)
{
  if (rhs.is_zero_by_zero ())
    {
      int nr = rows ();
      int nc = columns ();

      int len = MAX (nr, nc);

      int b = tree_to_mat_idx (ri.min ());
      int l = tree_to_mat_idx (ri.max ());
      if (b < 0 || l >= len)
	{
	  ::error ("A(range) = []: index out of range");
	  return;
	}

      if (nr == 1)
	delete_columns (ri);
      else if (nc == 1)
	delete_rows (ri);
      else
	panic_impossible ();
    }
  else if (rhs.is_scalar_type ())
    {
      int nr = rows ();
      int nc = columns ();

      if (nr == 1)
	{
	  ::error ("A(range) = X: where A is a row vector, X must also be a");
	  ::error ("row vector with the same number of elements as range");
	}
      else if (nc == 1)
	{
	  ::error ("A(range) = X: where A is a column vector, X must also be a");
	  ::error ("column vector with the same number of elements as range");
	}
      else
	panic_impossible ();
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int ilen = ri.nelem ();
      check_vector_assign (rhs_nr, rhs_nc, ilen, "range");
      if (error_state)
	return;

      force_orient f_orient = no_orient;
      if (rhs_nr == 1 && rhs_nc != 1)
	f_orient = row_orient;
      else if (rhs_nc == 1 && rhs_nr != 1)
	f_orient = column_orient;

      maybe_resize (tree_to_mat_idx (ri.max ()), f_orient);
      if (error_state)
	return;

      int nr = rows ();
      int nc = columns ();

      double b = ri.base ();
      double increment = ri.inc ();

      if (nr == 1)
	{
	  for (int i = 0; i < ri.nelem (); i++)
	    {
	      double tmp = b + i * increment;
	      int col = tree_to_mat_idx (tmp);
	      REP_ELEM_ASSIGN (0, col, rhs_m.elem (0, i), rhs_cm.elem (0, i),
			       rhs.is_real_type ());
	    }
	}
      else if (nc == 1)
	{
	  for (int i = 0; i < ri.nelem (); i++)
	    {
	      double tmp = b + i * increment;
	      int row = tree_to_mat_idx (tmp);
	      REP_ELEM_ASSIGN (row, 0, rhs_m.elem (i, 0), rhs_cm.elem (i, 0),
			       rhs.is_real_type ());
	    }
	}
      else
	panic_impossible ();
    }
  else
    panic_impossible ();
}

// Matrix assignment indexed by two values.  This function determines
// the type of the first arugment, checks as much as possible, and
// then calls one of a set of functions to handle the specific cases:
//
//   M (integer, arg2) = RHS  (MA1)
//   M (vector,  arg2) = RHS  (MA2)
//   M (range,   arg2) = RHS  (MA3)
//   M (colon,   arg2) = RHS  (MA4)
//
// Each of those functions determines the type of the second argument
// and calls another function to handle the real work of doing the
// assignment.

void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      const tree_constant& i_arg,
			      const tree_constant& j_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = tree_to_mat_idx (tmp_i.double_value ());
	do_matrix_assignment (rhs, i, j_arg);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	idx_vector iv (mi, user_pref.do_fortran_indexing, "row", rows ());
	if (! iv)
	  return;

	do_matrix_assignment (rhs, iv, j_arg);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	int nr = rows ();
	if (nr == 2 && is_zero_one (ri))
	  {
	    do_matrix_assignment (rhs, 1, j_arg);
	  }
	else if (nr == 2 && is_one_zero (ri))
	  {
	    do_matrix_assignment (rhs, 0, j_arg);
	  }
	else
	  {
	    if (index_check (ri, "row") < 0)
	      return;
	    do_matrix_assignment (rhs, ri, j_arg);
	  }
      }
      break;

    case magic_colon:
      do_matrix_assignment (rhs, magic_colon, j_arg);
      break;

    default:
      panic_impossible ();
      break;
    }
}

/* MA1 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, int i,
			      const tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	if (index_check (i, "row") < 0)
	  return;
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,int) = X, X must be a scalar");
	    return;
	  }
	maybe_resize (i, j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, i, j);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	if (index_check (i, "row") < 0)
	  return;
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (1, jv.capacity (), rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,matrix) = X: X must be a row vector with the same");
	    ::error ("number of elements as matrix");
	    return;
	  }
	maybe_resize (i, jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, i, jv);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	if (index_check (i, "row") < 0)
	  return;
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (1, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    ::error ("A(int,range) = X: X must be a row vector with the same");
	    ::error ("number of elements as range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, i, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, i, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;
	    maybe_resize (i, tree_to_mat_idx (rj.max ()));
	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, i, rj);
	  }
      }
      break;

    case magic_colon:
      {
	int nc = columns ();
	int nr = rows ();
	if (i == -1 && nr == 1 && rhs_nr == 0 && rhs_nc == 0
	    || index_check (i, "row") < 0)
	  return;
	else if (nc == 0 && nr == 0 && rhs_nr == 1)
	  {
	    if (rhs.is_complex_type ())
	      {
		complex_matrix = new ComplexMatrix ();
		type_tag = complex_matrix_constant;
	      }
	    else
	      {
		matrix = new Matrix ();
		type_tag = matrix_constant;
	      }
	    maybe_resize (i, rhs_nc-1);
	    if (error_state)
	      return;
	  }
	else if (indexed_assign_conforms (1, nc, rhs_nr, rhs_nc))
	  {
	    maybe_resize (i, nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (i < 0 || i >= nr)
	      {
		::error ("A(int,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(int,:) = X: X must be a row vector with the same");
	    ::error ("number of columns as A");
	    return;
	  }

	do_matrix_assignment (rhs, i, magic_colon);
      }
      break;

    default:
      panic_impossible ();
      break;
    }
}

/* MA2 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      idx_vector& iv, const tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (iv.capacity (), 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(matrix,int) = X: X must be a column vector with the");
	    ::error ("same number of elements as matrix");
	    return;
	  }
	maybe_resize (iv.max (), j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, iv, j);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (iv.capacity (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(r_mat,c_mat) = X: the number of rows in X must match");
	    ::error ("the number of elements in r_mat and the number of");
	    ::error ("columns in X must match the number of elements in c_mat");
	    return;
	  }
	maybe_resize (iv.max (), jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, iv, jv);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (iv.capacity (), rj.nelem (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(matrix,range) = X: the number of rows in X must match");
	    ::error ("the number of elements in matrix and the number of");
	    ::error ("columns in X must match the number of elements in range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, iv, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, iv, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;
	    maybe_resize (iv.max (), tree_to_mat_idx (rj.max ()));
	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, iv, rj);
	  }
      }
      break;

    case magic_colon:
      {
	int nc = columns ();
	int new_nc = nc;
	if (nc == 0)
	  new_nc = rhs_nc;

	if (indexed_assign_conforms (iv.capacity (), new_nc,
				     rhs_nr, rhs_nc))
	  {
	    maybe_resize (iv.max (), new_nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (iv.max () >= rows ())
	      {
		::error ("A(matrix,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(matrix,:) = X: the number of rows in X must match the");
	    ::error ("number of elements in matrix, and the number of columns");
	    ::error ("in X must match the number of columns in A");
	    return;
	  }

	do_matrix_assignment (rhs, iv, magic_colon);
      }
      break;

    default:
      panic_impossible ();
      break;
    }
}

/* MA3 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, Range& ri,
			      const tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return;
	if (! indexed_assign_conforms (ri.nelem (), 1, rhs_nr, rhs_nc))
	  {
	    ::error ("A(range,int) = X: X must be a column vector with the");
	    ::error ("same number of elements as range");
	    return;
	  }
	maybe_resize (tree_to_mat_idx (ri.max ()), j);
	if (error_state)
	  return;

	do_matrix_assignment (rhs, ri, j);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	if (! indexed_assign_conforms (ri.nelem (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(range,matrix) = X: the number of rows in X must match");
	    ::error ("the number of elements in range and the number of");
	    ::error ("columns in X must match the number of elements in matrix");
	    return;
	  }
	maybe_resize (tree_to_mat_idx (ri.max ()), jv.max ());
	if (error_state)
	  return;

	do_matrix_assignment (rhs, ri, jv);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (ri.nelem (), rj.nelem (),
				       rhs_nr, rhs_nc))
	  {
	    ::error ("A(r_range,c_range) = X: the number of rows in X must");
	    ::error ("match the number of elements in r_range and the number");
	    ::error ("of columns in X must match the number of elements in");
	    ::error ("c_range");
	    return;
	  }

	int nc = columns ();
	if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, ri, 1);
	  }
	else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, ri, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return;

	    maybe_resize (tree_to_mat_idx (ri.max ()),
			  tree_to_mat_idx (rj.max ()));

	    if (error_state)
	      return;

	    do_matrix_assignment (rhs, ri, rj);
	  }
      }
      break;

    case magic_colon:
      {
	int nc = columns ();
	int new_nc = nc;
	if (nc == 0)
	  new_nc = rhs_nc;

	if (indexed_assign_conforms (ri.nelem (), new_nc, rhs_nr, rhs_nc))
	  {
	    maybe_resize (tree_to_mat_idx (ri.max ()), new_nc-1);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    int b = tree_to_mat_idx (ri.min ());
	    int l = tree_to_mat_idx (ri.max ());
	    if (b < 0 || l >= rows ())
	      {
		::error ("A(range,:) = []: row index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(range,:) = X: the number of rows in X must match the");
	    ::error ("number of elements in range, and the number of columns");
	    ::error ("in X must match the number of columns in A");
	    return;
	  }

	do_matrix_assignment (rhs, ri, magic_colon);
      }
      break;

    default:
      panic_impossible ();
      break;
    }
}

/* MA4 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      TC_REP::constant_type i,
			      const tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	int nr = rows ();
	int nc = columns ();
	if (j == -1 && nc == 1 && rhs_nr == 0 && rhs_nc == 0
	    || index_check (j, "column") < 0)
	  return;
	if (nr == 0 && nc == 0 && rhs_nc == 1)
	  {
	    if (rhs.is_complex_type ())
	      {
		complex_matrix = new ComplexMatrix ();
		type_tag = complex_matrix_constant;
	      }
	    else
	      {
		matrix = new Matrix ();
		type_tag = matrix_constant;
	      }
	    maybe_resize (rhs_nr-1, j);
	    if (error_state)
	      return;
	  }
	else if (indexed_assign_conforms (nr, 1, rhs_nr, rhs_nc))
	  {
	    maybe_resize (nr-1, j);
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (j < 0 || j >= nc)
	      {
		::error ("A(:,int) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,int) = X: X must be a column vector with the same");
	    ::error ("number of rows as A");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, j);
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! jv)
	  return;

	int nr = rows ();
	int new_nr = nr;
	if (nr == 0)
	  new_nr = rhs_nr;

	if (indexed_assign_conforms (new_nr, jv.capacity (),
				     rhs_nr, rhs_nc))
	  {
	    maybe_resize (new_nr-1, jv.max ());
	    if (error_state)
	      return;
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    if (jv.max () >= columns ())
	      {
		::error ("A(:,matrix) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,matrix) = X: the number of rows in X must match the");
	    ::error ("number of rows in A, and the number of columns in X must");
	    ::error ("match the number of elements in matrix");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, jv);
      }
      break;

    case string_constant:
      gripe_string_invalid ();
      break;

    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	int nr = rows ();
	int new_nr = nr;
	if (nr == 0)
	  new_nr = rhs_nr;

	if (indexed_assign_conforms (new_nr, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    int nc = columns ();
	    if (nc == 2 && is_zero_one (rj) && rhs_nc == 1)
	      {
		do_matrix_assignment (rhs, magic_colon, 1);
	      }
	    else if (nc == 2 && is_one_zero (rj) && rhs_nc == 1)
	      {
		do_matrix_assignment (rhs, magic_colon, 0);
	      }
	    else
	      {
		if (index_check (rj, "column") < 0)
		  return;
		maybe_resize (new_nr-1, tree_to_mat_idx (rj.max ()));
		if (error_state)
		  return;
	      }
	  }
	else if (rhs_nr == 0 && rhs_nc == 0)
	  {
	    int b = tree_to_mat_idx (rj.min ());
	    int l = tree_to_mat_idx (rj.max ());
	    if (b < 0 || l >= columns ())
	      {
		::error ("A(:,range) = []: column index out of range");
		return;
	      }
	  }
	else
	  {
	    ::error ("A(:,range) = X: the number of rows in X must match the");
	    ::error ("number of rows in A, and the number of columns in X");
	    ::error ("must match the number of elements in range");
	    return;
	  }

	do_matrix_assignment (rhs, magic_colon, rj);
      }
      break;

    case magic_colon:
// a(:,:) = foo is equivalent to a = foo.
      do_matrix_assignment (rhs, magic_colon, magic_colon);
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Functions that actually handle assignment to a matrix using two
// index values.
//
//                   idx2
//            +---+---+----+----+
//   idx1     | i | v |  r | c  |
//   ---------+---+---+----+----+
//   integer  | 1 | 5 |  9 | 13 |
//   ---------+---+---+----+----+
//   vector   | 2 | 6 | 10 | 14 |
//   ---------+---+---+----+----+
//   range    | 3 | 7 | 11 | 15 |
//   ---------+---+---+----+----+
//   colon    | 4 | 8 | 12 | 16 |
//   ---------+---+---+----+----+

/* 1 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, int i, int j)
{
  REP_ELEM_ASSIGN (i, j, rhs.double_value (), rhs.complex_value (),
		   rhs.is_real_type ());
}

/* 2 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, int i, idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int j = 0; j < jv.capacity (); j++)
    REP_ELEM_ASSIGN (i, jv.elem (j), rhs_m.elem (0, j),
		     rhs_cm.elem (0, j), rhs.is_real_type ());
}

/* 3 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, int i, Range& rj)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int j = 0; j < rj.nelem (); j++)
    {
      double tmp = b + j * increment;
      int col = tree_to_mat_idx (tmp);
      REP_ELEM_ASSIGN (i, col, rhs_m.elem (0, j), rhs_cm.elem (0, j),
		       rhs.is_real_type ());
    }
}

/* 4 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, int i,
			      TC_REP::constant_type mcj)
{
  assert (mcj == magic_colon);

  int nc = columns ();

  if (rhs.is_zero_by_zero ())
    {
      delete_row (i);
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int j = 0; j < nc; j++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (0, j), rhs_cm.elem (0, j),
			 rhs.is_real_type ());
    }
  else if (rhs.is_scalar_type () && nc == 1)
    {
      REP_ELEM_ASSIGN (i, 0, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

/* 5 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      idx_vector& iv, int j)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, 0),
		       rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
}

/* 6 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      idx_vector& iv, idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jv.capacity (); j++)
	{
	  int col = jv.elem (j);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 7 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      idx_vector& iv, Range& rj)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int i = 0; i < iv.capacity (); i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < rj.nelem (); j++)
	{
	  double tmp = b + j * increment;
	  int col = tree_to_mat_idx (tmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 8 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      idx_vector& iv, TC_REP::constant_type mcj)
{
  assert (mcj == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_rows (iv);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nc = columns ();

      for (int j = 0; j < nc; j++)
	{
	  for (int i = 0; i < iv.capacity (); i++)
	    {
	      int row = iv.elem (i);
	      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 9 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs, Range& ri, int j)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = ri.base ();
  double increment = ri.inc ();

  for (int i = 0; i < ri.nelem (); i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, 0),
		       rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
}

/* 10 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      Range& ri, idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  double b = ri.base ();
  double increment = ri.inc ();

  for (int j = 0; j < jv.capacity (); j++)
    {
      int col = jv.elem (j);
      for (int i = 0; i < ri.nelem (); i++)
	{
	  double tmp = b + i * increment;
	  int row = tree_to_mat_idx (tmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem (i, j),
			   rhs_m.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 11 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      Range& ri, Range& rj)
{
  double ib = ri.base ();
  double iinc = ri.inc ();
  double jb = rj.base ();
  double jinc = rj.inc ();

  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

  for (int i = 0; i < ri.nelem (); i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < rj.nelem (); j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);
	  REP_ELEM_ASSIGN (row, col, rhs_m.elem  (i, j),
			   rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 12 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      Range& ri, TC_REP::constant_type mcj)
{
  assert (mcj == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_rows (ri);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      double ib = ri.base ();
      double iinc = ri.inc ();

      int nc = columns ();

      for (int i = 0; i < ri.nelem (); i++)
	{
	  double itmp = ib + i * iinc;
	  int row = tree_to_mat_idx (itmp);
	  for (int j = 0; j < nc; j++)
	    REP_ELEM_ASSIGN (row, j, rhs_m.elem (i, j),
			     rhs_cm.elem (i, j), rhs.is_real_type ());
	}
    }
}

/* 13 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      TC_REP::constant_type mci, int j)
{
  assert (mci == magic_colon);

  int nr = rows ();

  if (rhs.is_zero_by_zero ())
    {
      delete_column (j);
    }
  else if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int i = 0; i < nr; i++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (i, 0),
			 rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
  else if (rhs.is_scalar_type () && nr == 1)
    {
      REP_ELEM_ASSIGN (0, j, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

/* 14 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      TC_REP::constant_type mci, idx_vector& jv)
{
  assert (mci == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_columns (jv);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nr = rows ();

      for (int i = 0; i < nr; i++)
	{
	  for (int j = 0; j < jv.capacity (); j++)
	    {
	      int col = jv.elem (j);
	      REP_ELEM_ASSIGN (i, col, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 15 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      TC_REP::constant_type mci, Range& rj)
{
  assert (mci == magic_colon);

  if (rhs.is_zero_by_zero ())
    {
      delete_columns (rj);
    }
  else
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      int nr = rows ();

      double jb = rj.base ();
      double jinc = rj.inc ();

      for (int j = 0; j < rj.nelem (); j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);
	  for (int i = 0; i < nr; i++)
	    {
	      REP_ELEM_ASSIGN (i, col, rhs_m.elem (i, j),
			       rhs_cm.elem (i, j), rhs.is_real_type ());
	    }
	}
    }
}

/* 16 */
void
TC_REP::do_matrix_assignment (const tree_constant& rhs,
			      TC_REP::constant_type mci,
			      TC_REP::constant_type mcj)
{
  assert (mci == magic_colon && mcj == magic_colon);

  switch (type_tag)
    {
    case scalar_constant:
      break;

    case matrix_constant:
      delete matrix;
      break;

    case complex_scalar_constant:
      delete complex_scalar;
      break;

    case complex_matrix_constant:
      delete complex_matrix;
      break;

    case string_constant:
      delete [] string;
      break;

    case range_constant:
      delete range;
      break;

    case magic_colon:
    default:
      panic_impossible ();
      break;
    }

  type_tag = rhs.const_type ();

  switch (type_tag)
    {
    case scalar_constant:
      scalar = rhs.double_value ();
      break;

    case matrix_constant:
      matrix = new Matrix (rhs.matrix_value ());
      break;

    case string_constant:
      string = strsave (rhs.string_value ());
      break;

    case complex_matrix_constant:
      complex_matrix = new ComplexMatrix (rhs.complex_matrix_value ());
      break;

    case complex_scalar_constant:
      complex_scalar = new Complex (rhs.complex_value ());
      break;

    case range_constant:
      range = new Range (rhs.range_value ());
      break;

    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
}

// Functions for deleting rows or columns of a matrix.  These are used
// to handle statements like
//
//   M (i, j) = []

void
TC_REP::delete_row (int idx)
{
  if (type_tag == matrix_constant)
    {
      int nr = matrix->rows ();
      int nc = matrix->columns ();
      Matrix *new_matrix = new Matrix (nr-1, nc);
      int ii = 0;
      for (int i = 0; i < nr; i++)
	{
	  if (i != idx)
	    {
	      for (int j = 0; j < nc; j++)
		new_matrix->elem (ii, j) = matrix->elem (i, j);
	      ii++;
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      int nr = complex_matrix->rows ();
      int nc = complex_matrix->columns ();
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-1, nc);
      int ii = 0;
      for (int i = 0; i < nr; i++)
	{
	  if (i != idx)
	    {
	      for (int j = 0; j < nc; j++)
		new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
	      ii++;
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
TC_REP::delete_rows (idx_vector& iv)
{
  iv.sort_uniq ();
  int num_to_delete = iv.length ();

  int nr = rows ();
  int nc = columns ();

// If deleting all rows of a column vector, make result 0x0.
  if (nc == 1 && num_to_delete == nr)
    nc = 0;

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (i == iv.elem (idx))
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (i == iv.elem (idx))
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
TC_REP::delete_rows (Range& ri)
{
  ri.sort ();
  int num_to_delete = ri.nelem ();

  int nr = rows ();
  int nc = columns ();

// If deleting all rows of a column vector, make result 0x0.
  if (nc == 1 && num_to_delete == nr)
    nc = 0;

  double ib = ri.base ();
  double iinc = ri.inc ();

  int max_idx = tree_to_mat_idx (ri.max ());

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      double itmp = ib + idx * iinc;
	      int row = tree_to_mat_idx (itmp);

	      if (i == row && row <= max_idx)
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr-num_to_delete, nc);
      if (nr > num_to_delete)
	{
	  int ii = 0;
	  int idx = 0;
	  for (int i = 0; i < nr; i++)
	    {
	      double itmp = ib + idx * iinc;
	      int row = tree_to_mat_idx (itmp);

	      if (i == row && row <= max_idx)
		idx++;
	      else
		{
		  for (int j = 0; j < nc; j++)
		    new_matrix->elem (ii, j) = complex_matrix->elem (i, j);
		  ii++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
TC_REP::delete_column (int idx)
{
  if (type_tag == matrix_constant)
    {
      int nr = matrix->rows ();
      int nc = matrix->columns ();
      Matrix *new_matrix = new Matrix (nr, nc-1);
      int jj = 0;
      for (int j = 0; j < nc; j++)
	{
	  if (j != idx)
	    {
	      for (int i = 0; i < nr; i++)
		new_matrix->elem (i, jj) = matrix->elem (i, j);
	      jj++;
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      int nr = complex_matrix->rows ();
      int nc = complex_matrix->columns ();
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-1);
      int jj = 0;
      for (int j = 0; j < nc; j++)
	{
	  if (j != idx)
	    {
	      for (int i = 0; i < nr; i++)
		new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
	      jj++;
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
TC_REP::delete_columns (idx_vector& jv)
{
  jv.sort_uniq ();
  int num_to_delete = jv.length ();

  int nr = rows ();
  int nc = columns ();

// If deleting all columns of a row vector, make result 0x0.
  if (nr == 1 && num_to_delete == nc)
    nr = 0;

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == jv.elem (idx))
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == jv.elem (idx))
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

void
TC_REP::delete_columns (Range& rj)
{
  rj.sort ();
  int num_to_delete = rj.nelem ();

  int nr = rows ();
  int nc = columns ();

// If deleting all columns of a row vector, make result 0x0.
  if (nr == 1 && num_to_delete == nc)
    nr = 0;

  double jb = rj.base ();
  double jinc = rj.inc ();

  int max_idx = tree_to_mat_idx (rj.max ());

  if (type_tag == matrix_constant)
    {
      Matrix *new_matrix = new Matrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      double jtmp = jb + idx * jinc;
	      int col = tree_to_mat_idx (jtmp);

	      if (j == col && col <= max_idx)
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete matrix;
      matrix = new_matrix;
    }
  else if (type_tag == complex_matrix_constant)
    {
      ComplexMatrix *new_matrix = new ComplexMatrix (nr, nc-num_to_delete);
      if (nc > num_to_delete)
	{
	  int jj = 0;
	  int idx = 0;
	  for (int j = 0; j < nc; j++)
	    {
	      double jtmp = jb + idx * jinc;
	      int col = tree_to_mat_idx (jtmp);

	      if (j == col && col <= max_idx)
		idx++;
	      else
		{
		  for (int i = 0; i < nr; i++)
		    new_matrix->elem (i, jj) = complex_matrix->elem (i, j);
		  jj++;
		}
	    }
	}
      delete complex_matrix;
      complex_matrix = new_matrix;
    }
  else
    panic_impossible ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
