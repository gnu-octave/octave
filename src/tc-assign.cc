// tc-assign.cc                                         -*- C++ -*-
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

#ifdef __GNUG__
#pragma implementation
#endif

#include "idx-vector.h"
#include "user-prefs.h"
#include "tree-const.h"
#include "utils.h"
#include "gripes.h"
#include "error.h"

#include "tc-inlines.cc"

void
tree_constant_rep::assign (tree_constant& rhs, tree_constant *args, int nargs)
{
  tree_constant rhs_tmp = rhs.make_numeric ();

  if (type_tag == string_constant || type_tag == range_constant) 
    force_numeric ();

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
    case unknown_constant:
      do_scalar_assignment (rhs_tmp, args, nargs);
      break;
    case complex_matrix_constant:
    case matrix_constant:
      do_matrix_assignment (rhs_tmp, args, nargs);
      break;
    case string_constant:
      error ("invalid assignment to string type");
      break;
    case range_constant:
    case magic_colon:
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::do_scalar_assignment (tree_constant& rhs,
					 tree_constant *args, int nargs)
{
  assert (type_tag == unknown_constant
	  || type_tag == scalar_constant
	  || type_tag == complex_scalar_constant);

  if (rhs.is_scalar_type () && valid_scalar_indices (args, nargs))
    {
      if (type_tag == unknown_constant || type_tag == scalar_constant)
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
	      error ("invalid assignment to scalar");
	      return;
	    }
	}
      else
	{
	  if (rhs.const_type () == scalar_constant)
	    {
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
	      error ("invalid assignment to scalar");
	      return;
	    }
	}
    }
  else if (user_pref.resize_on_range_error)
    {
      tree_constant_rep::constant_type old_type_tag = type_tag;

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
// destroy the current value.  tree_constant_rep::eval(int) will take
// care of converting single element matrices back to scalars.

      do_matrix_assignment (rhs, args, nargs);

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
  else if (nargs > 3 || nargs < 2)
    error ("invalid index expression for scalar type");
  else
    error ("index invalid or out of range for scalar type");
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant *args, int nargs)
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

  switch (nargs)
    {
    case 2:
      if (args == NULL_TREE_CONST)
	error ("matrix index is null");
      else if (args[1].is_undefined ())
	error ("matrix index is a null expression");
      else
	do_matrix_assignment (rhs, args[1]);
      break;
    case 3:
      if (args == NULL_TREE_CONST)
	error ("matrix indices are null");
      else if (args[1].is_undefined ())
	error ("first matrix index is a null expression");
      else if (args[2].is_undefined ())
	error ("second matrix index is a null expression");
      else
	do_matrix_assignment (rhs, args[1], args[2]);
      break;
    default:
      error ("too many indices for matrix expression");
      break;
    }
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  if (user_pref.do_fortran_indexing)
    fortran_style_matrix_assignment (rhs, i_arg);
  else if (nr <= 1 || nc <= 1)
    vector_assignment (rhs, i_arg);
  else
    error ("single index only valid for row or column vector");
}

void
tree_constant_rep::fortran_style_matrix_assignment (tree_constant& rhs,
						    tree_constant& i_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

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
	if (index_check (i-1, "") < 0)
	  return;
	if (nr <= 1 || nc <= 1)
	  maybe_resize (i-1);
	else if (range_max_check (i-1, nr * nc) < 0)
	  return;

	nr = rows ();
	nc = columns ();

	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    error ("for A(int) = X: X must be a scalar");
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
	int imax = ii.max ();

	if (nr <= 1 || nc <= 1)
	  maybe_resize (imax-1);
	else if (range_max_check (imax-1, len) < 0)
	  return;

	if (ii.capacity () != rhs_nr * rhs_nc)
	  {
	    error ("A(matrix) = X: X and matrix must have the same\
 number of elements"); 
	    return;
	  }
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
      fortran_style_matrix_assignment (rhs, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::vector_assignment (tree_constant& rhs, tree_constant& i_arg)
{
  int nr = rows ();
  int nc = columns ();

  assert ((nr == 1 || nc == 1 || (nr == 0 && nc == 0))
	  && ! user_pref.do_fortran_indexing); 

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

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
	do_vector_assign (rhs, iv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	if (rows () == 2 && is_zero_one (ri))
	  {
	    do_vector_assign (rhs, 1);
	  }
	else
	  {
	    int imax;
	    if (index_check (ri, imax, "") < 0)
	      return;
	    do_vector_assign (rhs, ri, imax);
	  }
      }
      break;
    case magic_colon:
      {
	int rhs_nr = rhs.rows ();
	int rhs_nc = rhs.columns ();

	if (! indexed_assign_conforms (nr, nc, rhs_nr, rhs_nc))
	  {
	    error ("A(:) = X: X and A must have the same dimensions");
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

void
tree_constant_rep::check_vector_assign (int rhs_nr, int rhs_nc,
					int ilen, const char *rm)
{
  int nr = rows ();
  int nc = columns ();

  if (nr == 1 && nc == 1)  // No orientation to preserve
    {
      if (! ( ilen == rhs_nr || ilen == rhs_nc))
	error ("A(%s) = X: X and %s must have the same number of\
 elements", rm, rm); 
    }
  else if (nr == 1)  // Preserve current row orientation
    {
      if (! (rhs_nr == 1 && rhs_nc == ilen))
	error ("A(%s) = X: where A is a row vector, X must also be a\
 row vector with the same number of elements as %s", rm, rm); 
    }
  else if (nc == 1)  // Preserve current column orientation
    {
      if (! (rhs_nc == 1 && rhs_nr == ilen))
	error ("A(%s) = X: where A is a column vector, X must also\
 be a column vector with the same number of elements as %s", rm, rm); 
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::do_vector_assign (tree_constant& rhs, int i)
{
  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.columns ();

  if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
    {
      error ("for A(int) = X: X must be a scalar");
      return;
    }

  maybe_resize (i);

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

void
tree_constant_rep::do_vector_assign (tree_constant& rhs, idx_vector& iv)
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

  int nr = rows ();
  int nc = columns ();

  if (nr == 1)
    {
      for (int i = 0; i < iv.capacity (); i++)
	REP_ELEM_ASSIGN (0, iv.elem (i), rhs_m.elem (0, i),
			 rhs_cm.elem (0, i), rhs.is_real_type ());
    }
  else if (nc == 1)
    {
      for (int i = 0; i < iv.capacity (); i++)
	REP_ELEM_ASSIGN (iv.elem (i), 0, rhs_m.elem (i, 0),
			 rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::do_vector_assign (tree_constant& rhs, Range& ri, int imax)
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

  maybe_resize (imax, f_orient);

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

void
tree_constant_rep::fortran_style_matrix_assignment
  (tree_constant& rhs, tree_constant_rep::constant_type mci)
{
  assert (rhs.is_matrix_type ());
  assert (mci == tree_constant_rep::magic_colon);

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
      error ("A(:) = X: X and A must have the same number of elements");
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

void
tree_constant_rep::fortran_style_matrix_assignment (tree_constant& rhs,
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
    error ("number of rows and columns must match for indexed assignment");
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant& i_arg, 
					 tree_constant& j_arg)
{
  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "row") < 0)
	  return;
	do_matrix_assignment (rhs, i, j_arg);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	idx_vector iv (mi, user_pref.do_fortran_indexing, "row", rows ());
	do_matrix_assignment (rhs, iv, j_arg);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range ri = tmp_i.range_value ();
	if (rows () == 2 && is_zero_one (ri))
	  {
	    do_matrix_assignment (rhs, 1, j_arg);
	  }
	else
	  {
	    int imax;
	    if (index_check (ri, imax, "row") < 0)
	      return;
	    do_matrix_assignment (rhs, ri, imax, j_arg);
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

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
	if (! indexed_assign_conforms (1, 1, rhs_nr, rhs_nc))
	  {
	    error ("A(int,int) = X, X must be a scalar");
	    return;
	  }
	maybe_resize (i, j);
	do_matrix_assignment (rhs, i, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! indexed_assign_conforms (1, jv.capacity (), rhs_nr, rhs_nc))
	  {
	    error ("A(int,matrix) = X: X must be a row vector with the\
 same number of elements as matrix"); 
	    return;
	  }
	maybe_resize (i, jv.max ());
	do_matrix_assignment (rhs, i, jv);
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (! indexed_assign_conforms (1, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    error ("A(int,range) = X: X must be a row vector with the\
 same number of elements as range"); 
	    return;
	  }

	if (columns () == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, i, 1);
	  }
	else
	  {
	    int jmax;
	    if (index_check (rj, jmax, "column") < 0)
	      return;
	    maybe_resize (i, jmax);
	    do_matrix_assignment (rhs, i, rj);
	  }
      }
      break;
    case magic_colon:
      {
	int nc = columns ();
	if (nc == 0 && rows () == 0 && rhs_nr == 1)
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
	  }
	else if (indexed_assign_conforms (1, nc, rhs_nr, rhs_nc))
	  maybe_resize (i, nc-1);
	else
	  {
	    error ("A(int,:) = X: X must be a row vector with the\
 same number of columns as A"); 
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

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
	    error ("A(matrix,int) = X: X must be a column vector with\
 the same number of elements as matrix");  
	    return;
	  }
	maybe_resize (iv.max (), j);
	do_matrix_assignment (rhs, iv, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! indexed_assign_conforms (iv.capacity (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    error ("A(r_matrix,c_matrix) = X: the number of rows in X\
 must match the number of elements in r_matrix and the number of\
 columns in X must match the number of elements in c_matrix");  
	    return;
	  }
	maybe_resize (iv.max (), jv.max ());
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
	    error ("A(matrix,range) = X: the number of rows in X must\
 match the number of elements in matrix and the number of columns in X\
 must match the number of elements in range");  
	    return;
	  }

	if (columns () == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, iv, 1);
	  }
	else
	  {
	    int jmax;
	    if (index_check (rj, jmax, "column") < 0)
	      return;
	    maybe_resize (iv.max (), jmax);
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

	if (! indexed_assign_conforms (iv.capacity (), new_nc,
				       rhs_nr, rhs_nc))
	  {
	    error ("A(matrix,:) = X: the number of rows in X must\
 match the number of elements in matrix, and the number of columns in\
 X must match the number of columns in A");
	    return;
	  }
	maybe_resize (iv.max (), new_nc-1);
	do_matrix_assignment (rhs, iv, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 Range& ri, int imax,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

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
	    error ("A(range,int) = X: X must be a column vector with\
 the same number of elements as range");
	    return;
	  }
	maybe_resize (imax, j);
	do_matrix_assignment (rhs, ri, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column",
		       columns ());
	if (! indexed_assign_conforms (ri.nelem (), jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    error ("A(range,matrix) = X: the number of rows in X must\
 match the number of elements in range and the number of columns in X\
 must match the number of elements in matrix");
	    return;
	  }
	maybe_resize (imax, jv.max ());
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
	    error ("A(r_range,c_range) = X: the number of rows in X\
 must match the number of elements in r_range and the number of\
 columns in X must match the number of elements in c_range\n");
	    return;
	  }

	if (columns () == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, ri, 1);
	  }
	else
	  {
	    int jmax;
	    if (index_check (rj, jmax, "column") < 0)
	      return;
	    maybe_resize (imax, jmax);
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

	if (! indexed_assign_conforms (ri.nelem (), new_nc, rhs_nr, rhs_nc))
	  {
	    error ("A(range,:) = X: the number of rows in X must match\
 the number of elements in range, and the number of columns in X must\
 match the number of columns in A");  
	    return;
	  }
	maybe_resize (imax, new_nc-1);
	do_matrix_assignment (rhs, ri, magic_colon);
      }
      break;
    default:
      panic_impossible ();
      break;
    }
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type i,
					 tree_constant& j_arg)
{
  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

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
	int nr = rows ();
	if (nr == 0 && columns () == 0 && rhs_nc == 1)
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
	  }
	else if (indexed_assign_conforms (nr, 1, rhs_nr, rhs_nc))
	  maybe_resize (nr-1, j);
	else
	  {
	    error ("A(:,int) = X: X must be a column vector with the\
 same number of rows as A"); 
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
	int nr = rows ();
	int new_nr = nr;
	if (nr == 0)
	  new_nr = rhs_nr;

	if (! indexed_assign_conforms (new_nr, jv.capacity (),
				       rhs_nr, rhs_nc))
	  {
	    error ("A(:,matrix) = X: the number of rows in X must\
 match the number of rows in A, and the number of columns in X must\
 match the number of elements in matrix");   
	    return;
	  }
	maybe_resize (new_nr-1, jv.max ());
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

	if (! indexed_assign_conforms (new_nr, rj.nelem (), rhs_nr, rhs_nc))
	  {
	    error ("A(:,range) = X: the number of rows in X must match\
 the number of rows in A, and the number of columns in X must match\
 the number of elements in range");
	    return;
	  }

	if (columns () == 2 && is_zero_one (rj) && rhs_nc == 1)
	  {
	    do_matrix_assignment (rhs, magic_colon, 1);
	  }
	else
	  {
	    int jmax;
	    if (index_check (rj, jmax, "column") < 0)
	      return;
	    maybe_resize (new_nr-1, jmax);
	    do_matrix_assignment (rhs, magic_colon, rj);
	  }
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i, int j)
{
  REP_ELEM_ASSIGN (i, j, rhs.double_value (), rhs.complex_value (),
		   rhs.is_real_type ());
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 idx_vector& jv)
{
  REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);
  for (int j = 0; j < jv.capacity (); j++)
    REP_ELEM_ASSIGN (i, jv.elem (j), rhs_m.elem (0, j),
		     rhs_cm.elem (0, j), rhs.is_real_type ());
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i, Range& rj)
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, int i,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

  int nc = columns ();

  if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int j = 0; j < nc; j++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (0, j), rhs_cm.elem (0, j),
			 rhs.is_real_type ());
    }
  else if (rhs.const_type () == scalar_constant && nc == 1)
    {
      REP_ELEM_ASSIGN (i, 0, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ()); 
    }
  else
    panic_impossible ();
}
  
void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, idx_vector& iv,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri, int j)
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 idx_vector& jv)
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 Range& rj)
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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs, Range& ri,
					 tree_constant_rep::constant_type mcj)
{
  assert (mcj == magic_colon);

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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 int j)
{
  assert (mci == magic_colon);

  int nr = rows ();

  if (rhs.is_matrix_type ())
    {
      REP_RHS_MATRIX (rhs, rhs_m, rhs_cm, rhs_nr, rhs_nc);

      for (int i = 0; i < nr; i++)
	REP_ELEM_ASSIGN (i, j, rhs_m.elem (i, 0),
			 rhs_cm.elem (i, 0), rhs.is_real_type ());
    }
  else if (rhs.const_type () == scalar_constant && nr == 1)
    {
      REP_ELEM_ASSIGN (0, j, rhs.double_value (),
		       rhs.complex_value (), rhs.is_real_type ());
    }
  else
    panic_impossible ();
}

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 idx_vector& jv)
{
  assert (mci == magic_colon);

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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 Range& rj)
{
  assert (mci == magic_colon);

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

void
tree_constant_rep::do_matrix_assignment (tree_constant& rhs,
					 tree_constant_rep::constant_type mci,
					 tree_constant_rep::constant_type mcj)
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
