// tc-index.cc                                         -*- C++ -*-
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

#include "idx-vector.h"
#include "user-prefs.h"
#include "tree-const.h"
#include "utils.h"
#include "gripes.h"
#include "error.h"

#include "tc-inlines.cc"

int
tree_constant_rep::valid_as_scalar_index (void) const
{
  int valid = type_tag == magic_colon
    || (type_tag == scalar_constant && NINT (scalar) == 1)
    || (type_tag == range_constant
	&& range->nelem () == 1 && NINT (range->base ()) == 1);

  return valid;
}

tree_constant
tree_constant_rep::do_scalar_index (const tree_constant *args,
				    int nargs) const
{
  if (valid_scalar_indices (args, nargs))
    {
      if (type_tag == scalar_constant)
	return tree_constant (scalar);
      else if (type_tag == complex_scalar_constant)
	return tree_constant (*complex_scalar);
      else
	panic_impossible ();
    }
  else
    {
      int rows = 0;
      int cols = 0;

      switch (nargs)
	{
	case 3:
	  {
	    if (args[2].is_matrix_type ())
	      {
		Matrix mj = args[2].matrix_value ();

		idx_vector j (mj, user_pref.do_fortran_indexing, "");
		if (! j)
		  return tree_constant ();

		int len = j.length ();
		if (len == j.ones_count ())
		  cols = len;
	      }
	    else if (args[2].const_type () == magic_colon
		     || (args[2].is_scalar_type ()
			 && NINT (args[2].double_value ()) == 1))
	      {
		cols = 1;
	      }
	    else
	      break;
	  }
// Fall through...
	case 2:
	  {
	    if (args[1].is_matrix_type ())
	      {
		Matrix mi = args[1].matrix_value ();

		idx_vector i (mi, user_pref.do_fortran_indexing, "");
		if (! i)
		  return tree_constant ();

		int len = i.length ();
		if (len == i.ones_count ())
		  rows = len;
	      }
	    else if (args[1].const_type () == magic_colon
		     || (args[1].is_scalar_type ()
			 && NINT (args[1].double_value ()) == 1))
	      {
		rows = 1;
	      }
	    else if (args[1].is_scalar_type ()
		     && NINT (args[1].double_value ()) == 0)
	      {
		Matrix m (0, 0);
		return tree_constant (m);
	      }
	    else
	      break;

	    if (cols == 0)
	      {
		if (user_pref.prefer_column_vectors)
		  cols = 1;
		else
		  {
		    cols = rows;
		    rows = 1;
		  }
	      }

	    if (type_tag == scalar_constant)
	      {
		Matrix m (rows, cols, scalar);
		return tree_constant (m);
	      }
	    else if (type_tag == complex_scalar_constant)
	      {
		ComplexMatrix cm (rows, cols, *complex_scalar);
		return tree_constant (cm);
	      }
	    else
	      panic_impossible ();
	  }
	  break;
	default:
	  ::error ("illegal number of arguments for scalar type");
	  return tree_constant ();
	  break;
	}
    }

  ::error ("index invalid or out of range for scalar type");
  return tree_constant ();
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant *args,
				    int nargin) const
{
  tree_constant retval;

  switch (nargin)
    {
    case 2:
      if (args == NULL_TREE_CONST)
	::error ("matrix index is null");
      else if (args[1].is_undefined ())
	::error ("matrix index is a null expression");
      else
	retval = do_matrix_index (args[1]);
      break;
    case 3:
      if (args == NULL_TREE_CONST)
	::error ("matrix indices are null");
      else if (args[1].is_undefined ())
	::error ("first matrix index is a null expression");
      else if (args[2].is_undefined ())
	::error ("second matrix index is a null expression");
      else
	retval = do_matrix_index (args[1], args[2]);
      break;
    default:
      ::error ("too many indices for matrix expression");
      break;
    }

  return  retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  int nr = rows ();
  int nc = columns ();

  if (user_pref.do_fortran_indexing)
    retval = fortran_style_matrix_index (i_arg);
  else if (nr <= 1 || nc <= 1)
    retval = do_vector_index (i_arg);
  else
    ::error ("single index only valid for row or column vector");

  return retval;
}

tree_constant
tree_constant_rep::fortran_style_matrix_index
  (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int i = NINT (tmp_i.double_value ());
	int ii = fortran_row (i, nr) - 1;
	int jj = fortran_column (i, nr) - 1;
	if (index_check (i-1, "") < 0)
	  return tree_constant ();
	if (range_max_check (i-1, nr * nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (ii, jj);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	if (mi.rows () == 0 || mi.columns () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
// Yes, we really do want to call this with mi.
	    retval = fortran_style_matrix_index (mi);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      gripe_range_invalid ();
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::fortran_style_matrix_index (const Matrix& mi) const
{
  assert (is_matrix_type ());

  tree_constant retval;

  int nr = rows ();
  int nc = columns ();

  int len = nr * nc;

  int index_nr = mi.rows ();
  int index_nc = mi.columns ();

  if (index_nr >= 1 && index_nc >= 1)
    {
      const double *cop_out = (const double *) NULL;
      const Complex *c_cop_out = (const Complex *) NULL;
      int real_type = type_tag == matrix_constant;
      if (real_type)
	cop_out = matrix->data ();
      else
	c_cop_out = complex_matrix->data ();

      const double *cop_out_index = mi.data ();

      idx_vector iv (mi, 1, "", len);
      if (! iv)
	return tree_constant ();

      int result_size = iv.length ();

      if (nc == 1 || (nr != 1 && iv.one_zero_only ()))
	{
	  CRMATRIX (m, cm, result_size, 1);

	  for (int i = 0; i < result_size; i++)
	    {
	      int idx = iv.elem (i);
	      CRMATRIX_ASSIGN_ELEM (m, cm, i, 0, cop_out [idx],
				    c_cop_out [idx], real_type);
	    }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
      else if (nr == 1)
	{
	  CRMATRIX (m, cm, 1, result_size);

	  for (int i = 0; i < result_size; i++)
	    {
	      int idx = iv.elem (i);
	      CRMATRIX_ASSIGN_ELEM (m, cm, 0, i, cop_out [idx],
				    c_cop_out [idx], real_type);
	    }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
      else
	{
	  CRMATRIX (m, cm, index_nr, index_nc);

	  for (int j = 0; j < index_nc; j++)
	    for (int i = 0; i < index_nr; i++)
	      {
		double tmp = *cop_out_index++;
		int idx = tree_to_mat_idx (tmp);
		CRMATRIX_ASSIGN_ELEM (m, cm, i, j, cop_out [idx],
				      c_cop_out [idx], real_type);
	      }

	  ASSIGN_CRMATRIX_TO (retval, m, cm);
	}
    }
  else
    {
      if (index_nr == 0 || index_nc == 0)
	::error ("empty matrix invalid as index");
      else
	::error ("invalid matrix index");
      return tree_constant ();
    }

  return retval;
}

tree_constant
tree_constant_rep::do_vector_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  int len = nr > nc ? nr : nc;

  if (nr == 0 || nc == 0)
    {
      ::error ("attempt to index empty matrix");
      return retval;
    }

  assert ((nr == 1 || nc == 1) && ! user_pref.do_fortran_indexing);

  int swap_indices = (nr == 1);
  
  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
        int i = tree_to_mat_idx (tmp_i.double_value ());
        if (index_check (i, "") < 0)
	  return tree_constant ();
        if (swap_indices)
          {
	    if (range_max_check (i, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (0, i);
          }
        else
          {
	    if (range_max_check (i, nr) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, 0);
          }
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
        Matrix mi = tmp_i.matrix_value ();
	if (mi.rows () == 0 || mi.columns () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    idx_vector iv (mi, user_pref.do_fortran_indexing, "", len);
	    if (! iv)
	      return tree_constant ();

	    if (swap_indices)
	      {
		if (range_max_check (iv.max (), nc) < 0)
		  return tree_constant ();
		retval = do_matrix_index (0, iv);
	      }
	    else
	      {
		if (range_max_check (iv.max (), nr) < 0)
		  return tree_constant ();
		retval = do_matrix_index (iv, 0);
	      }
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
        Range ri = tmp_i.range_value ();
	if (len == 2 && is_zero_one (ri))
	  {
	    if (swap_indices)
	      retval = do_matrix_index (0, 1);
	    else
	      retval = do_matrix_index (1, 0);
	  }
	else if (len == 2 && is_one_zero (ri))
	  {
	    retval = do_matrix_index (0, 0);
	  }
	else
	  {
	    if (index_check (ri, "") < 0)
	      return tree_constant ();
	    if (swap_indices)
	      {
		if (range_max_check (tree_to_mat_idx (ri.max ()), nc) < 0)
		  return tree_constant ();
		retval = do_matrix_index (0, ri);
	      }
	    else
	      {
		if (range_max_check (tree_to_mat_idx (ri.max ()), nr) < 0)
		  return tree_constant ();
		retval = do_matrix_index (ri, 0);
	      }
	  }
      }
      break;
    case magic_colon:
      if (swap_indices)
        retval = do_matrix_index (0, magic_colon);
      else
        retval = do_matrix_index (magic_colon, 0);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const tree_constant& i_arg,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
        int i = tree_to_mat_idx (tmp_i.double_value ());
	if (index_check (i, "row") < 0)
	  return tree_constant ();
	retval = do_matrix_index (i, j_arg);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mi = tmp_i.matrix_value ();
	idx_vector iv (mi, user_pref.do_fortran_indexing, "row", rows ());
	if (! iv)
	  return tree_constant ();

	if (iv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  retval = do_matrix_index (iv, j_arg);
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
	    retval = do_matrix_index (1, j_arg);
	  }
	else if (nr == 2 && is_one_zero (ri))
	  {
	    retval = do_matrix_index (0, j_arg);
	  }
	else
	  {
	    if (index_check (ri, "row") < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, j_arg);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon, j_arg);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (i, j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (i, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (i, jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (i, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (i, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (i, tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (i, rj);
	  }
      }
      break;
    case magic_colon:
      if (range_max_check (i, 0, nr, nc) < 0)
	return tree_constant ();
      retval = do_matrix_index (i, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (iv.max (), j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (iv, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (iv.max (), jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (iv, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (iv, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (iv, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (iv.max (), tree_to_mat_idx (rj.max ()),
				 nr, nc) < 0) 
	      return tree_constant ();
	    retval = do_matrix_index (iv, rj);
	  }
      }
      break;
    case magic_colon:
      if (range_max_check (iv.max (), 0, nr, nc) < 0)
	return tree_constant ();
      retval = do_matrix_index (iv, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (tree_to_mat_idx (ri.max ()), j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (ri, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (tree_to_mat_idx (ri.max ()),
				 jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (ri, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (ri, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (tree_to_mat_idx (ri.max ()),
				 tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ri, rj);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (ri, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  tree_constant_rep::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (index_check (j, "column") < 0)
	  return tree_constant ();
	if (range_max_check (0, j, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (magic_colon, j);
      }
      break;
    case complex_matrix_constant:
    case matrix_constant:
      {
	Matrix mj = tmp_j.matrix_value ();
	idx_vector jv (mj, user_pref.do_fortran_indexing, "column", nc);
	if (! jv)
	  return tree_constant ();

	if (jv.length () == 0)
	  {
	    Matrix mtmp;
	    retval = tree_constant (mtmp);
	  }
	else
	  {
	    if (range_max_check (0, jv.max (), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (magic_colon, jv);
	  }
      }
      break;
    case string_constant:
      gripe_string_invalid ();
      break;
    case range_constant:
      {
	Range rj = tmp_j.range_value ();
	if (nc == 2 && is_zero_one (rj))
	  {
	    retval = do_matrix_index (magic_colon, 1);
	  }
	else if (nc == 2 && is_one_zero (rj))
	  {
	    retval = do_matrix_index (magic_colon, 0);
	  }
	else
	  {
	    if (index_check (rj, "column") < 0)
	      return tree_constant ();
	    if (range_max_check (0, tree_to_mat_idx (rj.max ()), nr, nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (magic_colon, rj);
	  }
      }
      break;
    case magic_colon:
      retval = do_matrix_index (magic_colon, magic_colon);
      break;
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, int j) const
{
  tree_constant retval;

  if (type_tag == matrix_constant)
    retval = tree_constant (matrix->elem (i, j));
  else
    retval = tree_constant (complex_matrix->elem (i, j));

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const idx_vector& jv) const
{
  tree_constant retval;

  int jlen = jv.capacity ();

  CRMATRIX (m, cm, 1, jlen);

  for (int j = 0; j < jlen; j++)
    {
      int col = jv.elem (j);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, col);
    }
  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (int i, const Range& rj) const
{
  tree_constant retval;

  int jlen = rj.nelem ();

  CRMATRIX (m, cm, 1, jlen);

  double b = rj.base ();
  double increment = rj.inc ();
  for (int j = 0; j < jlen; j++)
    {
      double tmp = b + j * increment;
      int col = tree_to_mat_idx (tmp);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, col);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (int i, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();

  CRMATRIX (m, cm, 1, nc);

  for (int j = 0; j < nc; j++)
    {
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, 0, j, i, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv, int j) const
{
  tree_constant retval;

  int ilen = iv.capacity ();

  CRMATRIX (m, cm, ilen, 1);

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, row, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const idx_vector& jv) const
{
  tree_constant retval;

  int ilen = iv.capacity ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, ilen, jlen);

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const idx_vector& iv,
				    const Range& rj) const
{
  tree_constant retval;

  int ilen = iv.capacity ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, ilen, jlen);

  double b = rj.base ();
  double increment = rj.inc ();

  for (int i = 0; i < ilen; i++)
    {
      int row = iv.elem (i);
      for (int j = 0; j < jlen; j++)
	{
	  double tmp = b + j * increment;
	  int col = tree_to_mat_idx (tmp);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (const idx_vector& iv, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();
  int ilen = iv.capacity ();

  CRMATRIX (m, cm, ilen, nc);

  for (int j = 0; j < nc; j++)
    {
      for (int i = 0; i < ilen; i++)
	{
	  int row = iv.elem (i);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, j);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri, int j) const
{
  tree_constant retval;

  int ilen = ri.nelem ();

  CRMATRIX (m, cm, ilen, 1);

  double b = ri.base ();
  double increment = ri.inc ();
  for (int i = 0; i < ilen; i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, row, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri,
				    const idx_vector& jv) const
{
  tree_constant retval;

  int ilen = ri.nelem ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, ilen, jlen);

  double b = ri.base ();
  double increment = ri.inc ();
  for (int i = 0; i < ilen; i++)
    {
      double tmp = b + i * increment;
      int row = tree_to_mat_idx (tmp);
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (const Range& ri, const Range& rj) const
{
  tree_constant retval;

  int ilen = ri.nelem ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, ilen, jlen);

  double ib = ri.base ();
  double iinc = ri.inc ();
  double jb = rj.base ();
  double jinc = rj.inc ();
  
  for (int i = 0; i < ilen; i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < jlen; j++)
	{
	  double jtmp = jb + j * jinc;
	  int col = tree_to_mat_idx (jtmp);

	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index
  (const Range& ri, tree_constant_rep::constant_type mcj) const
{
  assert (mcj == magic_colon);

  tree_constant retval;

  int nc = columns ();

  int ilen = ri.nelem ();

  CRMATRIX (m, cm, ilen, nc);

  double ib = ri.base ();
  double iinc = ri.inc ();

  for (int i = 0; i < ilen; i++)
    {
      double itmp = ib + i * iinc;
      int row = tree_to_mat_idx (itmp);
      for (int j = 0; j < nc; j++)
	{
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, row, j);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    int j) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();

  CRMATRIX (m, cm, nr, 1);

  for (int i = 0; i < nr; i++)
    {
      CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, 0, i, j);
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const idx_vector& jv) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();
  int jlen = jv.capacity ();

  CRMATRIX (m, cm, nr, jlen);

  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < jlen; j++)
	{
	  int col = jv.elem (j);
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, i, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    const Range& rj) const
{
  assert (mci == magic_colon);

  tree_constant retval;

  int nr = rows ();
  int jlen = rj.nelem ();

  CRMATRIX (m, cm, nr, jlen);

  double jb = rj.base ();
  double jinc = rj.inc ();

  for (int j = 0; j < jlen; j++)
    {
      double jtmp = jb + j * jinc;
      int col = tree_to_mat_idx (jtmp);
      for (int i = 0; i < nr; i++)
	{
	  CRMATRIX_ASSIGN_REP_ELEM (m, cm, i, j, i, col);
	}
    }

  ASSIGN_CRMATRIX_TO (retval, m, cm);

  return retval;
}

tree_constant
tree_constant_rep::do_matrix_index (tree_constant_rep::constant_type mci,
				    tree_constant_rep::constant_type mcj) const
{
  assert (mci == magic_colon && mcj == magic_colon);

  return tree_constant (*this);
}

tree_constant
tree_constant_rep::do_matrix_index
  (tree_constant_rep::constant_type mci) const
{
  assert (mci == magic_colon);

  tree_constant retval;
  int nr =  rows ();
  int nc =  columns ();
  int size = nr * nc;
  if (size > 0)
    {
      CRMATRIX (m, cm, size, 1);
      int idx = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  {
	    CRMATRIX_ASSIGN_REP_ELEM (m, cm, idx, 0, i, j);
	    idx++;
	  }
      ASSIGN_CRMATRIX_TO (retval, m, cm);
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/

