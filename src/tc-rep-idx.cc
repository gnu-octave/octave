// tc-rep-idx.cc                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
#include <config.h>
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

// Indexing functions.

// This is the top-level indexing function.

tree_constant
TC_REP::do_index (const Octave_object& args)
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (rows () == 0 || columns () == 0)
    {
      switch (args.length ())
	{
	case 2:
	  if (! args(1).is_magic_colon ()
	      && args(1).rows () != 0 && args(1).columns () != 0)
	    goto index_error;

	case 1:
	  if (! args(0).is_magic_colon ()
	      && args(0).rows () != 0 && args(0).columns () != 0)
	    goto index_error;

	  return Matrix ();

	default:
	index_error:
	  ::error ("attempt to index empty matrix");
	  return retval;
	}
    }

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
      retval = do_scalar_index (args);
      break;

    case complex_matrix_constant:
    case matrix_constant:
      retval = do_matrix_index (args);
      break;

    case string_constant:
      gripe_string_invalid ();
//      retval = do_string_index (args);
      break;

    default:

// This isn\'t great, but it\'s easier than implementing a lot of
// other special indexing functions.

      force_numeric ();

      if (! error_state && is_numeric_type ())
	retval = do_index (args);

      break;
    }

  return retval;
}

tree_constant
TC_REP::do_scalar_index (const Octave_object& args) const
{
  tree_constant retval;

  if (valid_scalar_indices (args))
    {
      if (type_tag == scalar_constant)
	retval = scalar;
      else if (type_tag == complex_scalar_constant)
	retval = *complex_scalar;
      else
	panic_impossible ();

      return retval;
    }
  else
    {
      int rows = -1;
      int cols = -1;

      int nargin = args.length ();

      switch (nargin)
	{
	case 2:
	  {
	    tree_constant arg = args(1);

	    if (arg.is_matrix_type ())
	      {
		Matrix mj = arg.matrix_value ();

		idx_vector j (mj, user_pref.do_fortran_indexing, "", 1);
		if (! j)
		  return retval;

		int jmax = j.max ();
		int len = j.length ();
		if (len == j.ones_count ())
		  cols = len;
		else if (jmax > 0)
		  {
		    error ("invalid scalar index = %d", jmax+1);
		    return retval;
		  }
	      }
	    else if (arg.const_type () == magic_colon)
	      {
		cols = 1;
	      }
	    else if (arg.is_scalar_type ())
	      {
		double dval = arg.double_value ();
		if (! xisnan (dval))
		  {
		    int ival = NINT (dval);
		    if (ival == 1)
		      cols = 1;
		    else if (ival == 0)
		      cols = 0;
		    else
		      break;;
		  }
		else
		  break;
	      }
	    else
	      break;
	  }

// Fall through...

	case 1:
	  {
	    tree_constant arg = args(0);

	    if (arg.is_matrix_type ())
	      {
		Matrix mi = arg.matrix_value ();

		idx_vector i (mi, user_pref.do_fortran_indexing, "", 1);
		if (! i)
		  return retval;

		int imax = i.max ();
		int len = i.length ();
		if (len == i.ones_count ())
		  rows = len;
		else if (imax > 0)
		  {
		    error ("invalid scalar index = %d", imax+1);
		    return retval;
		  }
	      }
	    else if (arg.const_type () == magic_colon)
	      {
		rows = 1;
	      }
	    else if (arg.is_scalar_type ())
	      {
		double dval = arg.double_value ();

		if (! xisnan (dval))
		  {
		    int ival = NINT (dval);
		    if (ival == 1)
		      rows = 1;
		    else if (ival == 0)
		      rows = 0;
		    else
		      break;
		  }
		else
		  break;
	      }
	    else
	      break;

// If only one index, cols will not be set, so we set it.
// If single index is [], rows will be zero, and we should set cols to
// zero too.

	    if (cols < 0)
	      {
		if (rows == 0)
		  cols = 0;
		else
		  {
		    if (user_pref.prefer_column_vectors)
		      cols = 1;
		    else
		      {
			cols = rows;
			rows = 1;
		      }
		  }
	      }

	    if (type_tag == scalar_constant)
	      {
		return Matrix (rows, cols, scalar);
	      }
	    else if (type_tag == complex_scalar_constant)
	      {
		return ComplexMatrix (rows, cols, *complex_scalar);
	      }
	    else
	      panic_impossible ();
	  }
	  break;

	default:
	  ::error ("invalid number of arguments for scalar type");
	  return tree_constant ();
	  break;
	}
    }

  ::error ("index invalid or out of range for scalar type");
  return tree_constant ();
}

tree_constant
TC_REP::do_matrix_index (const Octave_object& args) const
{
  tree_constant retval;

  int nargin = args.length ();

  switch (nargin)
    {
    case 1:
      {
	tree_constant arg = args(0);

	if (arg.is_undefined ())
	  ::error ("matrix index is a null expression");
	else
	  retval = do_matrix_index (arg);
      }
      break;

    case 2:
      {
	tree_constant arg_a = args(0);
	tree_constant arg_b = args(1);

	if (arg_a.is_undefined ())
	::error ("first matrix index is a null expression");
	else if (arg_b.is_undefined ())
	  ::error ("second matrix index is a null expression");
	else
	  retval = do_matrix_index (arg_a, arg_b);
      }
      break;

    default:
      if (nargin == 0)
	::error ("matrix indices expected, but none provided");
      else
	::error ("too many indices for matrix expression");
      break;
    }

  return  retval;
}

tree_constant
TC_REP::do_matrix_index (const tree_constant& i_arg) const
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
TC_REP::do_matrix_index (const tree_constant& i_arg,
			 const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type itype = tmp_i.const_type ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
        int i = tree_to_mat_idx (tmp_i.double_value ());
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
TC_REP::do_matrix_index (TC_REP::constant_type mci) const
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

tree_constant
TC_REP::fortran_style_matrix_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (itype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	double dval = tmp_i.double_value ();

	if (xisnan (dval))
	  {
	    ::error ("NaN is invalid as a matrix index");
	    return tree_constant ();
	  }
	else
	  {
	    int i = NINT (dval);
	    int ii = fortran_row (i, nr) - 1;
	    int jj = fortran_column (i, nr) - 1;
	    if (index_check (i-1, "") < 0)
	      return tree_constant ();
	    if (range_max_check (i-1, nr * nc) < 0)
	      return tree_constant ();
	    retval = do_matrix_index (ii, jj);
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
TC_REP::fortran_style_matrix_index (const Matrix& mi) const
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
      const double *cop_out = 0;
      const Complex *c_cop_out = 0;
      int real_type = type_tag == matrix_constant;
      if (real_type)
	cop_out = matrix->data ();
      else
	c_cop_out = complex_matrix->data ();

      const double *cop_out_index = mi.data ();

      idx_vector iv (mi, 1, "", len);
      if (! iv || range_max_check (iv.max (), len) < 0)
	return retval;

      int result_size = iv.length ();

// XXX FIXME XXX -- there is way too much duplicate code here...

      if (iv.one_zero_only ())
	{
	  if (iv.ones_count () == 0)
	    {
	      retval = Matrix ();
	    }
	  else
	    {
	      if (nr == 1)
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
		  CRMATRIX (m, cm, result_size, 1);

		  for (int i = 0; i < result_size; i++)
		    {
		      int idx = iv.elem (i);
		      CRMATRIX_ASSIGN_ELEM (m, cm, i, 0, cop_out [idx],
					    c_cop_out [idx], real_type);
		    }

		  ASSIGN_CRMATRIX_TO (retval, m, cm);
		}
	    }
	}
      else if (nc == 1)
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
TC_REP::do_vector_index (const tree_constant& i_arg) const
{
  tree_constant retval;

  tree_constant tmp_i = i_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type itype = tmp_i.const_type ();

  int nr = rows ();
  int nc = columns ();

  int len = MAX (nr, nc);

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
TC_REP::do_matrix_index (int i, const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	if (index_check (i, "row") < 0)
	  return tree_constant ();
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
	if (index_check (i, "row") < 0)
	  return tree_constant ();
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
	if (index_check (i, "row") < 0)
	  return tree_constant ();
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
      if (i == -1 && nr == 1)
	return Matrix ();
      if (index_check (i, "row") < 0
	  || range_max_check (i, 0, nr, nc) < 0)
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
TC_REP::do_matrix_index (const idx_vector& iv,
			 const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type jtype = tmp_j.const_type ();

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
TC_REP::do_matrix_index (const Range& ri,
			 const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type jtype = tmp_j.const_type ();

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
      {
	if (index_check (ri, "row") < 0)
	  return tree_constant ();
	if (range_max_check (tree_to_mat_idx (ri.max ()), 0, nr, nc) < 0)
	  return tree_constant ();
	retval = do_matrix_index (ri, magic_colon);
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

tree_constant
TC_REP::do_matrix_index (TC_REP::constant_type mci,
			 const tree_constant& j_arg) const
{
  tree_constant retval;

  tree_constant tmp_j = j_arg.make_numeric_or_range_or_magic ();

  if (error_state)
    return retval;

  TC_REP::constant_type jtype = tmp_j.const_type ();

  int nr = rows ();
  int nc = columns ();

  switch (jtype)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	int j = tree_to_mat_idx (tmp_j.double_value ());
	if (j == -1 && nc == 1)
	  return Matrix ();
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
TC_REP::do_matrix_index (int i, int j) const
{
  tree_constant retval;

  if (type_tag == matrix_constant)
    retval = tree_constant (matrix->elem (i, j));
  else
    retval = tree_constant (complex_matrix->elem (i, j));

  return retval;
}

tree_constant
TC_REP::do_matrix_index (int i, const idx_vector& jv) const
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
TC_REP::do_matrix_index (int i, const Range& rj) const
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
TC_REP::do_matrix_index (int i, TC_REP::constant_type mcj) const
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
TC_REP::do_matrix_index (const idx_vector& iv, int j) const
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
TC_REP::do_matrix_index (const idx_vector& iv, const idx_vector& jv) const
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
TC_REP::do_matrix_index (const idx_vector& iv, const Range& rj) const
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
TC_REP::do_matrix_index (const idx_vector& iv,
			 TC_REP::constant_type mcj) const
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
TC_REP::do_matrix_index (const Range& ri, int j) const
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
TC_REP::do_matrix_index (const Range& ri,
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
TC_REP::do_matrix_index (const Range& ri, const Range& rj) const
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
TC_REP::do_matrix_index (const Range& ri, TC_REP::constant_type mcj) const
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
TC_REP::do_matrix_index (TC_REP::constant_type mci, int j) const
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
TC_REP::do_matrix_index (TC_REP::constant_type mci,
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
TC_REP::do_matrix_index (TC_REP::constant_type mci, const Range& rj) const
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
TC_REP::do_matrix_index (TC_REP::constant_type mci,
			 TC_REP::constant_type mcj) const
{
  tree_constant retval;

  assert (mci == magic_colon && mcj == magic_colon);

  switch (type_tag)
    {
    case complex_scalar_constant:
      retval = *complex_scalar;
      break;

    case scalar_constant:
      retval = scalar;
      break;

    case complex_matrix_constant:
      retval = *complex_matrix;
      break;

    case matrix_constant:
      retval = *matrix;
      break;

    case range_constant:
      retval = *range;
      break;

    case string_constant:
      retval = string;
      break;

    case magic_colon:
    default:
      panic_impossible ();
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
