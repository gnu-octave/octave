// tc-rep.cc                                            -*- C++ -*-
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

#include "mx-base.h"
#include "Range.h"

#include "arith-ops.h"
#include "variables.h"
#include "sysdep.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"
#include "utils.h"
#include "pr-output.h"
#include "tree-const.h"
#include "idx-vector.h"
#include "unwind-prot.h"
#include "oct-map.h"

#include "tc-inlines.h"

// The following three variables could be made static members of the
// TC_REP class.

// Pointer to the blocks of memory we manage.
static TC_REP *newlist = 0;

// Multiplier for allocating new blocks.
static const int newlist_grow_size = 128;

// Indentation level for structures.
static int structure_indent_level = 0;

static void
increment_structure_indent_level (void)
{
  structure_indent_level += 2;
}

static void
decrement_structure_indent_level (void)
{
  structure_indent_level -= 2;
}

static int
any_element_is_complex (const ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (imag (a.elem (i, j)) != 0.0)
	return 1;
  return 0;
}

// The real representation of constants.

TC_REP::tree_constant_rep (void)
{
  type_tag = unknown_constant;
  orig_text = 0;
}

TC_REP::tree_constant_rep (double d)
{
  scalar = d;
  type_tag = scalar_constant;
  orig_text = 0;
}

TC_REP::tree_constant_rep (const Matrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      scalar = m.elem (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (m);
      type_tag = matrix_constant;
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const DiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      scalar = d.elem (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (d);
      type_tag = matrix_constant;
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const RowVector& v, int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const ColumnVector& v, int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v.elem (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const Complex& c)
{
  complex_scalar = new Complex (c);
  type_tag = complex_scalar_constant;
  orig_text = 0;
}

TC_REP::tree_constant_rep (const ComplexMatrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      complex_scalar = new Complex (m.elem (0, 0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      complex_matrix = new ComplexMatrix (m);
      type_tag = complex_matrix_constant;
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const ComplexDiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      complex_scalar = new Complex (d.elem (0, 0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      complex_matrix = new ComplexMatrix (d);
      type_tag = complex_matrix_constant;
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const ComplexRowVector& v,
			   int prefer_column_vector) 
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const ComplexColumnVector& v, int
			   prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      complex_scalar = new Complex (v.elem (0));
      type_tag = complex_scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? user_pref.prefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m.elem (i, 0) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m.elem (0, i) = v.elem (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const char *s)
{
  string = strsave (s);
  type_tag = string_constant;
  orig_text = 0;
}

TC_REP::tree_constant_rep (double b, double l, double i)
{
  range = new Range (b, l, i);
  int nel = range->nelem ();
  if (nel > 1)
    type_tag = range_constant;
  else
    {
      delete range;
      if (nel == 1)
	{
	  scalar = b;
	  type_tag = scalar_constant;
	}
      else if (nel == 0)
	{
	  matrix = new Matrix ();
	  type_tag = matrix_constant;
	}
      else
	{
	  type_tag = unknown_constant;
	  if (nel == -1)
	    ::error ("number of elements in range exceeds INT_MAX");
	  else
	    ::error ("invalid range");
	}
    }
  orig_text = 0;
}

TC_REP::tree_constant_rep (const Range& r)
{
  int nel = r.nelem ();
  if (nel > 1)
    {
      range = new Range (r);
      type_tag = range_constant;
    }
  else if (nel == 1)
    {
      scalar = r.base ();
      type_tag = scalar_constant;
    }
  else if (nel == 0)
    {
      matrix = new Matrix ();
      type_tag = matrix_constant;
    }
  else
    {
      type_tag = unknown_constant;
      if (nel == -1)
	::error ("number of elements in range exceeds INT_MAX");
      else
	::error ("invalid range");
    }

  orig_text = 0;
}

TC_REP::tree_constant_rep (const Octave_map& m)
{
  a_map = new Octave_map (m);
  type_tag = map_constant;
  orig_text = 0;
}

TC_REP::tree_constant_rep (TC_REP::constant_type t)
{
  assert (t == magic_colon || t == all_va_args);
  type_tag = t;
  orig_text = 0;
}

TC_REP::tree_constant_rep (const tree_constant_rep& t)
{
  type_tag = t.type_tag;

  switch (t.type_tag)
    {
    case unknown_constant:
      break;

    case scalar_constant:
      scalar = t.scalar;
      break;

    case matrix_constant:
      matrix = new Matrix (*(t.matrix));
      break;

    case string_constant:
      string = strsave (t.string);
      break;

    case complex_matrix_constant:
      complex_matrix = new ComplexMatrix (*(t.complex_matrix));
      break;

    case complex_scalar_constant:
      complex_scalar = new Complex (*(t.complex_scalar));
      break;

    case range_constant:
      range = new Range (*(t.range));
      break;

    case map_constant:
      a_map = new Octave_map (*(t.a_map));
      break;

    case magic_colon:
    case all_va_args:
      break;
    }

  orig_text = strsave (t.orig_text);
}

TC_REP::~tree_constant_rep (void)
{
  switch (type_tag)
    {
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

    case map_constant:
      delete a_map;
      break;

    case unknown_constant:
    case scalar_constant:
    case magic_colon:
    case all_va_args:
      break;
    }

  delete [] orig_text;
}

void *
TC_REP::operator new (size_t size)
{
  assert (size == sizeof (TC_REP));

  if (! newlist)
    {
      int block_size = newlist_grow_size * sizeof (TC_REP);
      newlist = (TC_REP *) new char [block_size];

      for (int i = 0; i < newlist_grow_size - 1; i++)
	newlist[i].freeptr = &newlist[i+1];

      newlist[i].freeptr = 0;
    }

  TC_REP *tmp = newlist;
  newlist = newlist->freeptr;
  return tmp;
}

void
TC_REP::operator delete (void *p, size_t size)
{
  TC_REP *tmp = (TC_REP *) p;
  tmp->freeptr = newlist;
  newlist = tmp;
}

int
TC_REP::rows (void) const
{
  int retval = -1;

  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
      retval = 1;
      break;

    case string_constant:
    case range_constant:
      retval = (columns () > 0);
      break;

    case matrix_constant:
      retval = matrix->rows ();
      break;

    case complex_matrix_constant:
      retval = complex_matrix->rows ();
      break;

    default:
      break;
    }

  return retval;
}

int
TC_REP::columns (void) const
{
  int retval = -1;

  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
      retval = 1;
      break;

    case matrix_constant:
      retval = matrix->columns ();
      break;

    case complex_matrix_constant:
      retval = complex_matrix->columns ();
      break;

    case string_constant:
      retval = strlen (string);
      break;

    case range_constant:
      retval = range->nelem ();
      break;

    default:
      break;
    }

  return retval;
}

tree_constant
TC_REP::all (void) const
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      tree_constant tmp = make_numeric ();

      if (error_state)
	return retval;

      return tmp.all ();
    }

  switch (type_tag)
    {
    case scalar_constant:
      {
	double status = (scalar != 0.0);
	retval = tree_constant (status);
      }
      break;

    case matrix_constant:
      {
	Matrix m = matrix->all ();
	retval = tree_constant (m);
      }
      break;

    case complex_scalar_constant:
      {
	double status = (*complex_scalar != 0.0);
	retval = tree_constant (status);
      }
      break;

    case complex_matrix_constant:
      {
	Matrix m = complex_matrix->all ();
	retval = tree_constant (m);
      }
      break;

    default:
      gripe_wrong_type_arg ("all", *this);
      break;
    }

  return retval;
}

tree_constant
TC_REP::any (void) const
{
  tree_constant retval;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      tree_constant tmp = make_numeric ();

      if (error_state)
	return retval;

      return tmp.any ();
    }

  switch (type_tag)
    {
    case scalar_constant:
      {
	double status = (scalar != 0.0);
	retval = tree_constant (status);
      }
      break;

    case matrix_constant:
      {
	Matrix m = matrix->any ();
	retval = tree_constant (m);
      }
      break;

    case complex_scalar_constant:
      {
	double status = (*complex_scalar != 0.0);
	retval = tree_constant (status);
      }
      break;

    case complex_matrix_constant:
      {
	Matrix m = complex_matrix->any ();
	retval = tree_constant (m);
      }
      break;

    default:
      gripe_wrong_type_arg ("any", *this);
      break;
    }

  return retval;
}

int
TC_REP::valid_as_scalar_index (void) const
{
  return (type_tag == magic_colon
	  || (type_tag == scalar_constant 
	      && ! xisnan (scalar)
	      && NINT (scalar) == 1)
	  || (type_tag == range_constant
	      && range->nelem () == 1
	      && ! xisnan (range->base ())
	      && NINT (range->base ()) == 1));
}

int
TC_REP::valid_as_zero_index (void) const
{
  return ((type_tag == scalar_constant
	   && ! xisnan (scalar)
	   && NINT (scalar) == 0)
	  || (type_tag == matrix_constant
	      && matrix->rows () == 0
	      && matrix->columns () == 0)
	  || (type_tag == range_constant
	      && range->nelem () == 1
	      && ! xisnan (range->base ())
	      && NINT (range->base ()) == 0));
}

int
TC_REP::is_true (void) const
{
  int retval = 0;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      tree_constant tmp = make_numeric ();

      if (error_state)
	return retval;

      return tmp.is_true ();
    }

  switch (type_tag)
    {
    case scalar_constant:
      retval = (scalar != 0.0);
      break;

    case matrix_constant:
      {
	Matrix m = (matrix->all ()) . all ();
	retval = (m.rows () == 1
		  && m.columns () == 1
		  && m.elem (0, 0) != 0.0);
      }
      break;

    case complex_scalar_constant:
      retval = (*complex_scalar != 0.0);
      break;

    case complex_matrix_constant:
      {
	Matrix m = (complex_matrix->all ()) . all ();
	retval = (m.rows () == 1
		  && m.columns () == 1
		  && m.elem (0, 0) != 0.0);
      }
      break;

    default:
      gripe_wrong_type_arg (0, *this);
      break;
    }

  return retval;
}

static void
warn_implicit_conversion (const char *from, const char *to)
{
  warning ("implicit conversion from %s to %s", from, to);
}

double
TC_REP::double_value (int force_string_conversion) const
{
  double retval = octave_NaN;

  switch (type_tag)
    {
    case scalar_constant:
      retval = scalar;
      break;

    case matrix_constant:
      {
	if (user_pref.do_fortran_indexing && rows () > 0 && columns () > 0)
	  retval = matrix->elem (0, 0);
	else
	  gripe_invalid_conversion ("real matrix", "real scalar");
      }
      break;

    case complex_matrix_constant:
    case complex_scalar_constant:
      {
	int flag = user_pref.ok_to_lose_imaginary_part;

	if (flag < 0)
	  warn_implicit_conversion ("complex scalar", "real scalar");

	if (flag)
	  {
	    if (type_tag == complex_scalar_constant)
	      retval = ::real (*complex_scalar);
	    else if (type_tag == complex_matrix_constant)
	      {
		if (user_pref.do_fortran_indexing
		    && rows () > 0 && columns () > 0)
		  retval = ::real (complex_matrix->elem (0, 0));
		else
		  gripe_invalid_conversion ("complex matrix", "real scalar");
	      }
	    else
	      panic_impossible ();
	  }
	else
	  gripe_invalid_conversion ("complex scalar", "real scalar");
      }
      break;

    case string_constant:
      {
	int flag = force_string_conversion;
	if (! flag)
	  flag = user_pref.implicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "real scalar");

	int len = strlen (string);
	if (flag && (len == 1 || (len > 1 && user_pref.do_fortran_indexing)))
	  retval = toascii ((int) string[0]);
	else
	  gripe_invalid_conversion ("string", "real scalar");
      }
      break;

    case range_constant:
      {
	int nel = range->nelem ();
	if (nel == 1 || (nel > 1 && user_pref.do_fortran_indexing))
	  retval = range->base ();
	else
	  gripe_invalid_conversion ("range", "real scalar");
      }
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "real scalar");
      break;
    }

  return retval;
}

Matrix
TC_REP::matrix_value (int force_string_conversion) const
{
  Matrix retval;

  switch (type_tag)
    {
    case scalar_constant:
      retval = Matrix (1, 1, scalar);
      break;

    case matrix_constant:
      retval = *matrix;
      break;

    case complex_scalar_constant:
    case complex_matrix_constant:
      {
	int flag = user_pref.ok_to_lose_imaginary_part;
	if (flag < 0)
	  warn_implicit_conversion ("complex matrix", "real matrix");

	if (flag)
	  {
	    if (type_tag == complex_scalar_constant)
	      retval = Matrix (1, 1, ::real (*complex_scalar));
	    else if (type_tag == complex_matrix_constant)
	      retval = ::real (*complex_matrix);
	    else
	      panic_impossible ();
	  }
	else
	  gripe_invalid_conversion ("complex matrix", "real matrix");
      }
      break;

    case string_constant:
      {
	int flag = force_string_conversion;
	if (! flag)
	  flag = user_pref.implicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "real matrix");

	if (flag)
	  {
	    int len = strlen (string);

	    if (len > 0)
	      {
		retval.resize (1, len);

		for (int i = 0; i < len; i++)
		  retval.elem (0, i) = toascii ((int) string[i]);
	      }
	    else
	      retval = Matrix ();
	  }
	else
	  gripe_invalid_conversion ("string", "real matrix");
      }
      break;

    case range_constant:
      retval = range->matrix_value ();
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "real matrix");
      break;
    }

  return retval;
}

Complex
TC_REP::complex_value (int force_string_conversion) const
{
  Complex retval (octave_NaN, octave_NaN);

  switch (type_tag)
    {
    case complex_scalar_constant:
      retval = *complex_scalar;
      break;

    case scalar_constant:
      retval = scalar;
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	if (user_pref.do_fortran_indexing && rows () > 0 && columns () > 0)
	  {
	    if (type_tag == complex_matrix_constant)
	      retval = complex_matrix->elem (0, 0);
	    else
	      retval = matrix->elem (0, 0);
	  }
	else
	  gripe_invalid_conversion ("real matrix", "real scalar");
      }
      break;

    case string_constant:
      {
	int flag = force_string_conversion;
	if (! flag)
	  flag = user_pref.implicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "complex scalar");

	int len = strlen (string);
	if (flag && (len == 1 || (len > 1 && user_pref.do_fortran_indexing)))
	  retval = toascii ((int) string[0]);
	else
	  gripe_invalid_conversion ("string", "complex scalar");
      }
      break;

    case range_constant:
      {
	int nel = range->nelem ();
	if (nel == 1 || (nel > 1 && user_pref.do_fortran_indexing))
	  retval = range->base ();
	else
	  gripe_invalid_conversion ("range", "complex scalar");
      }
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "complex scalar");
      break;
    }

  return retval;
}

ComplexMatrix
TC_REP::complex_matrix_value (int force_string_conversion) const
{
  ComplexMatrix retval;

  switch (type_tag)
    {
    case scalar_constant:
      retval = ComplexMatrix (1, 1, Complex (scalar));
      break;

    case complex_scalar_constant:
      retval = ComplexMatrix (1, 1, *complex_scalar);
      break;

    case matrix_constant:
      retval = ComplexMatrix (*matrix);
      break;

    case complex_matrix_constant:
      retval = *complex_matrix;
      break;

    case string_constant:
      {
	int flag = force_string_conversion;
	if (! flag)
	  flag = user_pref.implicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "complex matrix");

	if (flag)
	  {
	    int len = strlen (string);

	    retval.resize (1, len);

	    if (len > 1)
	      {
		for (int i = 0; i < len; i++)
		  retval.elem (0, i) = toascii ((int) string[i]);
	      }
	    else if (len == 1)
	      retval.elem (0, 0) = toascii ((int) string[0]);
	    else
	      panic_impossible ();
	  }
	else
	  gripe_invalid_conversion ("string", "real matrix");
      }
      break;

    case range_constant:
      retval = range->matrix_value ();
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "complex matrix");
      break;
    }

  return retval;
}

char *
TC_REP::string_value (void) const
{
  if (type_tag == string_constant)
    return string;
  else
    {
      gripe_invalid_conversion (type_as_string (), "string");
      return 0;
    }
}

Range
TC_REP::range_value (void) const
{
  assert (type_tag == range_constant);
  return *range;
}

Octave_map
TC_REP::map_value (void) const
{
  assert (type_tag == map_constant);
  return *a_map;
}

tree_constant&
TC_REP::lookup_map_element (const char *name, int insert)
{
  static tree_constant retval;

  if (type_tag == map_constant)
    {
      Pix idx = a_map->seek (name);

      if (idx)
	return a_map->contents (idx);
      else if (insert)
	return (*a_map) [name];
      else
	error ("structure has no member `%s'", name);
    }
  else
    error ("invalid structure access attempted");

  return retval;
}

// This could be made more efficient by doing all the work here rather
// than relying on matrix_value() to do any possible type conversions.

ColumnVector
TC_REP::vector_value (int force_string_conversion,
		      int force_vector_conversion) const
{
  ColumnVector retval;

  Matrix m = matrix_value (force_string_conversion);

  if (error_state)
    return retval;

  int nr = m.rows ();
  int nc = m.columns ();
  if (nr == 1)
    {
      retval.resize (nc);
      for (int i = 0; i < nc; i++)
	retval.elem (i) = m (0, i);
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (int i = 0; i < nr; i++)
	retval.elem (i) = m.elem (i, 0);
    }
  else if (nr > 0 && nc > 0
	   && (user_pref.do_fortran_indexing || force_vector_conversion))
    {
      retval.resize (nr * nc);
      int k = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval.elem (k++) = m.elem (i, j);
    }
  else
    gripe_invalid_conversion ("real matrix", "real vector");

  return retval;
}

// This could be made more efficient by doing all the work here rather
// than relying on complex_matrix_value() to do any possible type
// conversions.

ComplexColumnVector
TC_REP::complex_vector_value (int force_string_conversion,
			      int force_vector_conversion) const
{
  ComplexColumnVector retval;

  ComplexMatrix m = complex_matrix_value (force_string_conversion);

  if (error_state)
    return retval;

  int nr = m.rows ();
  int nc = m.columns ();
  if (nr == 1)
    {
      retval.resize (nc);
      for (int i = 0; i < nc; i++)
	retval.elem (i) = m (0, i);
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (int i = 0; i < nr; i++)
	retval.elem (i) = m.elem (i, 0);
    }
  else if (nr > 0 && nc > 0
	   && (user_pref.do_fortran_indexing || force_vector_conversion))
    {
      retval.resize (nr * nc);
      int k = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval.elem (k++) = m.elem (i, j);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex vector");

  return retval;
}

tree_constant
TC_REP::convert_to_str (void) const
{
  tree_constant retval;

  switch (type_tag)
    {
    case complex_scalar_constant:
    case scalar_constant:
      {
	double d = double_value ();

	if (xisnan (d))
	  {
	    ::error ("invalid conversion from NaN to character");
	    return retval;
	  }
	else
	  {
	    int i = NINT (d);
// Warn about out of range conversions?
	    char s[2];
	    s[0] = (char) i;
	    s[1] = '\0';
	    retval = tree_constant (s);
	  }
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	if (rows () == 0 && columns () == 0)
	  {
	    char s = '\0';
	    retval = tree_constant (&s);
	  }
	else
	  {
	    ColumnVector v = vector_value ();
	    int len = v.length ();
	    if (len == 0)
	      {
		char s = '\0';
		retval = tree_constant (&s);
	      }
	    else
	      {
		char *s = new char [len+1];
		s[len] = '\0';
		for (int i = 0; i < len; i++)
		  {
		    double d = v.elem (i);

		    if (xisnan (d))
		      {
			::error ("invalid conversion from NaN to character");
			delete [] s;
			return retval;
		      }
		    else
		      {
			int ival = NINT (d);
// Warn about out of range conversions?
			s[i] = (char) ival;
		      }
		  }
		retval = tree_constant (s);
		delete [] s;
	      }
	  }
      }
      break;

    case range_constant:
      {
	Range r = range_value ();
	double b = r.base ();
	double incr = r.inc ();
	int nel = r.nelem ();
	char *s = new char [nel+1];
	s[nel] = '\0';
	for (int i = 0; i < nel; i++)
	  {
	    double d = b + i * incr;

	    if (xisnan (d))
	      {
		::error ("invalid conversion from NaN to character");
		delete [] s;
		return retval;
	      }
	    else
	      {
		int ival = NINT (d);
// Warn about out of range conversions?
		s[i] = (char) ival;
	      }
	  }
	retval = tree_constant (s);
	delete [] s;
      }
      break;

    case string_constant:
      retval = string;
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "string");
      break;
    }

  return retval;
}

void
TC_REP::convert_to_row_or_column_vector (void)
{
  assert (type_tag == matrix_constant || type_tag == complex_matrix_constant);

  int nr = rows ();
  int nc = columns ();

  if (nr == 1 || nc == 1)
    return;

  int len = nr * nc;

  assert (len > 0);

  int new_nr = 1;
  int new_nc = 1;

  if (user_pref.prefer_column_vectors)
    new_nr = len;
  else
    new_nc = len;

  if (type_tag == matrix_constant)
    {
      Matrix *m = new Matrix (new_nr, new_nc);

      double *cop_out = matrix->fortran_vec ();

      for (int i = 0; i < len; i++)
	{
	  if (new_nr == 1)
	    m->elem (0, i) = *cop_out++;
	  else
	    m->elem (i, 0) = *cop_out++;
	}

      delete matrix;
      matrix = m;
    }
  else
    {
      ComplexMatrix *cm = new ComplexMatrix (new_nr, new_nc);

      Complex *cop_out = complex_matrix->fortran_vec ();

      for (int i = 0; i < len; i++)
	{
	  if (new_nr == 1)
	    cm->elem (0, i) = *cop_out++;
	  else
	    cm->elem (i, 0) = *cop_out++;
	}

      delete complex_matrix;
      complex_matrix = cm;
    }
}

void
TC_REP::force_numeric (int force_str_conv)
{
  switch (type_tag)
    {
    case scalar_constant:
    case matrix_constant:
    case complex_scalar_constant:
    case complex_matrix_constant:
      break;

    case string_constant:
      {
	if (! force_str_conv && ! user_pref.implicit_str_to_num_ok)
	  {
	    ::error ("failed to convert `%s' to a numeric type --", string);
	    ::error ("default conversion turned off");

	    return;
	  }

	int len = strlen (string);
	if (len > 1)
	  {
	    type_tag = matrix_constant;
	    Matrix *tm = new Matrix (1, len);
	    for (int i = 0; i < len; i++)
	      tm->elem (0, i) = toascii ((int) string[i]);
	    matrix = tm;
	  }
	else if (len == 1)
	  {
	    type_tag = scalar_constant;
	    scalar = toascii ((int) string[0]);
	  }
	else if (len == 0)
	  {
	    type_tag = matrix_constant;
	    matrix = new Matrix (0, 0);
	  }
	else
	  panic_impossible ();
      }
      break;

    case range_constant:
      {
	int len = range->nelem ();
	if (len > 1)
	  {
	    type_tag = matrix_constant;
	    Matrix *tm = new Matrix (1, len);
	    double b = range->base ();
	    double increment = range->inc ();
	    for (int i = 0; i < len; i++)
	      tm->elem (0, i) = b + i * increment;
	    matrix = tm;
	  }
	else if (len == 1)
	  {
	    type_tag = scalar_constant;
	    scalar = range->base ();
	  }
      }
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "numeric type");
      break;
    }
}

tree_constant
TC_REP::make_numeric (int force_str_conv) const
{
  tree_constant retval;

  switch (type_tag)
    {
    case scalar_constant:
      retval = tree_constant (scalar);
      break;

    case matrix_constant:
      retval = tree_constant (*matrix);
      break;

    case complex_scalar_constant:
      retval = tree_constant (*complex_scalar);
      break;

    case complex_matrix_constant:
      retval = tree_constant (*complex_matrix);
      break;

    case string_constant:
      retval = tree_constant (string);
      retval.force_numeric (force_str_conv);
      break;

    case range_constant:
      retval = tree_constant (*range);
      retval.force_numeric (force_str_conv);
      break;

    default:
      gripe_invalid_conversion (type_as_string (), "numeric value");
      break;
    }

  return retval;
}

void
TC_REP::bump_value (tree_expression::type etype)
{
  switch (etype)
    {
    case tree_expression::increment:
      switch (type_tag)
	{
	case scalar_constant:
	  scalar++;
	  break;

	case matrix_constant:
	  *matrix = *matrix + 1.0;
	  break;

	case complex_scalar_constant:
	  *complex_scalar = *complex_scalar + 1.0;
	  break;

	case complex_matrix_constant:
	  *complex_matrix = *complex_matrix + 1.0;
	  break;

	case range_constant:
	  range->set_base (range->base () + 1.0);
	  range->set_limit (range->limit () + 1.0);
	  break;

	default:
	  gripe_wrong_type_arg ("operator ++", type_as_string ());
	  break;
	}
      break;

    case tree_expression::decrement:
      switch (type_tag)
	{
	case scalar_constant:
	  scalar--;
	  break;

	case matrix_constant:
	  *matrix = *matrix - 1.0;
	  break;

	case range_constant:
	  range->set_base (range->base () - 1.0);
	  range->set_limit (range->limit () - 1.0);
	  break;

	default:
	  gripe_wrong_type_arg ("operator --", type_as_string ());
	  break;
	}
      break;

    default:
      panic_impossible ();
      break;
    }
}

void
TC_REP::resize (int i, int j)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->resize (i, j);
      break;

    case complex_matrix_constant:
      complex_matrix->resize (i, j);
      break;

    default:
      gripe_wrong_type_arg ("resize", type_as_string ());
      break;
    }
}

void
TC_REP::resize (int i, int j, double val)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->resize (i, j, val);
      break;

    case complex_matrix_constant:
      complex_matrix->resize (i, j, val);
      break;

    default:
      gripe_wrong_type_arg ("resize", type_as_string ());
      break;
    }
}

void
TC_REP::maybe_resize (int i, int j)
{
  int nr = rows ();
  int nc = columns ();

  i++;
  j++;

  assert (i > 0 && j > 0);

  if (i > nr || j > nc)
    {
      if (user_pref.resize_on_range_error)
	resize (MAX (i, nr), MAX (j, nc), 0.0);
      else
	{
	  if (i > nr)
	    ::error ("row index = %d exceeds max row dimension = %d", i, nr);

	  if (j > nc)
	    ::error ("column index = %d exceeds max column dimension = %d",
		     j, nc);
	}
    }
}

void
TC_REP::maybe_resize (int i, force_orient f_orient)
{
  int nr = rows ();
  int nc = columns ();

  i++;

  assert (i >= 0 && (nr <= 1 || nc <= 1));

// This function never reduces the size of a vector, and all vectors
// have dimensions of at least 0x0.  If i is 0, it is either because
// a vector has been indexed with a vector of all zeros (in which case
// the index vector is empty and nothing will happen) or a vector has
// been indexed with 0 (an error which will be caught elsewhere).
  if (i == 0)
    return;

  if (nr <= 1 && nc <= 1 && i >= 1)
    {
      if (user_pref.resize_on_range_error)
	{
	  if (f_orient == row_orient)
	    resize (1, i, 0.0);
	  else if (f_orient == column_orient)
	    resize (i, 1, 0.0);
	  else if (user_pref.prefer_column_vectors)
	    resize (i, 1, 0.0);
	  else
	    resize (1, i, 0.0);
	}
      else
	::error ("matrix index = %d exceeds max dimension = %d", i, nc);
    }
  else if (nr == 1 && i > nc)
    {
      if (user_pref.resize_on_range_error)
	resize (1, i, 0.0);
      else
	::error ("matrix index = %d exceeds max dimension = %d", i, nc);
    }
  else if (nc == 1 && i > nr)
    {
      if (user_pref.resize_on_range_error)
	resize (i, 1, 0.0);
      else
	::error ("matrix index = %d exceeds max dimension = ", i, nc);
    }
}

void
TC_REP::stash_original_text (char *s)
{
  orig_text = strsave (s);
}

void
TC_REP::maybe_mutate (void)
{
  if (error_state)
    return;

  switch (type_tag)
    {
    case complex_scalar_constant:
      if (::imag (*complex_scalar) == 0.0)
	{
	  double d = ::real (*complex_scalar);
	  delete complex_scalar;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;

    case complex_matrix_constant:
      if (! any_element_is_complex (*complex_matrix))
	{
	  Matrix *m = new Matrix (::real (*complex_matrix));
	  delete complex_matrix;
	  matrix = m;
	  type_tag = matrix_constant;
	}
      break;

    default:
      break;
    }

// Avoid calling rows() and columns() for things like magic_colon.

  int nr = 1;
  int nc = 1;
  if (type_tag == matrix_constant
      || type_tag == complex_matrix_constant
      || type_tag == range_constant)
    {
      nr = rows ();
      nc = columns ();
    }

  switch (type_tag)
    {
    case matrix_constant:
      if (nr == 1 && nc == 1)
	{
	  double d = matrix->elem (0, 0);
	  delete matrix;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;

    case complex_matrix_constant:
      if (nr == 1 && nc == 1)
	{
	  Complex c = complex_matrix->elem (0, 0);
	  delete complex_matrix;
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
      break;

    case range_constant:
      if (nr == 1 && nc == 1)
	{
	  double d = range->base ();
	  delete range;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;

    default:
      break;
    }
}

void
TC_REP::print (ostream& output_buf)
{
  if (error_state)
    return;

  switch (type_tag)
    {
    case scalar_constant:
      octave_print_internal (output_buf, scalar);
      break;

    case matrix_constant:
      octave_print_internal (output_buf, *matrix);
      break;

    case complex_scalar_constant:
      octave_print_internal (output_buf, *complex_scalar);
      break;

    case complex_matrix_constant:
      octave_print_internal (output_buf, *complex_matrix);
      break;

    case string_constant:
      output_buf << string << "\n";
      break;

    case range_constant:
      octave_print_internal (output_buf, *range);
      break;

    case map_constant:
      {
// XXX FIXME XXX -- would be nice to print the output in some standard
// order.  Maybe all substructures first, maybe alphabetize entries,
// etc.
	begin_unwind_frame ("TC_REP_print");

	unwind_protect_int (structure_indent_level);
	unwind_protect_int (user_pref.struct_levels_to_print);

	if (user_pref.struct_levels_to_print > 0)
	  {
	    user_pref.struct_levels_to_print--;

	    increment_structure_indent_level ();

	    for (Pix p = a_map->first (); p != 0; a_map->next (p))
	      {
		const char *key = a_map->key (p);
		tree_constant val = a_map->contents (p);

		output_buf.form ("%*s%s = ", structure_indent_level,
				 "", key);

		if (print_as_structure (val))
		  output_buf << "{\n";
		else if (! print_as_scalar (val))
		  output_buf << "\n";

		val.print (output_buf);
	      }

	    decrement_structure_indent_level ();

	    output_buf.form ("%*s%s", structure_indent_level, "", "}\n");
	  }
	else
	  output_buf << "<structure>\n";

	run_unwind_frame ("TC_REP_print");
      }
      break;

    case unknown_constant:
    case magic_colon:
    case all_va_args:
      panic_impossible ();
      break;
    }
}

void
TC_REP::print_code (ostream& os)
{
  switch (type_tag)
    {
    case scalar_constant:
      if (orig_text)
	os << orig_text;
      else
	octave_print_internal (os, scalar, 1);
      break;

    case matrix_constant:
      octave_print_internal (os, *matrix, 1);
      break;

    case complex_scalar_constant:
     {
	double re = complex_scalar->real ();
	double im = complex_scalar->imag ();

// If we have the original text and a pure imaginary, just print the
// original text, because this must be a constant that was parsed as
// part of a function.

	if (orig_text && re == 0.0 && im > 0.0)
	  os << orig_text;
	else
	  octave_print_internal (os, *complex_scalar, 1);
      }
      break;

    case complex_matrix_constant:
      octave_print_internal (os, *complex_matrix, 1);
      break;

    case string_constant:
      {
	os << "\"";
	char *s, *t = string;
	while (s = undo_string_escape (*t++))
	  os << s;
	os << "\"";
      }
      break;

    case range_constant:
      octave_print_internal (os, *range, 1);
      break;

    case magic_colon:
      os << ":";
      break;

    case all_va_args:
      os << "all_va_args";
      break;

    case map_constant:
    case unknown_constant:
      panic_impossible ();
      break;
    }
}

void
TC_REP::gripe_wrong_type_arg (const char *name,
			      const tree_constant_rep& tcr) const
{
  if (name)
    ::error ("%s: wrong type argument `%s'", name, tcr.type_as_string ());
  else
    ::error ("wrong type argument `%s'", name, tcr.type_as_string ());
}

char *
TC_REP::type_as_string (void) const
{
  switch (type_tag)
    {
    case scalar_constant:
      return "real scalar";

    case matrix_constant:
      return "real matrix";

    case complex_scalar_constant:
      return "complex scalar";

    case complex_matrix_constant:
      return "complex matrix";

    case string_constant:
      return "string";

    case range_constant:
      return "range";

    case map_constant:
      return "structure";

    default:
      return "<unknown type>";
    }
}

tree_constant
do_binary_op (tree_constant& a, tree_constant& b, tree_expression::type t)
{
  tree_constant retval;

  int first_empty = (a.rows () == 0 || a.columns () == 0);
  int second_empty = (b.rows () == 0 || b.columns () == 0);

  if (first_empty || second_empty)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("binary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid binary operation on empty matrix");
	  return retval;
	}
    }

  tree_constant tmp_a = a.make_numeric ();

  if (error_state)
    return retval;

  tree_constant tmp_b = b.make_numeric ();

  if (error_state)
    return retval;

  TC_REP::constant_type a_type = tmp_a.const_type ();
  TC_REP::constant_type b_type = tmp_b.const_type ();

  double d1, d2;
  Matrix m1, m2;
  Complex c1, c2;
  ComplexMatrix cm1, cm2;

  switch (a_type)
    {
    case TC_REP::scalar_constant:

      d1 = tmp_a.double_value ();

      switch (b_type)
	{
	case TC_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (d1, d2, t);
	  break;

	case TC_REP::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (d1, m2, t);
	  break;

	case TC_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (d1, c2, t);
	  break;

	case TC_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (d1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case TC_REP::matrix_constant:

      m1 = tmp_a.matrix_value ();

      switch (b_type)
	{
	case TC_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (m1, d2, t);
	  break;

	case TC_REP::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (m1, m2, t);
	  break;

	case TC_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (m1, c2, t);
	  break;

	case TC_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (m1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case TC_REP::complex_scalar_constant:

      c1 = tmp_a.complex_value ();

      switch (b_type)
	{
	case TC_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (c1, d2, t);
	  break;

	case TC_REP::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (c1, m2, t);
	  break;

	case TC_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (c1, c2, t);
	  break;

	case TC_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (c1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case TC_REP::complex_matrix_constant:

      cm1 = tmp_a.complex_matrix_value ();

      switch (b_type)
	{
	case TC_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (cm1, d2, t);
	  break;

	case TC_REP::matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (cm1, m2, t);
	  break;

	case TC_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (cm1, c2, t);
	  break;

	case TC_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (cm1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    default:
      gripe_wrong_type_arg_for_binary_op (tmp_a);
      break;
    }

  return retval;
}

tree_constant
do_unary_op (tree_constant& a, tree_expression::type t)
{
  tree_constant retval;

  if (a.rows () == 0 || a.columns () == 0)
    {
      int flag = user_pref.propagate_empty_matrices;
      if (flag < 0)
	warning ("unary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid unary operation on empty matrix");
	  return retval;
	}
    }

  tree_constant tmp_a = a.make_numeric ();

  if (error_state)
    return retval;

  switch (tmp_a.const_type ())
    {
    case TC_REP::scalar_constant:
      retval = do_unary_op (tmp_a.double_value (), t);
      break;

    case TC_REP::matrix_constant:
      {
	Matrix m = tmp_a.matrix_value ();
	retval = do_unary_op (m, t);
      }
      break;

    case TC_REP::complex_scalar_constant:
      retval = do_unary_op (tmp_a.complex_value (), t);
      break;

    case TC_REP::complex_matrix_constant:
      {
	ComplexMatrix m = tmp_a.complex_matrix_value ();
	retval = do_unary_op (m, t);
      }
      break;

    default:
      gripe_wrong_type_arg_for_unary_op (tmp_a);
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
