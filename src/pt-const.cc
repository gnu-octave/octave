/*

Copyright (C) 1996 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cstring>

#include <string>

#include <fstream.h>
#include <iostream.h>

#include <SLList.h>

#include "Array-flags.h"

#include "mx-base.h"
#include "Range.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "idx-vector.h"
#include "mappers.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"
#include "pr-output.h"
#include "sysdep.h"
#include "pt-const.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

Octave_map
tree_constant::map_value (void) const
{
  return val.map_value ();
}

void
tree_constant::print (void)
{
}

#if 0
octave_value
tree_constant::assign_map_element (SLList<string>&, const octave_value&)
{
  octave_value retval;
  error ("tree_constant::assign_map_element(): not implemented");
  return retval;
}

octave_value
tree_constant::assign_map_element (SLList<string>&, const octave_value_list&,
				   const octave_value&)
{
  octave_value retval;
  error ("tree_constant::assign_map_element(): not implemented");
  return retval;
}
#endif

octave_value
tree_constant::eval (bool print_result)
{
  if (print_result)
    val.print ();

  return val;
}

octave_value_list
tree_constant::eval (bool, int, const octave_value_list& idx)
{
  octave_value_list retval;

  if (idx.length () >  0)
    retval (0) = index (idx);
  else
    retval (0) = val;

  return retval;
}

octave_value
tree_constant::lookup_map_element (const string&, bool, bool)
{
  octave_value retval;
  error ("tree_constant::lookup_map_element() not implemented");
  return retval;
}

octave_value
tree_constant::lookup_map_element (SLList<string>&, bool, bool)
{
  octave_value retval;
  error ("tree_constant::lookup_map_element() not implemented");
  return retval;
}

void
tree_constant::accept (tree_walker& tw)
{
  tw.visit_constant (*this);
}

#if 0

#ifndef OCT_VAL_REP
#define OCT_VAL_REP octave_value::octave_value_rep
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef OCT_VAL_REP
#define OCT_VAL_REP octave_value::octave_value_rep
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

// The following three variables could be made static members of the
// OCT_VAL_REP class.

// Pointer to the blocks of memory we manage.
static OCT_VAL_REP *tc_rep_newlist = 0;

// Multiplier for allocating new blocks.
static const int tc_rep_newlist_grow_size = 128;

// XXX FIXME XXX -- these should be member functions.

static bool
any_element_is_complex (const ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (imag (a (i, j)) != 0.0)
	return true;

  return false;
}

static bool
any_element_is_complex (const ComplexDiagMatrix& d)
{
  int len = d.length ();

  for (int i = 0; i < len; i++)
    if (imag (d (i, i)) != 0.0)
      return true;

  return false;
}

static bool
any_element_is_complex (const ComplexRowVector& v)
{
  int len = v.length ();

  for (int i = 0; i < len; i++)
    if (imag (v (i)) != 0.0)
      return true;

  return false;
}

static bool
any_element_is_complex (const ComplexColumnVector& v)
{
  int len = v.length ();

  for (int i = 0; i < len; i++)
    if (imag (v (i)) != 0.0)
      return true;

  return false;
}

// The following three variables could be made static members of the
// octave_value class.

// Pointer to the blocks of memory we manage.
static octave_value *tc_newlist = 0;

// Multiplier for allocating new blocks.
static const int tc_newlist_grow_size = 128;

Octave_map
octave_value::map_value (void) const
{
  return rep->map_value ();
}

octave_value::~octave_value (void)
{
#if defined (MDEBUG)
  cerr << "~octave_value: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (--rep->count <= 0)
    {
      delete rep;
      rep = 0;
    }
}

void *
octave_value::operator new (size_t size)
{
  assert (size == sizeof (octave_value));

  if (! tc_newlist)
    {
      int block_size = tc_newlist_grow_size * sizeof (octave_value);
      tc_newlist = (octave_value *) new char [block_size];

      int i = 0;

      for (i = 0; i < tc_newlist_grow_size - 1; i++)
	tc_newlist[i].freeptr = &tc_newlist[i+1];

      tc_newlist[i].freeptr = 0;
    }

  octave_value *tmp = tc_newlist;
  tc_newlist = tc_newlist->freeptr;
  return tmp;
}

void
octave_value::operator delete (void *p, size_t /* size */)
{
  octave_value *tmp = (octave_value *) p;
  tmp->freeptr = tc_newlist;
  tc_newlist = tmp;
}

// Simple assignment.

octave_value
octave_value::operator = (const octave_value& a)
{
  if (rep != a.rep)
    {
      if (--rep->count <= 0)
	delete rep;
      rep = a.rep;
      rep->count++;
    }
  return *this;  
}

// Simple structure assignment.

void
octave_value::make_unique (void)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new octave_value_rep (*rep);
      rep->count = 1;
    }

  if (rep->is_map ())
    {
      for (Pix p = rep->a_map->first (); p != 0; rep->a_map->next (p))
	{
	  rep->a_map->contents (p) . make_unique ();
	}
    }
}

octave_value::octave_value_rep *
octave_value::make_unique_map (void)
{
  if (! rep->is_map ())
    {
      if (--rep->count <= 0)
	delete rep;

      Octave_map m;
      rep = new octave_value_rep (m);
      rep->count = 1;
    }

  make_unique ();

  return rep;
}

octave_value
octave_value::assign_map_element (SLList<string>& list,
				   octave_value& rhs)
{
  octave_value_rep *tmp_rep = make_unique_map ();

  if (rhs.is_map ())
    rhs.make_unique ();

  Pix p = list.first ();
  while (p)
    {
      string elt = list (p);

      list.next (p);

      octave_value& tmp = tmp_rep->lookup_map_element (elt, 1);

      if (! p)
	{
	  tmp = rhs;
	  return tmp;
	}

      tmp_rep = tmp.make_unique_map ();
    }

  return octave_value ();
}

// Indexed structure assignment.

octave_value
octave_value::assign_map_element (SLList<string>& list,
				   octave_value& rhs,
				   const octave_value_list& args)
{
  octave_value_rep *tmp_rep = make_unique_map ();

  if (rhs.is_map ())
    rhs.make_unique ();

  Pix p = list.first ();
  while (p)
    {
      string elt = list (p);

      list.next (p);

      octave_value& tmp = tmp_rep->lookup_map_element (elt, 1);

      if (! p)
	{
	  tmp.assign (rhs, args);
	  return tmp;
	}

      tmp_rep = tmp.make_unique_map ();
    }

  return octave_value ();
}

octave_value_list
octave_value::eval (bool print, int, const octave_value_list& args)
{
  octave_value_list retval;

  if (args.length () > 0)
    retval(0) = rep->do_index (args);
  else
    retval(0) = *this;

  if (retval(0).is_defined ())
    retval(0).eval (print);

  return retval;
}

void *
OCT_VAL_REP::operator new (size_t size)
{
  assert (size == sizeof (OCT_VAL_REP));

  if (! tc_rep_newlist)
    {
      int block_size = tc_rep_newlist_grow_size * sizeof (OCT_VAL_REP);
      tc_rep_newlist = (OCT_VAL_REP *) new char [block_size];

      int i = 0;

      for (i = 0; i < tc_rep_newlist_grow_size - 1; i++)
	tc_rep_newlist[i].freeptr = &tc_rep_newlist[i+1];

      tc_rep_newlist[i].freeptr = 0;
    }

  OCT_VAL_REP *tmp = tc_rep_newlist;
  tc_rep_newlist = tc_rep_newlist->freeptr;
  return tmp;
}

void
OCT_VAL_REP::operator delete (void *p, size_t /* size */)
{
  OCT_VAL_REP *tmp = (OCT_VAL_REP *) p;
  tmp->freeptr = tc_rep_newlist;
  tc_rep_newlist = tmp;
}

octave_value
OCT_VAL_REP::all (void) const
{
  octave_value retval;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      octave_value tmp = make_numeric ();

      if (error_state)
	return retval;

      return tmp.all ();
    }

  switch (type_tag)
    {
    default:
      gripe_wrong_type_arg ("all", *this);
      break;
    }

  return retval;
}

octave_value
OCT_VAL_REP::any (void) const
{
  octave_value retval;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      octave_value tmp = make_numeric ();

      if (error_state)
	return retval;

      return tmp.any ();
    }

  switch (type_tag)
    {
    default:
      gripe_wrong_type_arg ("any", *this);
      break;
    }

  return retval;
}

// XXX FIXME XXX -- we need a better way of handling conversions.

double
OCT_VAL_REP::double_value (bool force_string_conv) const
{
  double retval = octave_NaN;

  switch (type_tag)
    {
    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  gripe_implicit_conversion ("string", "real scalar");

	int len = char_matrix->rows ();
	if (flag
	    && ((char_matrix->rows () == 1 && len == 1)
		|| (len > 1 && Vdo_fortran_indexing)))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("string", "real scalar");
      }
      break;

    default:
      gripe_invalid_conversion (type_name (), "real scalar");
      break;
    }

  return retval;
}

Matrix
OCT_VAL_REP::matrix_value (bool force_string_conv) const
{
  Matrix retval;

  switch (type_tag)
    {
    case char_matrix_constant:
      retval = Matrix (*char_matrix);
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  gripe_implicit_conversion ("string", "real matrix");

	if (flag)
	  retval = Matrix (*char_matrix);
	else
	  gripe_invalid_conversion ("string", "real matrix");
      }
      break;

    default:
      gripe_invalid_conversion (type_name (), "real matrix");
      break;
    }

  return retval;
}

Complex
OCT_VAL_REP::complex_value (bool force_string_conv) const
{
  Complex retval (octave_NaN, octave_NaN);

  switch (type_tag)
    {
    case char_matrix_constant:
      {
	int len = char_matrix->cols ();
	if ((char_matrix->rows () == 1 && len == 1)
	    || (len > 1 && Vdo_fortran_indexing))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("char matrix", "complex scalar");
      }
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  gripe_implicit_conversion ("string", "complex scalar");

	int len = char_matrix->cols ();
	if (flag
	    && ((char_matrix->rows () == 1 && len == 1)
		|| (len > 1 && Vdo_fortran_indexing)))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("string", "complex scalar");
      }
      break;

    default:
      gripe_invalid_conversion (type_name (), "complex scalar");
      break;
    }

  return retval;
}

ComplexMatrix
OCT_VAL_REP::complex_matrix_value (bool force_string_conv) const
{
  ComplexMatrix retval;

  switch (type_tag)
    {
    case char_matrix_constant:
      retval = ComplexMatrix (*char_matrix);
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  gripe_implicit_conversion ("string", "complex matrix");

	if (flag)
	  retval = ComplexMatrix (*char_matrix);
	else
	  gripe_invalid_conversion ("complex", "real matrix");
      }
      break;

    default:
      gripe_invalid_conversion (type_name (), "complex matrix");
      break;
    }

  return retval;
}

// XXX FIXME XXX -- this needs to try to do some conversions...

charMatrix
OCT_VAL_REP::char_matrix_value (bool force_string_conv) const
{
  charMatrix retval;

  int flag = force_string_conv;
  if (! flag)
    flag = Vimplicit_str_to_num_ok;

  switch (type_tag)
    {
    case char_matrix_constant:
    case char_matrix_constant_str:
      retval = *char_matrix;
      break;

    default:
      if (! (rows () == 0 && columns () == 0))
	gripe_invalid_conversion (type_name (), "string");
      break;
    }

  return retval;
}

charMatrix
OCT_VAL_REP::all_strings (void) const
{
  if (type_tag == char_matrix_constant_str)
    return *char_matrix;
  else
    {
      gripe_invalid_conversion (type_name (), "string");
      return 0;
    }
}

string
OCT_VAL_REP::string_value (void) const
{
  string retval;

  if (type_tag == char_matrix_constant_str)
    retval = char_matrix->row_as_string (0);  // XXX FIXME??? XXX
  else
    gripe_invalid_conversion (type_name (), "string");

  return retval;
}

Octave_map
OCT_VAL_REP::map_value (void) const
{
  assert (type_tag == map_constant);
  return *a_map;
}

// This could be made more efficient by doing all the work here rather
// than relying on matrix_value() to do any possible type conversions.

ColumnVector
OCT_VAL_REP::vector_value (bool force_string_conv,
		      bool force_vector_conversion) const
{
  ColumnVector retval;

  Matrix m = matrix_value (force_string_conv);

  if (error_state)
    return retval;

  int nr = m.rows ();
  int nc = m.columns ();
  if (nr == 1)
    {
      retval.resize (nc);
      for (int i = 0; i < nc; i++)
	retval (i) = m (0, i);
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (int i = 0; i < nr; i++)
	retval (i) = m (i, 0);
    }
  else if (nr > 0 && nc > 0
	   && (Vdo_fortran_indexing || force_vector_conversion))
    {
      retval.resize (nr * nc);
      int k = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval (k++) = m (i, j);
    }
  else
    gripe_invalid_conversion ("real matrix", "real vector");

  return retval;
}

// This could be made more efficient by doing all the work here rather
// than relying on complex_matrix_value() to do any possible type
// conversions.

ComplexColumnVector
OCT_VAL_REP::complex_vector_value (bool force_string_conv,
			      bool force_vector_conversion) const
{
  ComplexColumnVector retval;

  ComplexMatrix m = complex_matrix_value (force_string_conv);

  if (error_state)
    return retval;

  int nr = m.rows ();
  int nc = m.columns ();
  if (nr == 1)
    {
      retval.resize (nc);
      for (int i = 0; i < nc; i++)
	retval (i) = m (0, i);
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (int i = 0; i < nr; i++)
	retval (i) = m (i, 0);
    }
  else if (nr > 0 && nc > 0
	   && (Vdo_fortran_indexing || force_vector_conversion))
    {
      retval.resize (nr * nc);
      int k = 0;
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval (k++) = m (i, j);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex vector");

  return retval;
}

octave_value
OCT_VAL_REP::convert_to_str (void) const
{
  octave_value retval;

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
	    // XXX FIXME XXX -- warn about out of range conversions?

	    int i = NINT (d);
	    char s[2];
	    s[0] = (char) i;
	    s[1] = '\0';
	    retval = octave_value (s, 1);
	  }
      }
      break;

    case complex_matrix_constant:
    case matrix_constant:
      {
	if (rows () == 0 && columns () == 0)
	  {
	    char s = '\0';
	    retval = octave_value (&s, 1);
	  }
	else
	  {
	    Matrix m = matrix_value ();

	    int nr = m.rows ();
	    int nc = m.columns ();

	    if (nr == 0 || nc == 0)
	      {
		char s = '\0';
		retval = octave_value (&s, 1);
	      }
	    else
	      {
		charMatrix chm (nr, nc);

		for (int j = 0; j < nc; j++)
		  {
		    for (int i = 0; i < nr; i++)
		      {
			double d = m (i, j);

			if (xisnan (d))
			  {
			    ::error ("invalid conversion from NaN to character");
			    return retval;
			  }
			else
			  {
			    // XXX FIXME XXX -- warn about out of
			    // range conversions?

			    int ival = NINT (d);
			    chm (i, j) = (char) ival;
			  }
		      }
		  }

		retval = octave_value (chm, 1);
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
		// XXX FIXME XXX -- warn about out of range
		// conversions?

		int ival = NINT (d);
		s[i] = (char) ival;
	      }
	  }
	retval = octave_value (s, 1);
	delete [] s;
      }
      break;

    case char_matrix_constant:
      retval = octave_value (*char_matrix, 1);
      break;

    case char_matrix_constant_str:
      retval = octave_value (*char_matrix, 1);
      break;

    default:
      gripe_invalid_conversion (type_name (), "string");
      break;
    }

  return retval;
}

void
OCT_VAL_REP::convert_to_row_or_column_vector (void)
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

  if (Vprefer_column_vectors)
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
	    (*m) (0, i) = *cop_out++;
	  else
	    (*m) (i, 0) = *cop_out++;
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
	    (*cm) (0, i) = *cop_out++;
	  else
	    (*cm) (i, 0) = *cop_out++;
	}

      delete complex_matrix;
      complex_matrix = cm;
    }
}

void
OCT_VAL_REP::convert_to_matrix_type (bool make_complex)
{
  switch (type_tag)
    {
    case complex_scalar_constant:
      {
	Complex *old_complex = complex_scalar;
	complex_matrix = new ComplexMatrix (1, 1, *complex_scalar);
	type_tag = complex_matrix_constant;
	delete old_complex;
      }
      break;

    case scalar_constant:
      {
	if (make_complex)
	  {
	    complex_matrix = new ComplexMatrix (1, 1, scalar);
	    type_tag = complex_matrix_constant;
	  }
	else
	  {
	    matrix = new Matrix (1, 1, scalar);
	    type_tag = matrix_constant;
	  }
      }
      break;

    case unknown_constant:
      {
	if (make_complex)
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
      break;

    case range_constant:
      {
	if (make_complex)
	  {
	    ComplexMatrix *tmp = new ComplexMatrix (range->matrix_value ());
	    delete range;
	    complex_matrix = tmp;
	    type_tag = complex_matrix_constant;
	  }
	else
	  {
	    Matrix *tmp = new Matrix (range->matrix_value ());
	    delete range;
	    matrix = tmp;
	    type_tag = matrix_constant;
	  }
      }
      break;

    default:
      panic_impossible ();
      break;
    }
}

void
OCT_VAL_REP::force_numeric (bool force_string_conv)
{
  switch (type_tag)
    {
    case scalar_constant:
    case matrix_constant:
    case complex_scalar_constant:
    case complex_matrix_constant:
    case char_matrix_constant:
      break;

    case char_matrix_constant_str:
      {
	if (! force_string_conv && ! Vimplicit_str_to_num_ok)
	  {
	    ::error ("string to numeric conversion failed --\
 default conversion turned off");
	    return;
	  }

	int nr = char_matrix->rows ();
	int nc = char_matrix->cols ();

	if (nr == 1 && nc == 1)
	  {
	    type_tag = scalar_constant;
	    double tmp = toascii ((int) (*char_matrix) (0, 0));
	    delete char_matrix;
	    scalar = tmp;
	  }
	else if (nr == 0 || nc == 0)
	  {
	    delete char_matrix;
	    type_tag = matrix_constant;
	    matrix = new Matrix (0, 0);
	  }
	else if (nr > 0 && nc > 0)
	  {
	    type_tag = matrix_constant;

	    Matrix *tm = new Matrix (nr, nc);

	    for (int i = 0; i < nr; i++)
	      {
		for (int j = 0; j < nc; j++)
		  {
		    int c = (int) (*char_matrix) (i, j);
		    (*tm) (i, j) = toascii (c);
		  }
	      }
	    delete char_matrix;
	    matrix = tm;
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
	      (*tm) (0, i) = b + i * increment;
	    delete range;
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
      gripe_invalid_conversion (type_name (), "numeric type");
      break;
    }
}

octave_value
OCT_VAL_REP::make_numeric (bool force_string_conv) const
{
  octave_value retval;

  switch (type_tag)
    {
    case scalar_constant:
      retval = scalar;
      break;

    case matrix_constant:
      retval = *matrix;
      break;

    case complex_scalar_constant:
      retval = *complex_scalar;
      break;

    case complex_matrix_constant:
      retval = *complex_matrix;
      break;

    case char_matrix_constant:
      retval = *char_matrix;
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  gripe_implicit_conversion ("string", "char matrix");

	if (flag)
	  {
	    retval = octave_value (*char_matrix, true);
	    retval.force_numeric (force_string_conv);
	  }
	else
	  gripe_invalid_conversion ("string", "char matrix");
      }
      break;

    case range_constant:
      retval = *range;
      retval.force_numeric (force_string_conv);
      break;

    default:
      gripe_invalid_conversion (type_name (), "numeric value");
      break;
    }

  return retval;
}

void
OCT_VAL_REP::maybe_mutate (void)
{
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
	  double d = (*matrix) (0, 0);
	  delete matrix;
	  scalar = d;
	  type_tag = scalar_constant;
	}
      break;

    case complex_matrix_constant:
      if (nr == 1 && nc == 1)
	{
	  Complex c = (*complex_matrix) (0, 0);
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
OCT_VAL_REP::print (ostream& output_buf)
{
  if (error_state)
    return;

    case map_constant:
      {
	// XXX FIXME XXX -- would be nice to print the output in some
	// standard order.  Maybe all substructures first, maybe
	// alphabetize entries, etc.

	begin_unwind_frame ("OCT_VAL_REP_print");

	unwind_protect_int (struct_indent);
	unwind_protect_int (Vstruct_levels_to_print);

	if (Vstruct_levels_to_print-- > 0)
	  {
	    output_buf.form ("\n%*s{\n", struct_indent, "");

	    increment_struct_indent ();

	    Pix p = a_map->first ();

	    while (p)
	      {
		bool pad_after = false;

		string key = a_map->key (p);
		octave_value val = a_map->contents (p);

		a_map->next (p);

		output_buf.form ("%*s%s =", struct_indent,
				 "", key.c_str ());

		if (val.print_as_scalar ())
		  output_buf << " ";
		else if (val.print_as_structure ())
		  {
		    if (p)
		      pad_after = true;
		  }
		else
		  {
		    if (p)
		      pad_after = true;

		    output_buf << "\n\n";
		  }

		val.print (output_buf);

		if (pad_after)
		  output_buf << "\n";
	      }

	    decrement_struct_indent ();

	    output_buf.form ("%*s%s", struct_indent, "", "}\n");
	  }
	else
	  output_buf << " <structure>\n";

	run_unwind_frame ("OCT_VAL_REP_print");
      }
      break;
}

// Indexing operations for the tree-constant representation class.

void
OCT_VAL_REP::clear_index (void)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->clear_index ();
      break;

    case OCT_VAL_REP::complex_matrix_constant:
      complex_matrix->clear_index ();
      break;

    case char_matrix_constant:
    case char_matrix_constant_str:
      char_matrix->clear_index ();
      break;

    default:
      panic_impossible ();
      break;
    }
}

#if 0
void
OCT_VAL_REP::set_index (double d)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->set_index (d);
      break;

    case OCT_VAL_REP::complex_matrix_constant:
      complex_matrix->set_index (d);
      break;

    case OCT_VAL_REP::char_matrix_constant:
    case OCT_VAL_REP::char_matrix_constant_str:
      char_matrix->set_index (d);
      break;

    default:
      panic_impossible ();
      break;
    }
}
#endif

void
OCT_VAL_REP::set_index (const Range& r)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->set_index (r);
      break;

    case OCT_VAL_REP::complex_matrix_constant:
      complex_matrix->set_index (r);
      break;

    case OCT_VAL_REP::char_matrix_constant:
    case OCT_VAL_REP::char_matrix_constant_str:
      char_matrix->set_index (r);
      break;

    default:
      panic_impossible ();
      break;
    }
}

void
OCT_VAL_REP::set_index (const ColumnVector& v)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->set_index (v);
      break;

    case OCT_VAL_REP::complex_matrix_constant:
      complex_matrix->set_index (v);
      break;

    case OCT_VAL_REP::char_matrix_constant:
    case OCT_VAL_REP::char_matrix_constant_str:
      char_matrix->set_index (v);
      break;

    default:
      panic_impossible ();
      break;
    }
}

void
OCT_VAL_REP::set_index (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.cols ();

  if (nr <= 1 || nc <= 1
      || Vdo_fortran_indexing)
    {
      switch (type_tag)
	{
	case matrix_constant:
	  matrix->set_index (m);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
	  complex_matrix->set_index (m);
	  break;

	case OCT_VAL_REP::char_matrix_constant:
	case OCT_VAL_REP::char_matrix_constant_str:
	  char_matrix->set_index (m);
	  break;

	default:
	  panic_impossible ();
	  break;
	}
    }
  else
    ::error ("invalid matrix used as index");
}

// XXX FIXME XXX -- this should probably be handled some other way...
// The arg here is expected to be ':'.
void
OCT_VAL_REP::set_index (char c)
{
  switch (type_tag)
    {
    case matrix_constant:
      matrix->set_index (c);
      break;

    case OCT_VAL_REP::complex_matrix_constant:
      complex_matrix->set_index (c);
      break;

    case OCT_VAL_REP::char_matrix_constant:
    case OCT_VAL_REP::char_matrix_constant_str:
      char_matrix->set_index (c);
      break;

    default:
      panic_impossible ();
      break;
    }
}

void
OCT_VAL_REP::set_index (const octave_value_list& args, bool rhs_is_complex)
{
  // XXX FIXME XXX -- it's not good that we have to list all the types
  // that can be indexed here.

  switch (type_tag)
    {
    case unknown_constant:
    case scalar_constant:
    case complex_scalar_constant:
    case range_constant:
      convert_to_matrix_type (rhs_is_complex);
      break;

    case matrix_constant:
    case complex_matrix_constant:
    case char_matrix_constant:
    case char_matrix_constant_str:
      break;

    default:
      ::error ("indexing %s type not implemented", type_name ());
      break;
    }

  if (! error_state)
    {
      int n = args.length ();

      for (int i = 0; i < n; i++)
	{
	  octave_value arg = args (i);

	  switch (arg.const_type ())
	    {
	    case range_constant:
	      set_index (arg.range_value ());
	      break;

	    case magic_colon:
	      set_index (':');
	      break;

	    default:
	      set_index (arg.matrix_value ());
	      break;
	    }

	  if (error_state)
	    {
	      clear_index ();
	      break;
	    }
	}
    }
}

static inline bool
valid_scalar_indices (const octave_value_list& args)
{
  int nargin = args.length ();

  for (int i = 0; i < nargin; i++)
    if (! args(i).valid_as_scalar_index ())
      return false;

  return true;
}

octave_value
OCT_VAL_REP::do_index (const octave_value_list& args)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool originally_scalar_type = is_scalar_type ();

  if (originally_scalar_type && valid_scalar_indices (args))
    {
      switch (type_tag)
	{
	case scalar_constant:
	  retval = scalar;
	  break;

	case complex_scalar_constant:
	  retval = *complex_scalar;
	  break;

	default:
	  panic_impossible ();
	  break;
	}
    }
  else
    {
      set_index (args);

      if (! error_state)
	{
	  switch (type_tag)
	    {
	    case range_constant:
	      force_numeric ();
	      // Fall through...

	    case matrix_constant:
	      retval = Matrix (matrix->value ());
	      break;

	    case complex_matrix_constant:
	      retval = ComplexMatrix (complex_matrix->value ());
	      break;

	    case char_matrix_constant:
	      retval = charMatrix (char_matrix->value ());
	      break;

	    case char_matrix_constant_str:
	      {
		// Kluge to prevent s([]) from turning into a string
		// with no rows...
		charMatrix tmp (char_matrix->value ());

		if (tmp.rows () == 0 && tmp.columns () == 0)
		  tmp.resize (1, 0);

		retval = octave_value (tmp, 1);
	      }
	      break;

	    default:
	      error ("can't index %s variables", type_name ());
	      break;
	    }

	  // We may have converted this value from a scalar to a
	  // matrix to allow indexing to work.

	  if (! error_state)
	    maybe_mutate ();
	}
    }

  return retval;
}

void
OCT_VAL_REP::maybe_widen (OCT_VAL_REP::constant_type rhs_type)
{
  switch (type_tag)
    {
    case matrix_constant:
      switch (rhs_type)
	{
	case complex_scalar_constant:
	case complex_matrix_constant:
	  {
	    ComplexMatrix *cm = new ComplexMatrix (*matrix);
	    delete matrix;
	    complex_matrix = cm;
	    type_tag = complex_matrix_constant;
	  }
	  break;

	default:
	  break;
	}
      break;

    case char_matrix_constant:
      switch (rhs_type)
	{
	case scalar_constant:
	case matrix_constant:
	  {
	    Matrix *m = new Matrix (*char_matrix);
	    delete matrix;
	    matrix = m;
	    type_tag = matrix_constant;
	  }
	  break;

	case complex_scalar_constant:
	case complex_matrix_constant:
	  {
	    ComplexMatrix *cm = new ComplexMatrix (*char_matrix);
	    delete matrix;
	    complex_matrix = cm;
	    type_tag = complex_matrix_constant;
	  }
	  break;

	default:
	  break;
	}
      break;

    default:
      break;
    }
}

// Assignment operations for the tree-constant representation class.

// Top-level tree-constant function that handles assignments.  Only
// decide if the left-hand side is currently a scalar or a matrix and
// hand off to other functions to do the real work.

// XXX FIXME XXX -- need some other way to make these functions
// visible here (they should be in some header file...)

extern void assign (Array2<Complex>&, const Array2<Complex>&);
extern void assign (Array2<Complex>&, const Array2<double>&);
extern void assign (Array2<Complex>&, const Array2<char>&);

extern void assign (Array2<double>&, const Array2<double>&);
extern void assign (Array2<double>&, const Array2<char>&);

extern void assign (Array2<char>&, const Array2<char>&);

void
OCT_VAL_REP::assign (octave_value& rhs, const octave_value_list& args)
{
  // XXX FIXME XXX -- we should probably have special cases for rhs
  // being a range type, since converting to a matrix can waste a lot
  // of memory.

  octave_value rhs_tmp = rhs;

  if (! (is_string ()
	 && (rhs_tmp.is_string ()
	     || rhs_tmp.is_zero_by_zero ())))
    {
      rhs_tmp = rhs_tmp.make_numeric ();

      if (error_state)
	return;
    }

  if (rhs_tmp.is_string
      && rhs_tmp.rows () == 1
      && rhs_tmp.columns () == 0)
    {
      rhs_tmp = rhs_tmp.make_numeric (1);

      if (error_state)
	return;
    }

  // An assignment to a range will normally require a conversion to a
  // vector in the end anyway, since it will normally destroy the
  // equally-spaced property of the range elements.  This is not as
  // memory efficient as possible, but it is much simpler than writing
  // additional indexing and assignment functions especially for
  // Ranges.

  if (is_defined () && ! (is_numeric_type () || is_string ()))
    {
      force_numeric ();

      if (error_state)
	return;
    }

  if (! rhs_tmp.is_zero_by_zero ())
    {
      maybe_widen (rhs_tmp.const_type ());

      if (error_state)
	return;
    }

  set_index (args, rhs_tmp.is_complex_type ());

  if (error_state)
    return;

  switch (type_tag)
    {
    case complex_matrix_constant:
      {
	switch (rhs_tmp.const_type ())
	  {
	  case complex_scalar_constant:
	  case complex_matrix_constant:
	    ::assign (*complex_matrix, rhs_tmp.complex_matrix_value ());
	    break;

	  case scalar_constant:
	  case matrix_constant:
	    ::assign (*complex_matrix, rhs_tmp.matrix_value ());
	    break;

	  default:
	    panic_impossible ();;
	    break;
	  }
      }
      break;

    case scalar_constant:
    case matrix_constant:
      {
	switch (rhs_tmp.const_type ())
	  {
	  case scalar_constant:
	  case matrix_constant:
	    ::assign (*matrix, rhs_tmp.matrix_value ());
	    break;

	  case char_matrix_constant:
	    ::assign (*matrix, rhs_tmp.char_matrix_value ());
	    break;

	  default:
	    panic_impossible ();
	    break;
	  }
      }
      break;

    case char_matrix_constant:
      ::assign (*char_matrix, rhs_tmp.char_matrix_value ());
      break;

    case char_matrix_constant_str:
      ::assign (*char_matrix, rhs_tmp.char_matrix_value ());
      if (char_matrix->rows () == 0 && char_matrix->columns () == 0)
	char_matrix->resize (1, 0);
      break;

    default:
      panic_impossible ();
      break;
    }

  // Do the right thing for assignments like `x(1) = pi' when x is
  // undefined before the assignment.

  if (is_matrix_type () || is_range ())
    maybe_mutate ();
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
