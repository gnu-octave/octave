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

#include "arith-ops.h"
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

// If TRUE, allow assignments like
//
//   octave> A(1) = 3; A(2) = 5
//
// for A already defined and a matrix type.
static bool Vdo_fortran_indexing;

// Should we allow things like:
//
//   octave> 'abc' + 0
//   97 98 99
//
// to happen?  A positive value means yes.  A negative value means
// yes, but print a warning message.  Zero means it should be
// considered an error.
int Vimplicit_str_to_num_ok;

// Should we allow silent conversion of complex to real when a real
// type is what we're really looking for?  A positive value means yes.
// A negative value means yes, but print a warning message.  Zero
// means it should be considered an error.
static int Vok_to_lose_imaginary_part;

// If TRUE, create column vectors when doing assignments like:
//
//   octave> A(1) = 3; A(2) = 5
//
// (for A undefined).  Only matters when resize_on_range_error is also
// TRUE.
static bool Vprefer_column_vectors;

// If TRUE, prefer logical (zore-one) indexing over normal indexing
// when there is a conflice.  For example, given a = [2, 3], the
// expression  a ([1, 1]) would return [2 3] (instead of [2 2], which
// would be returned if prefer_zero_one_indxing were FALSE).
static bool Vprefer_zero_one_indexing;

// If TRUE, print the name along with the value.
static bool Vprint_answer_id_name;

// Should operations on empty matrices return empty matrices or an
// error?  A positive value means yes.  A negative value means yes,
// but print a warning message.  Zero means it should be considered an
// error.
int Vpropagate_empty_matrices;

// If TRUE, resize matrices when performing and indexed assignment and
// the indices are outside the current bounds.
bool Vresize_on_range_error;

// How many levels of structure elements should we print?
static int Vstruct_levels_to_print;

// Indentation level for structures.
static int struct_indent = 0;

static void
increment_struct_indent (void)
{
  struct_indent += 2;
}

static void
decrement_struct_indent (void)
{
  struct_indent -= 2;
}

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

octave_value
octave_value::lookup_map_element (const string& ref, bool insert,
				   bool silent)
{
  octave_value retval;

  if (! ref.empty ())
    {
      SLList<string> list;

      size_t beg = 0;
      size_t end;

      do
	{
	  end = ref.find ('.', beg);

	  string tmp = (end == NPOS)
	    ? ref.substr (beg) : ref.substr (beg, end - beg);

	  list.append (tmp);
	}
      while (end != NPOS && (beg = end + 1));

      retval = lookup_map_element (list, insert, silent);
    }

  return retval;
}

octave_value
octave_value::lookup_map_element (SLList<string>& list, bool insert,
				   bool silent)
{
  octave_value retval;

  octave_value_rep *tmp_rep = rep;

  Pix p = list.first ();
  while (p)
    {
      string elt = list (p);

      list.next (p);

      octave_value tmp;

      tmp = tmp_rep->lookup_map_element (elt, insert, silent);

      if (error_state)
	break;

      tmp_rep = tmp.rep;

      if (! p)
	retval = tmp;
    }

  return retval;
}

void
octave_value::print (void)
{
  print (octave_stdout);
}

void
octave_value::print_with_name (const string& name, bool print_padding)
{
  print_with_name (octave_stdout, name, print_padding);
}

void
octave_value::print_with_name (ostream& output_buf, const string& name,
			       bool print_padding) 
{
  bool pad_after = false;

  if (Vprint_answer_id_name)
    {
      if (print_as_scalar ())
	output_buf << name << " = ";
      else if (print_as_structure ())
	{
	  pad_after = true;
	  output_buf << name << " =";
	}
      else
	{
	  pad_after = true;
	  output_buf << name << " =\n\n";
	}
    }

  print (output_buf);

  if (print_padding && pad_after)
    output_buf << "\n";
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

void
octave_value::accept (tree_walker& tw)
{
  tw.visit_octave_value (*this);
}

// The real representation of constants.

OCT_VAL_REP::octave_value_rep (void)
{
  type_tag = unknown_constant;
}

OCT_VAL_REP::octave_value_rep (double d)
{
  scalar = d;
  type_tag = scalar_constant;
}

OCT_VAL_REP::octave_value_rep (const Matrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      scalar = m (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (m);
      type_tag = matrix_constant;
    }
}

OCT_VAL_REP::octave_value_rep (const DiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      scalar = d (0, 0);
      type_tag = scalar_constant;
    }
  else
    {
      matrix = new Matrix (d);
      type_tag = matrix_constant;
    }
}

OCT_VAL_REP::octave_value_rep (const RowVector& v, int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? Vprefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m (i, 0) = v (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m (0, i) = v (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
}

OCT_VAL_REP::octave_value_rep (const ColumnVector& v, int prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      scalar = v (0);
      type_tag = scalar_constant;
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? Vprefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  Matrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m (i, 0) = v (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
      else
	{
	  Matrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m (0, i) = v (i);
	  matrix = new Matrix (m);
	  type_tag = matrix_constant;
	}
    }
}

OCT_VAL_REP::octave_value_rep (const Complex& c)
{
  if (::imag (c) == 0.0)
    {
      scalar = ::real (c);
      type_tag = scalar_constant;
    }
  else
    {
      complex_scalar = new Complex (c);
      type_tag = complex_scalar_constant;
    }
}

OCT_VAL_REP::octave_value_rep (const ComplexMatrix& m)
{
  if (m.rows () == 1 && m.columns () == 1)
    {
      Complex c = m (0, 0);

      if (::imag (c) == 0.0)
	{
	  scalar = ::real (c);
	  type_tag = scalar_constant;
	}
      else
	{
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
    }
  else
    {
      complex_matrix = new ComplexMatrix (m);
      type_tag = complex_matrix_constant;
    }
}

OCT_VAL_REP::octave_value_rep (const ComplexDiagMatrix& d)
{
  if (d.rows () == 1 && d.columns () == 1)
    {
      Complex c = d (0, 0);

      if (::imag (c) == 0.0)
	{
	  scalar = ::real (c);
	  type_tag = scalar_constant;
	}
      else
	{
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
    }
  else
    {
      complex_matrix = new ComplexMatrix (d);
      type_tag = complex_matrix_constant;
    }
}

OCT_VAL_REP::octave_value_rep (const ComplexRowVector& v,
			   int prefer_column_vector) 
{
  int len = v.capacity ();
  if (len == 1)
    {
      Complex c = v (0);

      if (::imag (c) == 0.0)
	{
	  scalar = ::real (c);
	  type_tag = scalar_constant;
	}
      else
	{
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? Vprefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m (i, 0) = v (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m (0, i) = v (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
}

OCT_VAL_REP::octave_value_rep (const ComplexColumnVector& v, int
			   prefer_column_vector)
{
  int len = v.capacity ();
  if (len == 1)
    {
      Complex c = v (0);

      if (::imag (c) == 0.0)
	{
	  scalar = ::real (c);
	  type_tag = scalar_constant;
	}
      else
	{
	  complex_scalar = new Complex (c);
	  type_tag = complex_scalar_constant;
	}
    }
  else
    {
      int pcv = (prefer_column_vector < 0)
	? Vprefer_column_vectors
	  : prefer_column_vector;

      if (pcv)
	{
	  ComplexMatrix m (len, 1);
	  for (int i = 0; i < len; i++)
	    m (i, 0) = v (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
      else
	{
	  ComplexMatrix m (1, len);
	  for (int i = 0; i < len; i++)
	    m (0, i) = v (i);
	  complex_matrix = new ComplexMatrix (m);
	  type_tag = complex_matrix_constant;
	}
    }
}

OCT_VAL_REP::octave_value_rep (const char *s)
{
  char_matrix = new charMatrix (s);
  type_tag = char_matrix_constant_str;
}

OCT_VAL_REP::octave_value_rep (const string& s)
{
  char_matrix = new charMatrix (s);
  type_tag = char_matrix_constant_str;
}

OCT_VAL_REP::octave_value_rep (const string_vector& s)
{
  int nr = s.length ();
  int nc = s.max_length ();
  char_matrix = new charMatrix (nr, nc, 0);
  for (int i = 0; i < nr; i++)
    {
      nc = s[i].length ();
      for (int j = 0; j < nc; j++)
	(*char_matrix) (i, j) = s[i][j];
    }
  type_tag = char_matrix_constant_str;
}

OCT_VAL_REP::octave_value_rep (const charMatrix& chm, bool is_str)
{
  char_matrix = new charMatrix (chm);
  type_tag = is_str ? char_matrix_constant_str : char_matrix_constant;
}

OCT_VAL_REP::octave_value_rep (double b, double l, double i)
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
}

OCT_VAL_REP::octave_value_rep (const Range& r)
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
}

OCT_VAL_REP::octave_value_rep (const Octave_map& m)
{
  a_map = new Octave_map (m);
  type_tag = map_constant;
}

OCT_VAL_REP::octave_value_rep (OCT_VAL_REP::constant_type t)
{
  assert (t == magic_colon || t == all_va_args);
  type_tag = t;
}

OCT_VAL_REP::octave_value_rep (const octave_value_rep& t)
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

    case char_matrix_constant:
      char_matrix = new charMatrix (*(t.char_matrix));
      break;

    case char_matrix_constant_str:
      char_matrix = new charMatrix (*(t.char_matrix));
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

  orig_text = t.orig_text;
}

OCT_VAL_REP::~octave_value_rep (void)
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

    case char_matrix_constant:
    case char_matrix_constant_str:
      delete char_matrix;
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

int
OCT_VAL_REP::rows (void) const
{
  int retval = -1;

  switch (type_tag)
    {
    case scalar_constant:
    case complex_scalar_constant:
      retval = 1;
      break;

    case char_matrix_constant:
    case char_matrix_constant_str:
      retval = char_matrix->rows ();
      break;

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
OCT_VAL_REP::columns (void) const
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

    case char_matrix_constant:
    case char_matrix_constant_str:
      retval = char_matrix->columns ();
      break;

    case range_constant:
      retval = range->nelem ();
      break;

    default:
      break;
    }

  return retval;
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
    case scalar_constant:
      retval = (double) (scalar != 0.0);
      break;

    case matrix_constant:
      retval = matrix->all ();
      break;

    case complex_scalar_constant:
      retval = (double) (*complex_scalar != 0.0);
      break;

    case complex_matrix_constant:
      retval = complex_matrix->all ();
      break;

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
    case scalar_constant:
      retval = (double) (scalar != 0.0);
      break;

    case matrix_constant:
      retval = matrix->any ();
      break;

    case complex_scalar_constant:
      retval = (double) (*complex_scalar != 0.0);
      break;

    case complex_matrix_constant:
      retval = complex_matrix->any ();
      break;

    default:
      gripe_wrong_type_arg ("any", *this);
      break;
    }

  return retval;
}

bool
OCT_VAL_REP::valid_as_scalar_index (void) const
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

bool
OCT_VAL_REP::valid_as_zero_index (void) const
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

bool
OCT_VAL_REP::is_true (void) const
{
  int retval = false;

  if (error_state)
    return retval;

  if (! is_numeric_type ())
    {
      octave_value tmp = make_numeric ();

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
		  && m (0, 0) != 0.0);
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
		  && m (0, 0) != 0.0);
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

// XXX FIXME XXX -- we need a better way of handling conversions.

double
OCT_VAL_REP::double_value (bool force_string_conv) const
{
  double retval = octave_NaN;

  switch (type_tag)
    {
    case scalar_constant:
      retval = scalar;
      break;

    case matrix_constant:
      {
	if (Vdo_fortran_indexing && rows () > 0 && columns () > 0)
	  retval = (*matrix) (0, 0);
	else
	  gripe_invalid_conversion ("real matrix", "real scalar");
      }
      break;

    case complex_matrix_constant:
    case complex_scalar_constant:
      {
	int flag = Vok_to_lose_imaginary_part;

	if (flag < 0)
	  warn_implicit_conversion ("complex scalar", "real scalar");

	if (flag)
	  {
	    if (type_tag == complex_scalar_constant)
	      retval = ::real (*complex_scalar);
	    else if (type_tag == complex_matrix_constant)
	      {
		if (Vdo_fortran_indexing
		    && rows () > 0 && columns () > 0)
		  retval = ::real ((*complex_matrix) (0, 0));
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

    case char_matrix_constant:
      {
	int len = char_matrix->rows ();
	if ((char_matrix->rows () == 1 && len == 1)
	    || (len > 1 && Vdo_fortran_indexing))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("char matrix", "real scalar");
      }
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "real scalar");

	int len = char_matrix->rows ();
	if (flag
	    && ((char_matrix->rows () == 1 && len == 1)
		|| (len > 1 && Vdo_fortran_indexing)))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("string", "real scalar");
      }
      break;

    case range_constant:
      {
	int nel = range->nelem ();
	if (nel == 1 || (nel > 1 && Vdo_fortran_indexing))
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
OCT_VAL_REP::matrix_value (bool force_string_conv) const
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
	int flag = Vok_to_lose_imaginary_part;
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

    case char_matrix_constant:
      retval = Matrix (*char_matrix);
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "real matrix");

	if (flag)
	  retval = Matrix (*char_matrix);
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
OCT_VAL_REP::complex_value (bool force_string_conv) const
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
	if (Vdo_fortran_indexing && rows () > 0 && columns () > 0)
	  {
	    if (type_tag == complex_matrix_constant)
	      retval = (*complex_matrix) (0, 0);
	    else
	      retval = (*matrix) (0, 0);
	  }
	else
	  gripe_invalid_conversion ("real matrix", "real scalar");
      }
      break;

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
	  warn_implicit_conversion ("string", "complex scalar");

	int len = char_matrix->cols ();
	if (flag
	    && ((char_matrix->rows () == 1 && len == 1)
		|| (len > 1 && Vdo_fortran_indexing)))
	  retval = toascii ((int) (*char_matrix) (0, 0));
	else
	  gripe_invalid_conversion ("string", "complex scalar");
      }
      break;

    case range_constant:
      {
	int nel = range->nelem ();
	if (nel == 1 || (nel > 1 && Vdo_fortran_indexing))
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
OCT_VAL_REP::complex_matrix_value (bool force_string_conv) const
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

    case char_matrix_constant:
      retval = ComplexMatrix (*char_matrix);
      break;

    case char_matrix_constant_str:
      {
	int flag = force_string_conv;
	if (! flag)
	  flag = Vimplicit_str_to_num_ok;

	if (flag < 0)
	  warn_implicit_conversion ("string", "complex matrix");

	if (flag)
	  retval = ComplexMatrix (*char_matrix);
	else
	  gripe_invalid_conversion ("complex", "real matrix");
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
	gripe_invalid_conversion (type_as_string (), "string");
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
      gripe_invalid_conversion (type_as_string (), "string");
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
    gripe_invalid_conversion (type_as_string (), "string");

  return retval;
}

Range
OCT_VAL_REP::range_value (void) const
{
  assert (type_tag == range_constant);
  return *range;
}

Octave_map
OCT_VAL_REP::map_value (void) const
{
  assert (type_tag == map_constant);
  return *a_map;
}

octave_value&
OCT_VAL_REP::lookup_map_element (const string& name, bool insert, bool silent)
{
  static octave_value retval;

  if (type_tag == map_constant)
    {
      Pix idx = a_map->seek (name);

      if (idx)
	return a_map->contents (idx);
      else if (insert)
	return (*a_map) [name];
      else if (! silent)
	error ("structure has no member `%s'", name.c_str ());
    }
  else if (! silent)
    error ("invalid structure access attempted");

  return retval;
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
      gripe_invalid_conversion (type_as_string (), "string");
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
      gripe_invalid_conversion (type_as_string (), "numeric type");
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
	  warn_implicit_conversion ("string", "char matrix");

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
      gripe_invalid_conversion (type_as_string (), "numeric value");
      break;
    }

  return retval;
}

void
OCT_VAL_REP::bump_value (tree_expression::type etype)
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
OCT_VAL_REP::resize (int i, int j)
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
OCT_VAL_REP::resize (int i, int j, double val)
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
OCT_VAL_REP::stash_original_text (const string &s)
{
  orig_text = s;
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

  switch (type_tag)
    {
    case scalar_constant:
      octave_print_internal (output_buf, scalar, false);
      break;

    case matrix_constant:
      octave_print_internal (output_buf, *matrix, false,
			     struct_indent);
      break;

    case complex_scalar_constant:
      octave_print_internal (output_buf, *complex_scalar, false);
      break;

    case complex_matrix_constant:
      octave_print_internal (output_buf, *complex_matrix, false,
			     struct_indent);
      break;

    case char_matrix_constant:
      octave_print_internal (output_buf, *char_matrix, false,
			     struct_indent);
      break;

    case char_matrix_constant_str:
      octave_print_internal (output_buf, *char_matrix, false, true,
			     struct_indent);
      break;

    case range_constant:
      octave_print_internal (output_buf, *range, false, struct_indent);
      break;

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

    case unknown_constant:
    case magic_colon:
    case all_va_args:
      panic_impossible ();
      break;
    }
}

void
OCT_VAL_REP::gripe_wrong_type_arg (const char *name,
			      const octave_value_rep& tcr) const
{
  if (name)
    ::error ("%s: wrong type argument `%s'", name, tcr.type_as_string ());
  else
    ::error ("wrong type argument `%s'", name, tcr.type_as_string ());
}

char *
OCT_VAL_REP::type_as_string (void) const
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

    case char_matrix_constant:
      return "char matrix";

    case char_matrix_constant_str:
      return "string";

    case range_constant:
      return "range";

    case map_constant:
      return "structure";

    default:
      return "<unknown type>";
    }
}

octave_value
do_binary_op (octave_value& a, octave_value& b, tree_expression::type t)
{
  octave_value retval;

  bool first_empty = (a.rows () == 0 || a.columns () == 0);
  bool second_empty = (b.rows () == 0 || b.columns () == 0);

  if (first_empty || second_empty)
    {
      int flag = Vpropagate_empty_matrices;
      if (flag < 0)
	warning ("binary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid binary operation on empty matrix");
	  return retval;
	}
    }

  int force = (a.is_string () && b.is_string ()
	       && (t == tree_expression::cmp_lt
		   || t == tree_expression::cmp_le
		   || t == tree_expression::cmp_eq
		   || t == tree_expression::cmp_ge
		   || t == tree_expression::cmp_gt
		   || t == tree_expression::cmp_ne));

  octave_value tmp_a = a.make_numeric (force);

  if (error_state)
    return retval;

  octave_value tmp_b = b.make_numeric (force);

  if (error_state)
    return retval;

  OCT_VAL_REP::constant_type a_type = tmp_a.const_type ();
  OCT_VAL_REP::constant_type b_type = tmp_b.const_type ();

  double d1, d2;
  Matrix m1, m2;
  Complex c1, c2;
  ComplexMatrix cm1, cm2;

  switch (a_type)
    {
    case OCT_VAL_REP::scalar_constant:

      d1 = tmp_a.double_value ();

      switch (b_type)
	{
	case OCT_VAL_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (d1, d2, t);
	  break;

	case OCT_VAL_REP::matrix_constant:
	case OCT_VAL_REP::char_matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (d1, m2, t);
	  break;

	case OCT_VAL_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (d1, c2, t);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (d1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case OCT_VAL_REP::matrix_constant:
    case OCT_VAL_REP::char_matrix_constant:

      m1 = tmp_a.matrix_value ();

      switch (b_type)
	{
	case OCT_VAL_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (m1, d2, t);
	  break;

	case OCT_VAL_REP::matrix_constant:
	case OCT_VAL_REP::char_matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (m1, m2, t);
	  break;

	case OCT_VAL_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (m1, c2, t);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (m1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case OCT_VAL_REP::complex_scalar_constant:

      c1 = tmp_a.complex_value ();

      switch (b_type)
	{
	case OCT_VAL_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (c1, d2, t);
	  break;

	case OCT_VAL_REP::matrix_constant:
	case OCT_VAL_REP::char_matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (c1, m2, t);
	  break;

	case OCT_VAL_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (c1, c2, t);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
	  cm2 = tmp_b.complex_matrix_value ();
	  retval = do_binary_op (c1, cm2, t);
	  break;

	default:
	  gripe_wrong_type_arg_for_binary_op (tmp_b);
	  break;
	}
      break;

    case OCT_VAL_REP::complex_matrix_constant:

      cm1 = tmp_a.complex_matrix_value ();

      switch (b_type)
	{
	case OCT_VAL_REP::scalar_constant:
	  d2 = tmp_b.double_value ();
	  retval = do_binary_op (cm1, d2, t);
	  break;

	case OCT_VAL_REP::matrix_constant:
	case OCT_VAL_REP::char_matrix_constant:
	  m2 = tmp_b.matrix_value ();
	  retval = do_binary_op (cm1, m2, t);
	  break;

	case OCT_VAL_REP::complex_scalar_constant:
	  c2 = tmp_b.complex_value ();
	  retval = do_binary_op (cm1, c2, t);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
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

octave_value
do_unary_op (octave_value& a, tree_expression::type t)
{
  octave_value retval;

  if (a.rows () == 0 || a.columns () == 0)
    {
      int flag = Vpropagate_empty_matrices;
      if (flag < 0)
	warning ("unary operation on empty matrix");
      else if (flag == 0)
	{
	  ::error ("invalid unary operation on empty matrix");
	  return retval;
	}
    }

  // XXX FIXME XXX -- it is very unlikely that this is the correct
  // place for this special case...

  if (a.const_type () == OCT_VAL_REP::char_matrix_constant_str
      && (t == tree_expression::transpose
	  || t == tree_expression::hermitian))
    {
      charMatrix chm = a.all_strings ();

      if (! error_state)
	retval = octave_value (chm.transpose (), true);
    }
  else
    {
      octave_value tmp_a = a.make_numeric ();

      if (error_state)
	return retval;

      switch (tmp_a.const_type ())
	{
	case OCT_VAL_REP::scalar_constant:
	  retval = do_unary_op (tmp_a.double_value (), t);
	  break;

	case OCT_VAL_REP::matrix_constant:
	  {
	    Matrix m = tmp_a.matrix_value ();
	    retval = do_unary_op (m, t);
	  }
	  break;

	case OCT_VAL_REP::complex_scalar_constant:
	  retval = do_unary_op (tmp_a.complex_value (), t);
	  break;

	case OCT_VAL_REP::complex_matrix_constant:
	  {
	    ComplexMatrix m = tmp_a.complex_matrix_value ();
	    retval = do_unary_op (m, t);
	  }
	  break;

	default:
	  gripe_wrong_type_arg_for_unary_op (tmp_a);
	  break;
	}
    }

  return retval;
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
      ::error ("indexing %s type not implemented", type_as_string ());
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
	      error ("can't index %s variables", type_as_string ());
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

bool
OCT_VAL_REP::print_as_scalar (void)
{
  int nr = rows ();
  int nc = columns ();

  return (is_scalar_type ()
	  || (is_string () && nr <= 1)
	  || (is_matrix_type ()
	      && ((nr == 1 && nc == 1)
		  || nr == 0
		  || nc == 0)));
}

bool
OCT_VAL_REP::print_as_structure (void)
{
  return is_map ();
}

static int
do_fortran_indexing (void)
{
  Vdo_fortran_indexing = check_preference ("do_fortran_indexing");

  liboctave_dfi_flag = Vdo_fortran_indexing;

  return 0;
}

static int
implicit_str_to_num_ok (void)
{
  Vimplicit_str_to_num_ok = check_preference ("implicit_str_to_num_ok");

  return 0;
}

static int
ok_to_lose_imaginary_part (void)
{
  Vok_to_lose_imaginary_part = check_preference ("ok_to_lose_imaginary_part");

  return 0;
}

static int
prefer_column_vectors (void)
{
  Vprefer_column_vectors
    = check_preference ("prefer_column_vectors");

  liboctave_pcv_flag = Vprefer_column_vectors;

  return 0;
}

static int
prefer_zero_one_indexing (void)
{
  Vprefer_zero_one_indexing = check_preference ("prefer_zero_one_indexing");

  liboctave_pzo_flag = Vprefer_zero_one_indexing;

  return 0;
}

static int
print_answer_id_name (void)
{
  Vprint_answer_id_name = check_preference ("print_answer_id_name");

  return 0;
}

static int
propagate_empty_matrices (void)
{
  Vpropagate_empty_matrices = check_preference ("propagate_empty_matrices");

  return 0;
}

static int
resize_on_range_error (void)
{
  Vresize_on_range_error = check_preference ("resize_on_range_error");

  liboctave_rre_flag = Vresize_on_range_error;

  return 0;
}

static int
struct_levels_to_print (void)
{
  double val;
  if (builtin_real_scalar_variable ("struct_levels_to_print", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && (double) ival == val)
	{
	  Vstruct_levels_to_print = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("struct_levels_to_print");
  return -1;
}

void
symbols_of_pt_const (void)
{
  DEFVAR (do_fortran_indexing, 0.0, 0, do_fortran_indexing,
    "allow single indices for matrices");

  DEFVAR (implicit_str_to_num_ok, 0.0, 0, implicit_str_to_num_ok,
    "allow implicit string to number conversion");

  DEFVAR (ok_to_lose_imaginary_part, "warn", 0, ok_to_lose_imaginary_part,
    "silently convert from complex to real by dropping imaginary part");

  DEFVAR (prefer_column_vectors, 1.0, 0, prefer_column_vectors,
    "prefer column/row vectors");

  DEFVAR (prefer_zero_one_indexing, 0.0, 0, prefer_zero_one_indexing,
    "when there is a conflict, prefer zero-one style indexing");

  DEFVAR (print_answer_id_name, 1.0, 0, print_answer_id_name,
    "set output style to print `var_name = ...'");

  DEFVAR (propagate_empty_matrices, 1.0, 0, propagate_empty_matrices,
    "operations on empty matrices return an empty matrix, not an error");

  DEFVAR (resize_on_range_error, 1.0, 0, resize_on_range_error,
    "enlarge matrices on assignment");

  DEFVAR (struct_levels_to_print, 2.0, 0, struct_levels_to_print,
    "number of levels of structure elements to print");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
