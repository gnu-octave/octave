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

#include "Array-flags.h"

#include "ov.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-ch-mat.h"
#include "ov-str-mat.h"
#include "ov-range.h"
#include "ov-struct.h"
#include "ov-colon.h"
#include "ov-va-args.h"
#include "ov-typeinfo.h"

#include "defun.h"
#include "gripes.h"
#include "pager.h"
#include "pr-output.h"
#include "utils.h"
#include "variables.h"

// If TRUE, allow assignments like
//
//   octave> A(1) = 3; A(2) = 5
//
// for A already defined and a matrix type.
bool Vdo_fortran_indexing;

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
int Vok_to_lose_imaginary_part;

// If TRUE, create column vectors when doing assignments like:
//
//   octave> A(1) = 3; A(2) = 5
//
// (for A undefined).  Only matters when resize_on_range_error is also
// TRUE.
bool Vprefer_column_vectors;

// If TRUE, prefer logical (zore-one) indexing over normal indexing
// when there is a conflice.  For example, given a = [2, 3], the
// expression  a ([1, 1]) would return [2 3] (instead of [2 2], which
// would be returned if prefer_zero_one_indxing were FALSE).
bool Vprefer_zero_one_indexing;

// If TRUE, print the name along with the value.
bool Vprint_answer_id_name;

// Should operations on empty matrices return empty matrices or an
// error?  A positive value means yes.  A negative value means yes,
// but print a warning message.  Zero means it should be considered an
// error.
int Vpropagate_empty_matrices;

// If TRUE, resize matrices when performing and indexed assignment and
// the indices are outside the current bounds.
bool Vresize_on_range_error;

// How many levels of structure elements should we print?
int Vstruct_levels_to_print;

// Allow divide by zero errors to be suppressed.
bool Vwarn_divide_by_zero;

// Indentation level for structures.
int struct_indent = 0;

// XXX FIXME XXX
void
increment_struct_indent (void)
{
  struct_indent += 2;
}

void
decrement_struct_indent (void)
{
  struct_indent -= 2;
}

// Octave's value type.

string
octave_value::binary_op_as_string (binary_op op)
{
  string retval;

  switch (op)
    {
    case add:
      retval = "+";
      break;

    case sub:
      retval = "-";
      break;

    case mul:
      retval = "*";
      break;

    case div:
      retval = "/";
      break;

    case pow:
      retval = "^";
      break;

    case ldiv:
      retval = "\\";
      break;

    case lt:
      retval = "<";
      break;

    case le:
      retval = "<=";
      break;

    case eq:
      retval = "==";
      break;

    case ge:
      retval = ">=";
      break;

    case gt:
      retval = ">";
      break;

    case ne:
      retval = "!=";
      break;

    case el_mul:
      retval = ".*";
      break;

    case el_div:
      retval = "./";
      break;

    case el_pow:
      retval = ".^";
      break;

    case el_ldiv:
      retval = ".\\";
      break;

    case el_and:
      retval = "&";
      break;

    case el_or:
      retval = "|";
      break;

    case struct_ref:
      retval = ".";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

octave_value::octave_value (void)
  : rep (new octave_base_value ()) { rep->count = 1; }

octave_value::octave_value (double d)
  : rep (new octave_scalar (d)) { rep->count = 1; }

octave_value::octave_value (const Matrix& m)
  : rep (new octave_matrix (m))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const DiagMatrix& d)
  : rep (new octave_matrix (d))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const RowVector& v, int pcv)
  : rep (new octave_matrix (v, pcv))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ColumnVector& v, int pcv)
  : rep (new octave_matrix (v, pcv))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const Complex& C)
  : rep (new octave_complex (C))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ComplexMatrix& m)
  : rep (new octave_complex_matrix (m))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ComplexDiagMatrix& d)
  : rep (new octave_complex_matrix (d))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ComplexRowVector& v, int pcv)
  : rep (new octave_complex_matrix (v, pcv))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ComplexColumnVector& v, int pcv)
  : rep (new octave_complex_matrix (v, pcv))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const char *s)
  : rep (new octave_char_matrix_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const string& s)
  : rep (new octave_char_matrix_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const string_vector& s)
  : rep (new octave_char_matrix_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const charMatrix& chm, bool is_string)
  : rep (0)
{
  if (is_string)
    rep = new octave_char_matrix_str (chm);
  else
    rep = new octave_char_matrix (chm);

  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (double base, double limit, double inc)
  : rep (new octave_range (base, limit, inc))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const Range& r)
  : rep (new octave_range (r))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const Octave_map& m)
  : rep (new octave_struct (m)) { rep->count = 1; }

octave_value::octave_value (octave_value::magic_colon)
  : rep (new octave_magic_colon ()) { rep->count = 1; }

octave_value::octave_value (octave_value::all_va_args)
  : rep (new octave_all_va_args ()) { rep->count = 1; }

octave_value::octave_value (octave_value *new_rep)
  : rep (new_rep) { rep->count = 1; }

octave_value::~octave_value (void)
{
#if defined (MDEBUG)
  cerr << "~octave_value: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (rep && --rep->count == 0)
    {
      delete rep;
      rep = 0;
    }
}

void
octave_value::maybe_mutate (void)
{
  octave_value *tmp = rep->try_narrowing_conversion ();

  if (tmp && tmp != rep)
    {
      if (--rep->count == 0)
	delete rep;

      rep = tmp;
      rep->count = 1;
    }    
}

static void
gripe_no_conversion (const string& tn1, const string& tn2)
{
  error ("no suitable conversion found for assignment of %s to indexed %s",
	 tn2.c_str (), tn1.c_str ());
}

octave_value&
octave_value::assign (const octave_value_list& idx, const octave_value& rhs)
{
  make_unique ();

  bool assignment_ok = try_assignment (idx, rhs);

  if (! (error_state || assignment_ok))
    {
      assignment_ok = try_assignment_with_conversion (idx, rhs);

      if (! (error_state || assignment_ok))
	gripe_no_conversion (type_name (), rhs.type_name ());
    }

  if (! error_state)
    maybe_mutate ();

  return *this;
}

Octave_map
octave_value::map_value (void) const
{
  return rep->map_value ();
}

ColumnVector
octave_value::vector_value (bool force_string_conv,
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
    {
      string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "real vector");
    }

  return retval;
}

ComplexColumnVector
octave_value::complex_vector_value (bool force_string_conv,
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
    {
      string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "complex vector");
    }

  return retval;
}

void
octave_value::print (bool pr_as_read_syntax)
{
  print (octave_stdout, pr_as_read_syntax);
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

bool
octave_value::print_as_scalar (void)
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

static void
gripe_indexed_assignment (const string& tn1, const string& tn2)
{
  error ("assignment of %s to indexed %s not implemented",
	 tn2.c_str (), tn1.c_str ());
}

static void
gripe_conversion_failed (const string& tn1, const string& tn2)
{
  error ("type conversion for assignment of %s to indexed %s failed",
	 tn2.c_str (), tn1.c_str ());
}

bool
octave_value::convert_and_assign (const octave_value_list& idx,
				  const octave_value& rhs)
{
  bool assignment_ok = false;

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  int t_result
    = octave_value_typeinfo::lookup_pref_assign_conv (t_lhs, t_rhs);

  if (t_result >= 0)
    {
      type_conv_fcn cf
	= octave_value_typeinfo::lookup_widening_op (t_lhs, t_result);

      if (cf)
	{
	  octave_value *tmp = cf (*rep);

	  if (tmp)
	    {
	      if (tmp != rep)
		{
		  if (--rep->count == 0)
		    delete rep;

		  rep = tmp;
		  rep->count = 1;
		}
	      else
		delete tmp;

	      assignment_ok = try_assignment (idx, rhs);
	    }
	  else
	    gripe_conversion_failed (type_name (), rhs.type_name ());
	}
      else
	gripe_indexed_assignment (type_name (), rhs.type_name ());
    }

  return (assignment_ok && ! error_state);
}

bool
octave_value::try_assignment_with_conversion (const octave_value_list& idx,
					      const octave_value& rhs)
{
  bool assignment_ok = convert_and_assign (idx, rhs);

  if (! (error_state || assignment_ok))
    {
      octave_value tmp_rhs;
      type_conv_fcn cf_rhs = rhs.numeric_conversion_function ();

      if (cf_rhs)
	tmp_rhs = octave_value (cf_rhs (*rhs.rep));
      else
	tmp_rhs = rhs;

      octave_value *old_rep = 0;
      type_conv_fcn cf_this = numeric_conversion_function ();

      if (cf_this)
	{
	  old_rep = rep;
	  rep = cf_this (*rep);
	  rep->count = 1;
	}

      if (cf_this || cf_rhs)
	{
	  assignment_ok = try_assignment (idx, tmp_rhs);

	  if (! (error_state || assignment_ok))
	    assignment_ok = convert_and_assign (idx, tmp_rhs);
	}

      if (! assignment_ok && old_rep)
	{
	  if (--rep->count == 0)
	    delete rep;

	  rep = old_rep;
	  old_rep = 0;
	}

      if (old_rep && --old_rep->count == 0)
	delete old_rep;
    }

  return (assignment_ok && ! error_state);
}

bool
octave_value::try_assignment (const octave_value_list& idx,
			      const octave_value& rhs)
{
  bool retval = false;

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  assign_op_fcn f = octave_value_typeinfo::lookup_assign_op (t_lhs, t_rhs);

  if (f)
    {
      f (*rep, idx, *(rhs.rep));

      retval = (! error_state);
    }

  return retval;
}

static void
gripe_binary_op (const string& on, const string& tn1, const string& tn2)
{
  error ("binary operator %s not implemented for %s by %s operations",
	 on.c_str (), tn1.c_str (), tn2.c_str ());
}

octave_value
do_binary_op (octave_value::binary_op op, const octave_value& v1,
	      const octave_value& v2)
{
  octave_value retval;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  binary_op_fcn f = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

  if (f)
    retval = f (*v1.rep, *v2.rep);
  else
    {
      octave_value tv1;
      type_conv_fcn cf1 = v1.numeric_conversion_function ();

      if (cf1)
	{
	  tv1 = octave_value (cf1 (*v1.rep));
	  t1 = tv1.type_id ();
	}
      else
	tv1 = v1;

      octave_value tv2;
      type_conv_fcn cf2 = v2.numeric_conversion_function ();

      if (cf2)
	{
	  tv2 = octave_value (cf2 (*v2.rep));
	  t2 = tv2.type_id ();
	}
      else
	tv2 = v2;

      if (cf1 || cf2)
	{
	  binary_op_fcn f
	    = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

	  if (f)
	    retval = f (*tv1.rep, *tv2.rep);
	  else
	    gripe_binary_op (octave_value::binary_op_as_string (op),
			     v1.type_name (), v2.type_name ());
	}
      else
	gripe_binary_op (octave_value::binary_op_as_string (op),
			 v1.type_name (), v2.type_name ());
    }

  return retval;
}

void
install_types (void)
{
  octave_base_value::register_type ();
  octave_scalar::register_type ();
  octave_complex::register_type ();
  octave_matrix::register_type ();
  octave_complex_matrix::register_type ();
  octave_range::register_type ();
  octave_char_matrix::register_type ();
  octave_char_matrix_str::register_type ();
  octave_struct::register_type ();
  octave_all_va_args::register_type ();
  octave_magic_colon::register_type ();
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

static int
warn_divide_by_zero (void)
{
  Vwarn_divide_by_zero = check_preference ("warn_divide_by_zero");

  return 0;
}

void
symbols_of_value (void)
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

  DEFVAR (warn_divide_by_zero, 1.0, 0, warn_divide_by_zero,
    "If TRUE, warn about division by zero");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
