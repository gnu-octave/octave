/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array-flags.h"
#include "data-conv.h"
#include "quit.h"
#include "str-vec.h"

#include "oct-obj.h"
#include "oct-stream.h"
#include "ov.h"
#include "ov-base.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-cell.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-bool-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-re-sparse.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-ch-mat.h"
#include "ov-str-mat.h"
#include "ov-range.h"
#include "ov-struct.h"
#include "ov-file.h"
#include "ov-streamoff.h"
#include "ov-list.h"
#include "ov-cs-list.h"
#include "ov-colon.h"
#include "ov-va-args.h"
#include "ov-builtin.h"
#include "ov-mapper.h"
#include "ov-dld-fcn.h"
#include "ov-usr-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-fcn-inline.h"
#include "ov-typeinfo.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "pager.h"
#include "parse.h"
#include "pr-output.h"
#include "utils.h"
#include "variables.h"

// We are likely to have a lot of octave_value objects to allocate, so
// make the grow_size large.
DEFINE_OCTAVE_ALLOCATOR2(octave_value, 1024);

// If TRUE, turn off printing of results in functions (as if a
// semicolon has been appended to each statement).
static bool Vsilent_functions;

// If TRUE, print a warning for assignments like
//
//   octave> A(1) = 3; A(2) = 5
//
// for A already defined and a matrix type.
bool Vwarn_fortran_indexing;

// Should we warn about conversions from complex to real?
int Vwarn_imag_to_real;

// Should we print a warning when converting `[97, 98, 99, "123"]'
// to a character string?
bool Vwarn_num_to_str;

// If TRUE, warn for operations like
//
//   octave> 'abc' + 0
//   97 98 99
//
int Vwarn_str_to_num;

// If TRUE, print the name along with the value.
bool Vprint_answer_id_name;

// How many levels of structure elements should we print?
int Vstruct_levels_to_print;

// Allow divide by zero errors to be suppressed.
bool Vwarn_divide_by_zero;

// If TRUE, print a warning when a matrix is resized by an indexed
// assignment with indices outside the current bounds.
bool Vwarn_resize_on_range_error;

// XXX FIXME XXX

// Octave's value type.

std::string
octave_value::unary_op_as_string (unary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_not:
      retval = "!";
      break;

    case op_uplus:
      retval = "+";
      break;

    case op_uminus:
      retval = "-";
      break;

    case op_transpose:
      retval = ".'";
      break;

    case op_hermitian:
      retval = "'";
      break;

    case op_incr:
      retval = "++";
      break;

    case op_decr:
      retval = "--";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

std::string
octave_value::binary_op_as_string (binary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_add:
      retval = "+";
      break;

    case op_sub:
      retval = "-";
      break;

    case op_mul:
      retval = "*";
      break;

    case op_div:
      retval = "/";
      break;

    case op_pow:
      retval = "^";
      break;

    case op_ldiv:
      retval = "\\";
      break;

    case op_lshift:
      retval = "<<";
      break;

    case op_rshift:
      retval = ">>";
      break;

    case op_lt:
      retval = "<";
      break;

    case op_le:
      retval = "<=";
      break;

    case op_eq:
      retval = "==";
      break;

    case op_ge:
      retval = ">=";
      break;

    case op_gt:
      retval = ">";
      break;

    case op_ne:
      retval = "!=";
      break;

    case op_el_mul:
      retval = ".*";
      break;

    case op_el_div:
      retval = "./";
      break;

    case op_el_pow:
      retval = ".^";
      break;

    case op_el_ldiv:
      retval = ".\\";
      break;

    case op_el_and:
      retval = "&";
      break;

    case op_el_or:
      retval = "|";
      break;

    case op_struct_ref:
      retval = ".";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

std::string
octave_value::assign_op_as_string (assign_op op)
{
  std::string retval;

  switch (op)
    {
    case op_asn_eq:
      retval = "=";
      break;

    case op_add_eq:
      retval = "+=";
      break;

    case op_sub_eq:
      retval = "-=";
      break;

    case op_mul_eq:
      retval = "*=";
      break;

    case op_div_eq:
      retval = "/=";
      break;

    case op_ldiv_eq:
      retval = "\\=";
      break;

    case op_pow_eq:
      retval = "^=";
      break;

    case op_lshift_eq:
      retval = "<<=";
      break;

    case op_rshift_eq:
      retval = ">>=";
      break;

    case op_el_mul_eq:
      retval = ".*=";
      break;

    case op_el_div_eq:
      retval = "./=";
      break;

    case op_el_ldiv_eq:
      retval = ".\\=";
      break;

    case op_el_pow_eq:
      retval = ".^=";
      break;

    case op_el_and_eq:
      retval = "&=";
      break;

    case op_el_or_eq:
      retval = "|=";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

octave_value *
octave_value::nil_rep (void) const
{
  static octave_base_value *nr = new octave_base_value ();
  return nr;
}

octave_value::octave_value (void)
  : rep (nil_rep ())
{
  rep->count++;
}

octave_value::octave_value (short int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

octave_value::octave_value (unsigned short int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

octave_value::octave_value (int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

octave_value::octave_value (unsigned int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

octave_value::octave_value (long int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

octave_value::octave_value (unsigned long int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}

#if defined (HAVE_LONG_LONG_INT)
octave_value::octave_value (long long int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}
#endif

#if defined (HAVE_UNSIGNED_LONG_LONG_INT)
octave_value::octave_value (unsigned long long int i)
  : rep (new octave_scalar (i))
{
  rep->count = 1;
}
#endif

octave_value::octave_value (octave_time t)
  : rep (new octave_scalar (t))
{
  rep->count = 1;
}

octave_value::octave_value (double d)
  : rep (new octave_scalar (d))
{
  rep->count = 1;
}

octave_value::octave_value (const Cell& c, bool is_csl)
  : rep (0)
{
  if (is_csl)
    rep = new octave_cs_list (c);
  else
    rep = new octave_cell (c);

  rep->count = 1;
}

octave_value::octave_value (const ArrayN<octave_value>& a, bool is_csl)
  : rep (0)
{
  Cell c (a);

  if (is_csl)
    rep = new octave_cs_list (c);
  else
    rep = new octave_cell (c);

  rep->count = 1;
}

octave_value::octave_value (const Matrix& m)
  : rep (new octave_matrix (m))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const NDArray& a)
  : rep (new octave_matrix (a))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ArrayN<double>& a)
  : rep (new octave_matrix (a))
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

octave_value::octave_value (const RowVector& v)
  : rep (new octave_matrix (v))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ColumnVector& v)
  : rep (new octave_matrix (v))
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

octave_value::octave_value (const ComplexNDArray& a)
  : rep (new octave_complex_matrix (a))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ArrayN<Complex>& a)
  : rep (new octave_complex_matrix (a))
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

octave_value::octave_value (const ComplexRowVector& v)
  : rep (new octave_complex_matrix (v))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ComplexColumnVector& v)
  : rep (new octave_complex_matrix (v))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (bool b)
  : rep (new octave_bool (b))
{
  rep->count = 1;
}

octave_value::octave_value (const boolMatrix& bm)
  : rep (new octave_bool_matrix (bm))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const boolNDArray& bnda)
  : rep (new octave_bool_matrix (bnda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (char c, char type)
  : rep (type == '"'
	 ? new octave_char_matrix_dq_str (c)
	 : new octave_char_matrix_sq_str (c))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const char *s, char type)
  : rep (type == '"'
	 ? new octave_char_matrix_dq_str (s)
	 : new octave_char_matrix_sq_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const std::string& s, char type)
  : rep (type == '"'
	 ? new octave_char_matrix_dq_str (s)
	 : new octave_char_matrix_sq_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const string_vector& s, char type)
  : rep (type == '"'
	 ? new octave_char_matrix_dq_str (s)
	 : new octave_char_matrix_sq_str (s))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const charMatrix& chm, bool is_str, char type)
  : rep (is_str
	 ? (type == '"'
	    ? new octave_char_matrix_dq_str (chm)
	    : new octave_char_matrix_sq_str (chm))
	 : new octave_char_matrix (chm))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const charNDArray& chm, bool is_str, char type)
  : rep (is_str
	 ? (type == '"'
	    ? new octave_char_matrix_dq_str (chm)
	    : new octave_char_matrix_sq_str (chm))
	 : new octave_char_matrix (chm))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const ArrayN<char>& chm, bool is_str, char type)
  : rep (is_str
	 ? (type == '"'
	    ? new octave_char_matrix_dq_str (chm)
	    : new octave_char_matrix_sq_str (chm))
	 : new octave_char_matrix (chm))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const SparseMatrix& m, const SparseType &t)
  : rep (new octave_sparse_matrix (m, t))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const SparseComplexMatrix& m, const SparseType &t)
  : rep (new octave_sparse_complex_matrix (m, t))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const SparseBoolMatrix& bm, const SparseType &t)
  : rep (new octave_sparse_bool_matrix (bm, t))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_int8& i)
  : rep (new octave_int8_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint8& i)
  : rep (new octave_uint8_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_int16& i)
  : rep (new octave_int16_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint16& i)
  : rep (new octave_uint16_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_int32& i)
  : rep (new octave_int32_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint32& i)
  : rep (new octave_uint32_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_int64& i)
  : rep (new octave_int64_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint64& i)
  : rep (new octave_uint64_scalar (i))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const int8NDArray& inda)
  : rep (new octave_int8_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const uint8NDArray& inda)
  : rep (new octave_uint8_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const int16NDArray& inda)
  : rep (new octave_int16_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const uint16NDArray& inda)
  : rep (new octave_uint16_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const int32NDArray& inda)
  : rep (new octave_int32_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const uint32NDArray& inda)
  : rep (new octave_uint32_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const int64NDArray& inda)
  : rep (new octave_int64_matrix (inda))
{
  rep->count = 1;
  maybe_mutate ();
}

octave_value::octave_value (const uint64NDArray& inda)
  : rep (new octave_uint64_matrix (inda))
{
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
  : rep (new octave_struct (m))
{
  rep->count = 1;
}

octave_value::octave_value (const streamoff_array& off)
  : rep (new octave_streamoff (off))
{
  rep->count = 1;
}

octave_value::octave_value (const octave_value_list& l, bool is_csl)
  : rep (0)
{
  if (is_csl)
    rep = new octave_cs_list (l);
  else
    rep = new octave_list (l);

  rep->count = 1;
}

octave_value::octave_value (octave_value::magic_colon)
  : rep (new octave_magic_colon ())
{
  rep->count = 1;
}

octave_value::octave_value (octave_value::all_va_args)
  : rep (new octave_all_va_args ())
{
  rep->count = 1;
}

octave_value::octave_value (octave_value *new_rep, int cnt)
  : rep (new_rep)
{
  rep->count = cnt;
}

octave_value::~octave_value (void)
{
#if defined (MDEBUG)
  std::cerr << "~octave_value: rep: " << rep
	    << " rep->count: " << rep->count << "\n";
#endif

  if (rep && --rep->count == 0)
    {
      delete rep;
      rep = 0;
    }
}

octave_value *
octave_value::clone (void) const
{
  panic_impossible ();
  return 0;
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

octave_value
octave_value::single_subsref (const std::string& type,
			      const octave_value_list& idx)
{
  std::list<octave_value_list> i;

  i.push_back (idx);

  return rep->subsref (type, i);
}

octave_value_list
octave_value::subsref (const std::string& type,
		       const std::list<octave_value_list>& idx, int nargout)
{
  if (is_constant ())
    return rep->subsref (type, idx);
  else
    return rep->subsref (type, idx, nargout);
}

octave_value
octave_value::next_subsref (const std::string& type,
			    const std::list<octave_value_list>& idx,
			    size_t skip) 
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
	new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx);
    }
  else
    return *this;
}

octave_value_list
octave_value::next_subsref (int nargout, const std::string& type,
			    const std::list<octave_value_list>& idx,
			    size_t skip) 
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
	new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, nargout);
    }
  else
    return *this;
}

octave_value_list
octave_value::do_multi_index_op (int nargout, const octave_value_list& idx)
{
  return rep->do_multi_index_op (nargout, idx);
}

static void
gripe_no_conversion (const std::string& on, const std::string& tn1,
		     const std::string& tn2)
{
  error ("operator %s: no conversion for assignment of `%s' to indexed `%s'",
	 on.c_str (), tn2.c_str (), tn1.c_str ());
}

#if 0
static void
gripe_assign_failed (const std::string& on, const std::string& tn1,
		     const std::string& tn2)
{
  error ("assignment failed for `%s %s %s'",
	 tn1.c_str (), on.c_str (), tn2.c_str ());
}
#endif

static void
gripe_assign_failed_or_no_method (const std::string& on,
				  const std::string& tn1,
				  const std::string& tn2)
{
  error ("assignment failed, or no method for `%s %s %s'",
	 tn1.c_str (), on.c_str (), tn2.c_str ());
}

octave_value
octave_value::subsasgn (const std::string& type,
			const std::list<octave_value_list>& idx,
			const octave_value& rhs)
{
  return rep->subsasgn (type, idx, rhs);
}

octave_value
octave_value::assign (assign_op op, const std::string& type,
		      const std::list<octave_value_list>& idx,
		      const octave_value& rhs)
{
  octave_value retval;

  make_unique ();

  octave_value t_rhs = rhs;

  if (op != op_asn_eq)
    {
      // XXX FIXME XXX -- only do the following stuff if we can't find
      // a specific function to call to handle the op= operation for
      // the types we have.

      octave_value t;
      if (is_constant ())
	t = subsref (type, idx);
      else
	{
	  octave_value_list tl = subsref (type, idx, 1);
	  if (tl.length () > 0)
	    t = tl(0);
	}

      if (! error_state)
	{
	  binary_op binop = op_eq_to_binary_op (op);

	  if (! error_state)
	    t_rhs = do_binary_op (binop, t, rhs);
	}
    }

  if (! error_state)
    {
      if (type[0] == '.' && ! is_map ())
	{
	  octave_value tmp = Octave_map ();
	  retval = tmp.subsasgn (type, idx, t_rhs);
	}
      else
	retval = subsasgn (type, idx, t_rhs);
    }

  if (error_state)
    gripe_assign_failed_or_no_method (assign_op_as_string (op),
				      type_name (), rhs.type_name ());

  return retval;
}

const octave_value&
octave_value::assign (assign_op op, const octave_value& rhs)
{
  if (op == op_asn_eq)
    operator = (rhs);
  else
    {
      // XXX FIXME XXX -- only do the following stuff if we can't find
      // a specific function to call to handle the op= operation for
      // the types we have.

      binary_op binop = op_eq_to_binary_op (op);

      if (! error_state)
	{
	  octave_value t = do_binary_op (binop, *this, rhs);

	  if (! error_state)
	    operator = (t);
	}

      if (error_state)
	gripe_assign_failed_or_no_method (assign_op_as_string (op),
					  type_name (), rhs.type_name ());
    }

  return *this;
}

octave_idx_type
octave_value::rows (void) const
{
  dim_vector dv = dims ();

  return (dv.length () > 0) ? dv(0) : -1;
}

octave_idx_type
octave_value::columns (void) const
{
  dim_vector dv = dims ();

  return (dv.length () > 1) ? dv(1) : -1;
}

octave_idx_type
octave_value::length (void) const
{
  int retval = 0;

  dim_vector dv = dims ();

  for (int i = 0; i < dv.length (); i++)
    {
      if (dv(i) < 0)
	{
	  retval = -1;
	  break;
	}

      if (dv(i) == 0)
	{
	  retval = 0;
	  break;
	}

      if (dv(i) > retval)
	retval = dv(i);
    }

  return retval;
}

int
octave_value::ndims (void) const
{
  dim_vector dv = dims ();

  int n_dims = dv.length ();
     
   // Remove trailing singleton dimensions.

   for (int i = n_dims; i > 2; i--)
     {
       if (dv(i-1) == 1)
	 n_dims--;
       else
	 break;
     }
   
   // The result is always >= 2.

   if (n_dims < 2)
     n_dims = 2;

   return n_dims;
}

Cell
octave_value::cell_value (void) const
{
  return rep->cell_value ();
}

Octave_map
octave_value::map_value (void) const
{
  return rep->map_value ();
}

std::streamoff
octave_value::streamoff_value (void) const
{
  return rep->streamoff_value ();
}

streamoff_array
octave_value::streamoff_array_value (void) const
{
  return rep->streamoff_array_value ();
}

octave_function *
octave_value::function_value (bool silent)
{
  return rep->function_value (silent);
}

octave_user_function *
octave_value::user_function_value (bool silent)
{
  return rep->user_function_value (silent);
}

octave_fcn_handle *
octave_value::fcn_handle_value (bool silent)
{
  return rep->fcn_handle_value (silent);
}

octave_fcn_inline *
octave_value::fcn_inline_value (bool silent)
{
  return rep->fcn_inline_value (silent);
}

octave_value_list
octave_value::list_value (void) const
{
  return rep->list_value ();
}

ColumnVector
octave_value::column_vector_value (bool force_string_conv,
				   bool /* frc_vec_conv */) const
{
  ColumnVector retval;

  Matrix m = matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nc == 1)
    {
      retval.resize (nr);
      for (octave_idx_type i = 0; i < nr; i++)
	retval (i) = m (i, 0);
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "real column vector");
    }

  return retval;
}

ComplexColumnVector
octave_value::complex_column_vector_value (bool force_string_conv,
					   bool /* frc_vec_conv */) const
{
  ComplexColumnVector retval;

  ComplexMatrix m = complex_matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nc == 1)
    {
      retval.resize (nr);
      for (octave_idx_type i = 0; i < nr; i++)
	retval (i) = m (i, 0);
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "complex column vector");
    }

  return retval;
}

RowVector
octave_value::row_vector_value (bool force_string_conv,
				bool /* frc_vec_conv */) const
{
  RowVector retval;

  Matrix m = matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 1)
    {
      retval.resize (nc);
      for (octave_idx_type i = 0; i < nc; i++)
	retval (i) = m (0, i);
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "real row vector");
    }

  return retval;
}

ComplexRowVector
octave_value::complex_row_vector_value (bool force_string_conv,
					bool /* frc_vec_conv */) const
{
  ComplexRowVector retval;

  ComplexMatrix m = complex_matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 1)
    {
      retval.resize (nc);
      for (octave_idx_type i = 0; i < nc; i++)
	retval (i) = m (0, i);
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "complex row vector");
    }

  return retval;
}

// Sloppy...

Array<double>
octave_value::vector_value (bool force_string_conv,
			    bool force_vector_conversion) const
{
  Array<double> retval;

  Matrix m = matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 1)
    {
      retval.resize (nc);
      for (octave_idx_type i = 0; i < nc; i++)
	retval (i) = m (0, i);
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (octave_idx_type i = 0; i < nr; i++)
	retval (i) = m (i, 0);
    }
  else if (nr > 0 && nc > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (! force_vector_conversion && Vwarn_fortran_indexing)
	gripe_implicit_conversion (type_name (), "real vector");

      retval.resize (nr * nc);
      octave_idx_type k = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  {
	    OCTAVE_QUIT;

	    retval (k++) = m (i, j);
	  }
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "real vector");
    }

  return retval;
}

Array<int>
octave_value::int_vector_value (bool force_string_conv, bool require_int,
				bool force_vector_conversion) const
{
  Array<int> retval;

  Matrix m = matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 1)
    {
      retval.resize (nc);
      for (octave_idx_type i = 0; i < nc; i++)
	{
	  OCTAVE_QUIT;

	  double d = m (0, i);

	  if (require_int && D_NINT (d) != d)
	    {
	      error ("conversion to integer value failed");
	      return retval;
	    }

	  retval (i) = static_cast<int> (d);
	}
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;

	  double d = m (i, 0);

	  if (require_int && D_NINT (d) != d)
	    {
	      error ("conversion to integer value failed");
	      return retval;
	    }

	  retval (i) = static_cast<int> (d);
	}
    }
  else if (nr > 0 && nc > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (! force_vector_conversion && Vwarn_fortran_indexing)
	gripe_implicit_conversion (type_name (), "real vector");

      retval.resize (nr * nc);
      octave_idx_type k = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	{
	  for (octave_idx_type i = 0; i < nr; i++)
	    {
	      OCTAVE_QUIT;

	      double d = m (i, j);

	      if (require_int && D_NINT (d) != d)
		{
		  error ("conversion to integer value failed");
		  return retval;
		}

	      retval (k++) = static_cast<int> (d);
	    }
	}
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "real vector");
    }

  return retval;
}

Array<Complex>
octave_value::complex_vector_value (bool force_string_conv,
				    bool force_vector_conversion) const
{
  Array<Complex> retval;

  ComplexMatrix m = complex_matrix_value (force_string_conv);

  if (error_state)
    return retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 1)
    {
      retval.resize (nc);
      for (octave_idx_type i = 0; i < nc; i++)
	{
	  OCTAVE_QUIT;
	  retval (i) = m (0, i);
	}
    }
  else if (nc == 1)
    {
      retval.resize (nr);
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  OCTAVE_QUIT;
	  retval (i) = m (i, 0);
	}
    }
  else if (nr > 0 && nc > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (! force_vector_conversion && Vwarn_fortran_indexing)
	gripe_implicit_conversion (type_name (), "complex vector");

      retval.resize (nr * nc);
      octave_idx_type k = 0;
      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  {
	    OCTAVE_QUIT;
	    retval (k++) = m (i, j);
	  }
    }
  else
    {
      std::string tn = type_name ();
      gripe_invalid_conversion (tn.c_str (), "complex vector");
    }

  return retval;
}

octave_value
octave_value::convert_to_str (bool pad, bool force, char type) const
{
  octave_value retval = convert_to_str_internal (pad, force, type);

  if (! force && is_numeric_type () && Vwarn_num_to_str)
    gripe_implicit_conversion (type_name (), retval.type_name ());

  return retval;
}

void
octave_value::print_with_name (std::ostream& output_buf,
			       const std::string& name, 
			       bool print_padding) const
{
  if (! (evaluating_function_body && Vsilent_functions))
    {
      bool pad_after = false;

      if (Vprint_answer_id_name)
	pad_after = print_name_tag (output_buf, name);

      print (output_buf);

      if (print_padding && pad_after)
	newline (output_buf);
    }
}

static void
gripe_indexed_assignment (const std::string& tn1, const std::string& tn2)
{
  error ("assignment of `%s' to indexed `%s' not implemented",
	 tn2.c_str (), tn1.c_str ());
}

static void
gripe_assign_conversion_failed (const std::string& tn1,
				const std::string& tn2)
{
  error ("type conversion for assignment of `%s' to indexed `%s' failed",
	 tn2.c_str (), tn1.c_str ());
}

int
octave_value::write (octave_stream& os, int block_size,
		     oct_data_conv::data_type output_type, int skip,
		     oct_mach_info::float_format flt_fmt) const
{
  return rep->write (os, block_size, output_type, skip, flt_fmt);
}

octave_value
octave_value::numeric_assign (const std::string& type,
			      const std::list<octave_value_list>& idx,
			      const octave_value& rhs)
{
  octave_value retval;

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  assign_op_fcn f
    = octave_value_typeinfo::lookup_assign_op (op_asn_eq, t_lhs, t_rhs);

  bool done = false;

  if (f)
    {
      f (*this, idx.front (), rhs.get_rep ());

      done = (! error_state);
    }

  if (done)
    retval = octave_value (this, count + 1);
  else
    {
      int t_result
	= octave_value_typeinfo::lookup_pref_assign_conv (t_lhs, t_rhs);

      if (t_result >= 0)
	{
	  type_conv_fcn cf
	    = octave_value_typeinfo::lookup_widening_op (t_lhs, t_result);

	  if (cf)
	    {
	      octave_value *tmp (cf (*this));

	      if (tmp)
		{
		  retval = tmp->subsasgn (type, idx, rhs);

		  done = (! error_state);
		}
	      else
		gripe_assign_conversion_failed (type_name (),
						rhs.type_name ());
	    }
	  else
	    gripe_indexed_assignment (type_name (), rhs.type_name ());
	}

      if (! (done || error_state))
	{
	  octave_value tmp_rhs;
	  type_conv_fcn cf_rhs = rhs.numeric_conversion_function ();

	  if (cf_rhs)
	    {
	      octave_value *tmp = cf_rhs (rhs.get_rep ());

	      if (tmp)
		tmp_rhs = octave_value (tmp);
	      else
		{
		  gripe_assign_conversion_failed (type_name (),
						  rhs.type_name ());
		  return octave_value ();
		}
	    }
	  else
	    tmp_rhs = rhs;

	  type_conv_fcn cf_this = numeric_conversion_function ();

	  octave_value *tmp_lhs = this;

	  if (cf_this)
	    {
	      octave_value *tmp = cf_this (*this);

	      if (tmp)
		tmp_lhs = tmp;
	      else
		{
		  gripe_assign_conversion_failed (type_name (),
						  rhs.type_name ());
		  return octave_value ();
		}
	    }

	  if (cf_this || cf_rhs)
	    {
	      retval = tmp_lhs->subsasgn (type, idx, tmp_rhs);

	      done = (! error_state);
	    }
	  else
	    gripe_no_conversion (assign_op_as_string (op_asn_eq),
				 type_name (), rhs.type_name ());
	}
    }

  // The assignment may have converted to a type that is wider than
  // necessary.

  retval.maybe_mutate ();

  return retval;
}

static void
gripe_binary_op (const std::string& on, const std::string& tn1,
		 const std::string& tn2)
{
  error ("binary operator `%s' not implemented for `%s' by `%s' operations",
	 on.c_str (), tn1.c_str (), tn2.c_str ());
}

static void
gripe_binary_op_conv (const std::string& on)
{
  error ("type conversion failed for binary operator `%s'", on.c_str ());
}

octave_value
do_binary_op (octave_value::binary_op op,
	      const octave_value& v1, const octave_value& v2)
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
	  octave_value *tmp = cf1 (*v1.rep);

	  if (tmp)
	    {
	      tv1 = octave_value (tmp);
	      t1 = tv1.type_id ();
	    }
	  else
	    {
	      gripe_binary_op_conv (octave_value::binary_op_as_string (op));
	      return retval;
	    }
	}
      else
	tv1 = v1;

      octave_value tv2;
      type_conv_fcn cf2 = v2.numeric_conversion_function ();

      if (cf2)
	{
	  octave_value *tmp = cf2 (*v2.rep);

	  if (tmp)
	    {
	      tv2 = octave_value (tmp);
	      t2 = tv2.type_id ();
	    }
	  else
	    {
	      gripe_binary_op_conv (octave_value::binary_op_as_string (op));
	      return retval;
	    }
	}
      else
	tv2 = v2;

      if (cf1 || cf2)
	{
	  f = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

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

static void
gripe_cat_op (const std::string& tn1, const std::string& tn2)
{
  error ("concatenation operator not implemented for `%s' by `%s' operations",
	 tn1.c_str (), tn2.c_str ());
}

static void
gripe_cat_op_conv (void)
{
  error ("type conversion failed for concatenation operator");
}

octave_value
do_cat_op (const octave_value& v1, const octave_value& v2, 
	   const Array<int>& ra_idx)
{
  octave_value retval;

  // Rapid return for concatenation with an empty object. Dimension
  // checking handled elsewhere.
  if (v1.all_zero_dims ())
    return v2;
  if (v2.all_zero_dims ())
    return v1;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  cat_op_fcn f = octave_value_typeinfo::lookup_cat_op (t1, t2);

  if (f)
    retval = f (*v1.rep, *v2.rep, ra_idx);
  else
    {
      octave_value tv1;
      type_conv_fcn cf1 = v1.numeric_conversion_function ();

      if (cf1)
	{
	  octave_value *tmp = cf1 (*v1.rep);

	  if (tmp)
	    {
	      tv1 = octave_value (tmp);
	      t1 = tv1.type_id ();
	    }
	  else
	    {
	      gripe_cat_op_conv ();
	      return retval;
	    }
	}
      else
	tv1 = v1;

      octave_value tv2;
      type_conv_fcn cf2 = v2.numeric_conversion_function ();

      if (cf2)
	{
	  octave_value *tmp = cf2 (*v2.rep);

	  if (tmp)
	    {
	      tv2 = octave_value (tmp);
	      t2 = tv2.type_id ();
	    }
	  else
	    {
	      gripe_cat_op_conv ();
	      return retval;
	    }
	}
      else
	tv2 = v2;

      if (cf1 || cf2)
	{
	  f = octave_value_typeinfo::lookup_cat_op (t1, t2);

	  if (f)
	    retval = f (*tv1.rep, *tv2.rep, ra_idx);
	  else
	    gripe_cat_op (v1.type_name (), v2.type_name ());
	}
      else
	gripe_cat_op (v1.type_name (), v2.type_name ());
    }

  return retval;
}

void
octave_value::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "type_name: " << type_name () << "\n"
     << prefix << "count:     " << get_count () << "\n"
     << prefix << "rep info:  ";

  rep->print_info (os, prefix + " ");
}

static void
gripe_unary_op (const std::string& on, const std::string& tn)
{
  error ("unary operator `%s' not implemented for `%s' operands",
	 on.c_str (), tn.c_str ());
}

static void
gripe_unary_op_conv (const std::string& on)
{
  error ("type conversion failed for unary operator `%s'", on.c_str ());
}

octave_value
do_unary_op (octave_value::unary_op op, const octave_value& v)
{
  octave_value retval;

  int t = v.type_id ();

  unary_op_fcn f = octave_value_typeinfo::lookup_unary_op (op, t);

  if (f)
    retval = f (*v.rep);
  else
    {
      octave_value tv;
      type_conv_fcn cf = v.numeric_conversion_function ();

      if (cf)
	{
	  octave_value *tmp = cf (*v.rep);

	  if (tmp)
	    {
	      tv = octave_value (tmp);
	      t = tv.type_id ();

	      f = octave_value_typeinfo::lookup_unary_op (op, t);

	      if (f)
		retval = f (*tv.rep);
	      else
		gripe_unary_op (octave_value::unary_op_as_string (op),
				v.type_name ());
	    }
	  else
	    gripe_unary_op_conv (octave_value::unary_op_as_string (op));
	}
      else
	gripe_unary_op (octave_value::unary_op_as_string (op),
			v.type_name ());
    }

  return retval;
}

static void
gripe_unary_op_conversion_failed (const std::string& op,
				  const std::string& tn)
{
  error ("operator %s: type conversion for `%s' failed",
	 op.c_str (), tn.c_str ());
}

const octave_value&
octave_value::do_non_const_unary_op (unary_op op)
{
  octave_value retval;

  int t = type_id ();

  non_const_unary_op_fcn f
    = octave_value_typeinfo::lookup_non_const_unary_op (op, t);

  if (f)
    {
      make_unique ();

      f (*rep);
    }
  else
    {
      type_conv_fcn cf = numeric_conversion_function ();

      if (cf)
	{
	  octave_value *tmp = cf (*rep);

	  if (tmp)
	    {
	      octave_value *old_rep = rep;
	      rep = tmp;
	      rep->count = 1;

	      t = type_id ();

	      f = octave_value_typeinfo::lookup_non_const_unary_op (op, t);

	      if (f)
		{
		  f (*rep);

		  if (old_rep && --old_rep->count == 0)
		    delete old_rep;
		}
	      else
		{
		  if (old_rep)
		    {
		      if (--rep->count == 0)
			delete rep;

		      rep = old_rep;
		    }

		  gripe_unary_op (octave_value::unary_op_as_string (op),
				  type_name ());
		}
	    }
	  else
	    gripe_unary_op_conversion_failed
	      (octave_value::unary_op_as_string (op), type_name ());
	}
      else
	gripe_unary_op (octave_value::unary_op_as_string (op), type_name ());
    }

  return *this;
}

#if 0
static void
gripe_unary_op_failed_or_no_method (const std::string& on,
				    const std::string& tn) 
{
  error ("operator %s: no method, or unable to evaluate for %s operand",
	 on.c_str (), tn.c_str ());
}
#endif

void
octave_value::do_non_const_unary_op (unary_op, const octave_value_list&)
{
  abort ();
}

octave_value
octave_value::do_non_const_unary_op (unary_op op, const std::string& type,
				     const std::list<octave_value_list>& idx)
{
  octave_value retval;

  if (idx.empty ())
    {
      do_non_const_unary_op (op);

      retval = *this;
    }
  else
    {
      // XXX FIXME XXX -- only do the following stuff if we can't find a
      // specific function to call to handle the op= operation for the
      // types we have.

      assign_op assop = unary_op_to_assign_op (op);

      retval = assign (assop, type, idx, 1.0);
    }

  return retval;
}

// Current indentation.
int octave_value::curr_print_indent_level = 0;

// TRUE means we are at the beginning of a line.
bool octave_value::beginning_of_line = true;

// Each print() function should call this before printing anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
octave_value::indent (std::ostream& os) const
{
  assert (curr_print_indent_level >= 0);
 
  if (beginning_of_line)
    {
      // XXX FIXME XXX -- do we need this?
      // os << prefix;

      for (int i = 0; i < curr_print_indent_level; i++)
	os << " ";

      beginning_of_line = false;
    }
}

// All print() functions should use this to print new lines.

void
octave_value::newline (std::ostream& os) const
{
  os << "\n";

  beginning_of_line = true;
}

// For ressetting print state.

void
octave_value::reset (void) const
{
  beginning_of_line = true;
  curr_print_indent_level = 0;
}

octave_value::assign_op
octave_value::unary_op_to_assign_op (unary_op op)
{
  assign_op binop = unknown_assign_op;

  switch (op)
    {
    case op_incr:
      binop = op_add_eq;
      break;

    case op_decr:
      binop = op_sub_eq;
      break;

    default:
      {
	std::string on = unary_op_as_string (op);
	error ("operator %s: no assign operator found", on.c_str ());
      }
    }

  return binop;
}

octave_value::binary_op 
octave_value::op_eq_to_binary_op (assign_op op)
{
  binary_op binop = unknown_binary_op;

  switch (op)
    {
    case op_add_eq:
      binop = op_add;
      break;

    case op_sub_eq:
      binop = op_sub;
      break;

    case op_mul_eq:
      binop = op_mul;
      break;

    case op_div_eq:
      binop = op_div;
      break;

    case op_ldiv_eq:
      binop = op_ldiv;
      break;

    case op_pow_eq:
      binop = op_pow;
      break;

    case op_lshift_eq:
      binop = op_lshift;
      break;

    case op_rshift_eq:
      binop = op_rshift;
      break;

    case op_el_mul_eq:
      binop = op_el_mul;
      break;

    case op_el_div_eq:
      binop = op_el_div;
      break;

    case op_el_ldiv_eq:
      binop = op_el_ldiv;
      break;

    case op_el_pow_eq:
      binop = op_el_pow;
      break;

    case op_el_and_eq:
      binop = op_el_and;
      break;

    case op_el_or_eq:
      binop = op_el_or;
      break;

    default:
      {
	std::string on = assign_op_as_string (op);
	error ("operator %s: no binary operator found", on.c_str ());
      }
    }

  return binop;
}

octave_value
octave_value::empty_conv (const std::string& type, const octave_value& rhs)
{
  octave_value retval;

  if (type.length () > 0)
    {
      switch (type[0])
	{
	case '(':
	  {
	    if (type.length () > 1 && type[1] == '.')
	      retval = Octave_map ();
	    else
	      retval = octave_value (rhs.empty_clone ());
	  }
	  break;

	case '{':
	  retval = Cell ();
	  break;

	case '.':
	  retval = Octave_map ();
	  break;

	default:
	  panic_impossible ();
	}
    }
  else
    retval = octave_value (rhs.empty_clone ());

  return retval;
}

void
install_types (void)
{
  octave_base_value::register_type ();
  octave_cell::register_type ();
  octave_scalar::register_type ();
  octave_complex::register_type ();
  octave_matrix::register_type ();
  octave_complex_matrix::register_type ();
  octave_range::register_type ();
  octave_bool::register_type ();
  octave_bool_matrix::register_type ();
  octave_char_matrix::register_type ();
  octave_char_matrix_str::register_type ();
  octave_char_matrix_sq_str::register_type ();
  octave_int8_scalar::register_type ();
  octave_int16_scalar::register_type ();
  octave_int32_scalar::register_type ();
  octave_int64_scalar::register_type ();
  octave_uint8_scalar::register_type ();
  octave_uint16_scalar::register_type ();
  octave_uint32_scalar::register_type ();
  octave_uint64_scalar::register_type ();
  octave_int8_matrix::register_type ();
  octave_int16_matrix::register_type ();
  octave_int32_matrix::register_type ();
  octave_int64_matrix::register_type ();
  octave_uint8_matrix::register_type ();
  octave_uint16_matrix::register_type ();
  octave_uint32_matrix::register_type ();
  octave_uint64_matrix::register_type ();
  octave_sparse_bool_matrix::register_type ();
  octave_sparse_matrix::register_type ();
  octave_sparse_complex_matrix::register_type ();
  octave_struct::register_type ();
  octave_list::register_type ();
  octave_cs_list::register_type ();
  octave_all_va_args::register_type ();
  octave_magic_colon::register_type ();
  octave_builtin::register_type ();
  octave_mapper::register_type ();
  octave_user_function::register_type ();
  octave_dld_function::register_type ();
  octave_fcn_handle::register_type ();
  octave_fcn_inline::register_type ();
  octave_streamoff::register_type ();
}

#if 0
DEFUN (cast, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cast (@var{val}, @var{type})\n\
Convert @var{val} to the new data type @var{type}.\n\
@end deftypefn\n\
@seealso{class, typeinfo}")
{
  octave_value retval;

  if (args.length () == 2)
    error ("cast: not implemented");
  else
    print_usage ("cast");

  return retval;
}
#endif

DEFUN (sizeof, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sizeof (@var{val})\n\
Return the size of @var{val} in bytes\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).byte_size ();
  else
    print_usage ("sizeof");

  return retval;
}

static int
warn_fortran_indexing (void)
{
  Vwarn_fortran_indexing = check_preference ("warn_fortran_indexing");

  liboctave_wfi_flag = Vwarn_fortran_indexing;

  return 0;
}

static int
warn_imag_to_real (void)
{
  Vwarn_imag_to_real = check_preference ("warn_imag_to_real");

  return 0;
}

static int
warn_num_to_str (void)
{
  Vwarn_num_to_str = check_preference ("warn_num_to_str");

  return 0;
}

static int
warn_str_to_num (void)
{
  Vwarn_str_to_num = check_preference ("warn_str_to_num");

  return 0;
}

static int
print_answer_id_name (void)
{
  Vprint_answer_id_name = check_preference ("print_answer_id_name");

  return 0;
}

static int
warn_resize_on_range_error (void)
{
  Vwarn_resize_on_range_error
    = check_preference ("warn_resize_on_range_error");

  liboctave_wrore_flag = Vwarn_resize_on_range_error;

  return 0;
}

static int
silent_functions (void)
{
  Vsilent_functions = check_preference ("silent_functions");

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
      if (ival == val)
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
symbols_of_ov (void)
{
  DEFVAR (print_answer_id_name, true, print_answer_id_name,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} print_answer_id_name\n\
If the value of @code{print_answer_id_name} is nonzero, variable\n\
names are printed along with the result.  Otherwise, only the result\n\
values are printed.  The default value is 1.\n\
@end defvr");

  DEFVAR (silent_functions, false, silent_functions,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} silent_functions\n\
If the value of @code{silent_functions} is nonzero, internal output\n\
from a function is suppressed.  Otherwise, the results of expressions\n\
within a function body that are not terminated with a semicolon will\n\
have their values printed.  The default value is 0.\n\
\n\
For example, if the function\n\
\n\
@example\n\
function f ()\n\
  2 + 2\n\
endfunction\n\
@end example\n\
\n\
@noindent\n\
is executed, Octave will either print @samp{ans = 4} or nothing\n\
depending on the value of @code{silent_functions}.\n\
@end defvr");

  DEFVAR (struct_levels_to_print, 2.0, struct_levels_to_print,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} struct_levels_to_print\n\
You can tell Octave how many structure levels to display by setting the\n\
built-in variable @code{struct_levels_to_print}.  The default value is 2.\n\
@end defvr");

  DEFVAR (warn_divide_by_zero, true, warn_divide_by_zero,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_divide_by_zero\n\
If the value of @code{warn_divide_by_zero} is nonzero, a warning\n\
is issued when Octave encounters a division by zero.  If the value is\n\
0, the warning is omitted.  The default value is 1.\n\
@end defvr");

  DEFVAR (warn_fortran_indexing, false, warn_fortran_indexing,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_fortran_indexing\n\
If the value of @code{warn_fortran_indexing} is nonzero, a warning is\n\
printed for expressions which select elements of a two-dimensional matrix\n\
using a single index.  The default value is 0.\n\
@end defvr");

  DEFVAR (warn_imag_to_real, false, warn_imag_to_real,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_imag_to_real\n\
If the value of @code{warn_imag_to_real} is nonzero, a warning is\n\
printed for implicit conversions of complex numbers to real numbers.\n\
The default value is 0.\n\
@end defvr");

  DEFVAR (warn_num_to_str, true, warn_num_to_str,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_num_to_str\n\
If the value of @code{warn_num_to_str} is nonzero, a warning is\n\
printed for implicit conversions of numbers to their ASCII character\n\
equivalents when strings are constructed using a mixture of strings and\n\
numbers in matrix notation.  For example,\n\
\n\
@example\n\
@group\n\
[ \"f\", 111, 111 ]\n\
     @result{} \"foo\"\n\
@end group\n\
@end example\n\
elicits a warning if @code{warn_num_to_str} is nonzero.  The default\n\
value is 1.\n\
@end defvr");

  DEFVAR (warn_resize_on_range_error, false, warn_resize_on_range_error,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_resize_on_range_error\n\
If the value of @code{warn_resize_on_range_error} is nonzero, print a\n\
warning when a matrix is resized by an indexed assignment with\n\
indices outside the current bounds.  The default value is 0.\n\
@end defvr");

  DEFVAR (warn_str_to_num, false, warn_str_to_num,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_str_to_num\n\
If the value of @code{warn_str_to_num} is nonzero, a warning is printed\n\
for implicit conversions of strings to their numeric ASCII equivalents.\n\
For example,\n\
@example\n\
@group\n\
\"abc\" + 0\n\
     @result{} 97 98 99\n\
@end group\n\
@end example\n\
elicits a warning if @code{warn_str_to_num} is nonzero.  The default\n\
value is 0.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
