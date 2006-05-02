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

#include <climits>

#include <iostream>

#include "Array-flags.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "so-array.h"

#include "defun.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-cell.h"
#include "ov-ch-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-list.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "ov-fcn-handle.h"
#include "parse.h"
#include "utils.h"
#include "variables.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_base_value,
				     "<unknown type>", "unknown");

// If TRUE, print the name along with the value.
static bool Vprint_answer_id_name;

// If TRUE, turn off printing of results in functions (as if a
// semicolon has been appended to each statement).
static bool Vsilent_functions;

octave_value
octave_base_value::squeeze (void) const
{
  std::string nm = type_name ();
  error ("squeeze: invalid operation for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::subsref (const std::string&,
			    const std::list<octave_value_list>&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::subsref (const std::string&,
			    const std::list<octave_value_list>&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::do_index_op (const octave_value_list&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::do_index_op (const octave_value_list& idx)
{
  return do_index_op (idx, 0);
}

octave_value_list
octave_base_value::do_multi_index_op (int, const octave_value_list&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

idx_vector
octave_base_value::index_vector (void) const
{
  std::string nm = type_name ();
  error ("%s type invalid as index value", nm.c_str ());
  return idx_vector ();
}

int
octave_base_value::ndims (void) const
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

octave_value
octave_base_value::subsasgn (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     const octave_value& rhs)
{
  octave_value retval;

  if (is_defined ())
    {
      if (is_numeric_type ())
	{
	  switch (type[0])
	    {
	    case '(':
	      {
		if (type.length () == 1)
		  retval = numeric_assign (type, idx, rhs);
		else if (is_empty ())
		  {
		    // Allow conversion of empty matrix to some other
		    // type in cases like
		    //
		    //  x = []; x(i).f = rhs

		    octave_value tmp = octave_value::empty_conv (type, rhs);

		    retval = tmp.subsasgn (type, idx, rhs);
		  }
		else
		  {
		    std::string nm = type_name ();
		    error ("in indexed assignment of %s, last rhs index must be ()",
			   nm.c_str ());
		  }
	      }
	      break;

	    case '{':
	    case '.':
	      {
		std::string nm = type_name ();
		error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	      }
	      break;

	    default:
	      panic_impossible ();
	    }
	}
      else
	{
	  std::string nm = type_name ();
	  error ("can't perform indexed assignment for %s type", nm.c_str ());
	}
    }
  else
    {
      // Create new object of appropriate type for given index and rhs
      // types and then call subsasgn again for that object.

      octave_value tmp = octave_value::empty_conv (type, rhs);

      retval = tmp.subsasgn (type, idx, rhs);
    }

  return retval;
}

octave_idx_type
octave_base_value::nnz (void) const
{
  gripe_wrong_type_arg ("octave_base_value::nnz ()", type_name ());
  return -1;
}

octave_idx_type
octave_base_value::nzmax (void) const
{
  gripe_wrong_type_arg ("octave_base_value::nzmax ()", type_name ());
  return -1;
}

octave_value
octave_base_value::reshape (const dim_vector&) const
{
  gripe_wrong_type_arg ("octave_base_value::reshape ()", type_name ());
  return octave_value ();
}

octave_value
octave_base_value::permute (const Array<int>&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::permute ()", type_name ());
  return octave_value ();
}

octave_value
octave_base_value::resize (const dim_vector&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::resize ()", type_name ());
  return octave_value ();
}

octave_value
octave_base_value::all (int) const
{
  return 0.0;
}

octave_value
octave_base_value::any (int) const
{
  return 0.0;
}

octave_value
octave_base_value::convert_to_str (bool pad, bool force, char type) const
{
  octave_value retval = convert_to_str_internal (pad, force, type);

  if (! force && is_numeric_type ())
    gripe_implicit_conversion ("Octave:num-to-str",
			       type_name (), retval.type_name ());

  return retval;
}

octave_value
octave_base_value::convert_to_str_internal (bool, bool, char) const
{
  gripe_wrong_type_arg ("octave_base_value::convert_to_str_internal ()",
			type_name ());
  return octave_value ();
}

void
octave_base_value::convert_to_row_or_column_vector (void)
{
  gripe_wrong_type_arg
    ("octave_base_value::convert_to_row_or_column_vector ()",
     type_name ());
}

void
octave_base_value::print (std::ostream&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::print ()", type_name ());
}

void
octave_base_value::print_raw (std::ostream&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::print_raw ()", type_name ());
}

bool
octave_base_value::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (print_as_scalar ())
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      newline (os);
      retval = true;
    }

  return retval;
}

void
octave_base_value::print_with_name (std::ostream& output_buf,
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

void
octave_base_value::print_info (std::ostream& os,
			       const std::string& /* prefix */) const
{
  os << "no info for type: " << type_name () << "\n";
}

#define INT_CONV_METHOD(T, F, MIN_LIMIT, MAX_LIMIT) \
  T \
  octave_base_value::F ## _value (bool require_int, bool frc_str_conv) const \
  { \
    T retval = 0; \
 \
    double d = double_value (frc_str_conv); \
 \
    if (! error_state) \
      { \
	if (require_int && D_NINT (d) != d) \
	  error ("conversion of %g to " #T " value failed", d); \
	else if (d < MIN_LIMIT) \
	  retval = MIN_LIMIT; \
	else if (d > MAX_LIMIT) \
	  retval = MAX_LIMIT; \
	else \
	  retval = static_cast<T> (fix (d)); \
      } \
    else \
      gripe_wrong_type_arg ("octave_base_value::" #F "_value ()", \
			    type_name ()); \
 \
    return retval; \
  }

INT_CONV_METHOD (short int, short, SHRT_MIN, SHRT_MAX)
INT_CONV_METHOD (unsigned short int, ushort, 0, USHRT_MAX)

INT_CONV_METHOD (int, int, INT_MIN, INT_MAX)
INT_CONV_METHOD (unsigned int, uint, 0, UINT_MAX)

INT_CONV_METHOD (long int, long, LONG_MIN, LONG_MAX)
INT_CONV_METHOD (unsigned long int, ulong, 0, ULONG_MAX)

int
octave_base_value::nint_value (bool frc_str_conv) const
{
  int retval = 0;

  double d = double_value (frc_str_conv);

  if (! error_state)
    {
      if (xisnan (d))
	{
	  error ("conversion of NaN to integer value failed");
	  return retval;
	}

      retval = static_cast<int> (fix (d));
    }
  else
    gripe_wrong_type_arg ("octave_base_value::nint_value ()", type_name ());

  return retval;
}

double
octave_base_value::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();
  gripe_wrong_type_arg ("octave_base_value::double_value ()", type_name ());
  return retval;
}

Cell
octave_base_value::cell_value () const
{
  Cell retval;
  gripe_wrong_type_arg ("octave_base_value::cell_value()", type_name ());
  return retval;
}

Matrix
octave_base_value::matrix_value (bool) const
{
  Matrix retval;
  gripe_wrong_type_arg ("octave_base_value::matrix_value()", type_name ());
  return retval;
}

NDArray
octave_base_value::array_value (bool) const
{
  NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::array_value()", type_name ());
  return retval;
}

Complex
octave_base_value::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();
  Complex retval (tmp, tmp);
  gripe_wrong_type_arg ("octave_base_value::complex_value()", type_name ());
  return retval;
}

ComplexMatrix
octave_base_value::complex_matrix_value (bool) const
{
  ComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::complex_matrix_value()",
			type_name ());
  return retval;
}

ComplexNDArray
octave_base_value::complex_array_value (bool) const
{
  ComplexNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::complex_array_value()",
			type_name ());
  return retval;
}

bool
octave_base_value::bool_value (void) const
{
  bool retval = false;
  gripe_wrong_type_arg ("octave_base_value::bool_value()", type_name ());
  return retval;
}

boolMatrix
octave_base_value::bool_matrix_value (void) const
{
  boolMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::bool_matrix_value()",
			type_name ());
  return retval;
}

boolNDArray
octave_base_value::bool_array_value (void) const
{
  boolNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::bool_array_value()",
			type_name ());
  return retval;
}

charMatrix
octave_base_value::char_matrix_value (bool force) const
{
  charMatrix retval;

  octave_value tmp = convert_to_str (false, force);

  if (! error_state)
    retval = tmp.char_matrix_value ();

  return retval;
}

charNDArray
octave_base_value::char_array_value (bool) const
{
  charNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::char_array_value()",
			type_name ());
  return retval;
}

SparseMatrix
octave_base_value::sparse_matrix_value (bool) const
{
  SparseMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_matrix_value()", type_name ());
  return retval;
}

SparseComplexMatrix
octave_base_value::sparse_complex_matrix_value (bool) const
{
  SparseComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_complex_matrix_value()", type_name ());
  return retval;
}

SparseBoolMatrix
octave_base_value::sparse_bool_matrix_value (bool) const
{
  SparseBoolMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_bool_matrix_value()", type_name ());
  return retval;
}

octave_int8
octave_base_value::int8_scalar_value (void) const
{
  octave_int8 retval;
  gripe_wrong_type_arg ("octave_base_value::int8_scalar_value()",
			type_name ());
  return retval;
}

octave_int16
octave_base_value::int16_scalar_value (void) const
{
  octave_int16 retval;
  gripe_wrong_type_arg ("octave_base_value::int16_scalar_value()",
			type_name ());
  return retval;
}

octave_int32
octave_base_value::int32_scalar_value (void) const
{
  octave_int32 retval;
  gripe_wrong_type_arg ("octave_base_value::int32_scalar_value()",
			type_name ());
  return retval;
}

octave_int64
octave_base_value::int64_scalar_value (void) const
{
  octave_int64 retval;
  gripe_wrong_type_arg ("octave_base_value::int64_scalar_value()",
			type_name ());
  return retval;
}

octave_uint8
octave_base_value::uint8_scalar_value (void) const
{
  octave_uint8 retval;
  gripe_wrong_type_arg ("octave_base_value::uint8_scalar_value()",
			type_name ());
  return retval;
}

octave_uint16
octave_base_value::uint16_scalar_value (void) const
{
  octave_uint16 retval;
  gripe_wrong_type_arg ("octave_base_value::uint16_scalar_value()",
			type_name ());
  return retval;
}

octave_uint32
octave_base_value::uint32_scalar_value (void) const
{
  octave_uint32 retval;
  gripe_wrong_type_arg ("octave_base_value::uint32_scalar_value()",
			type_name ());
  return retval;
}

octave_uint64
octave_base_value::uint64_scalar_value (void) const
{
  octave_uint64 retval;
  gripe_wrong_type_arg ("octave_base_value::uint64_scalar_value()",
			type_name ());
  return retval;
}

int8NDArray
octave_base_value::int8_array_value (void) const
{
  int8NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int8_array_value()",
			type_name ());
  return retval;
}

int16NDArray
octave_base_value::int16_array_value (void) const
{
  int16NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int16_array_value()",
			type_name ());
  return retval;
}

int32NDArray
octave_base_value::int32_array_value (void) const
{
  int32NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int32_array_value()",
			type_name ());
  return retval;
}

int64NDArray
octave_base_value::int64_array_value (void) const
{
  int64NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int64_array_value()",
			type_name ());
  return retval;
}

uint8NDArray
octave_base_value::uint8_array_value (void) const
{
  uint8NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint8_array_value()",
			type_name ());
  return retval;
}

uint16NDArray
octave_base_value::uint16_array_value (void) const
{
  uint16NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint16_array_value()",
			type_name ());
  return retval;
}

uint32NDArray
octave_base_value::uint32_array_value (void) const
{
  uint32NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint32_array_value()",
			type_name ());
  return retval;
}

uint64NDArray
octave_base_value::uint64_array_value (void) const
{
  uint64NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint64_array_value()",
			type_name ());
  return retval;
}

string_vector
octave_base_value::all_strings (bool pad) const
{
  string_vector retval;

  octave_value tmp = convert_to_str (pad, true);

  if (! error_state)
    retval = tmp.all_strings ();

  return retval;
}

std::string
octave_base_value::string_value (bool force) const
{
  std::string retval;

  octave_value tmp = convert_to_str (force);

  if (! error_state)
    retval = tmp.string_value ();

  return retval;
}

Range
octave_base_value::range_value (void) const
{
  Range retval;
  gripe_wrong_type_arg ("octave_base_value::range_value()", type_name ());
  return retval;
}

Octave_map
octave_base_value::map_value (void) const
{
  Octave_map retval;
  gripe_wrong_type_arg ("octave_base_value::map_value()", type_name ());
  return retval;
}

string_vector
octave_base_value::map_keys (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_base_value::map_keys()", type_name ());
  return retval;
}

std::streamoff
octave_base_value::streamoff_value (void) const
{
  std::streamoff retval (-1);
  gripe_wrong_type_arg ("octave_base_value::streamoff_value()", type_name ());
  return retval;
}

streamoff_array
octave_base_value::streamoff_array_value (void) const
{
  streamoff_array retval;
  gripe_wrong_type_arg ("octave_base_value::streamoff_array_value()",
			type_name ());
  return retval;
}

octave_function *
octave_base_value::function_value (bool silent)
{
  octave_function *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::function_value()",
			  type_name ());
  return retval;
}

octave_user_function *
octave_base_value::user_function_value (bool silent)
{
  octave_user_function *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::user_function_value()",
			  type_name ());
  return retval;
}

octave_fcn_handle *
octave_base_value::fcn_handle_value (bool silent)
{
  octave_fcn_handle *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::fcn_handle_value()",
			  type_name ());
  return retval;
}

octave_fcn_inline *
octave_base_value::fcn_inline_value (bool silent)
{
  octave_fcn_inline *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::fcn_inline_value()",
			  type_name ());
  return retval;
}

octave_value_list
octave_base_value::list_value (void) const
{
  octave_value_list retval;
  gripe_wrong_type_arg ("octave_base_value::list_value()", type_name ());
  return retval;
}

bool 
octave_base_value::save_ascii (std::ostream&, bool&, bool)
{
  gripe_wrong_type_arg ("octave_base_value::save_ascii()", type_name ());
  return false;
}

bool 
octave_base_value::load_ascii (std::istream&)
{
  gripe_wrong_type_arg ("octave_base_value::load_ascii()", type_name ());
  return false;
}

bool 
octave_base_value::save_binary (std::ostream&, bool&)
{
  gripe_wrong_type_arg ("octave_base_value::save_binary()", type_name ());
  return false;
}

bool 
octave_base_value::load_binary (std::istream&, bool,
				oct_mach_info::float_format)
{
  gripe_wrong_type_arg ("octave_base_value::load_binary()", type_name ());
  return false;
}

#if defined (HAVE_HDF5)

bool
octave_base_value::save_hdf5 (hid_t, const char *, bool)
{
  gripe_wrong_type_arg ("octave_base_value::save_binary()", type_name ());

  return false;
}

bool 
octave_base_value::load_hdf5 (hid_t, const char *, bool)
{
  gripe_wrong_type_arg ("octave_base_value::load_binary()", type_name ());

  return false;
}

#endif

int
octave_base_value::write (octave_stream&, int, oct_data_conv::data_type,
			  int, oct_mach_info::float_format) const
{
  gripe_wrong_type_arg ("octave_base_value::write()", type_name ());

  return false;
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

static void
gripe_no_conversion (const std::string& on, const std::string& tn1,
		     const std::string& tn2)
{
  error ("operator %s: no conversion for assignment of `%s' to indexed `%s'",
	 on.c_str (), tn2.c_str (), tn1.c_str ());
}

octave_value
octave_base_value::numeric_assign (const std::string& type,
				   const std::list<octave_value_list>& idx,
				   const octave_value& rhs)
{
  octave_value retval;

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  octave_value_typeinfo::assign_op_fcn f
    = octave_value_typeinfo::lookup_assign_op (octave_value::op_asn_eq,
					       t_lhs, t_rhs);

  bool done = false;

  if (f)
    {
      f (*this, idx.front (), rhs.get_rep ());

      done = (! error_state);
    }

  if (done)
    {
      count++;
      retval = octave_value (this);
    }
  else
    {
      int t_result
	= octave_value_typeinfo::lookup_pref_assign_conv (t_lhs, t_rhs);

      if (t_result >= 0)
	{
	  octave_base_value::type_conv_fcn cf
	    = octave_value_typeinfo::lookup_widening_op (t_lhs, t_result);

	  if (cf)
	    {
	      octave_base_value *tmp (cf (*this));

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

	  octave_base_value::type_conv_fcn cf_rhs
	    = rhs.numeric_conversion_function ();

	  if (cf_rhs)
	    {
	      octave_base_value *tmp = cf_rhs (rhs.get_rep ());

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

	  octave_base_value::type_conv_fcn cf_this
	    = numeric_conversion_function ();

	  octave_base_value *tmp_lhs = this;

	  if (cf_this)
	    {
	      octave_base_value *tmp = cf_this (*this);

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
	    gripe_no_conversion (octave_value::assign_op_as_string (octave_value::op_asn_eq),
				 type_name (), rhs.type_name ());
	}
    }

  // The assignment may have converted to a type that is wider than
  // necessary.

  retval.maybe_mutate ();

  return retval;
}

// Current indentation.
int octave_base_value::curr_print_indent_level = 0;

// TRUE means we are at the beginning of a line.
bool octave_base_value::beginning_of_line = true;

// Each print() function should call this before printing anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
octave_base_value::indent (std::ostream& os) const
{
  assert (curr_print_indent_level >= 0);
 
  if (beginning_of_line)
    {
      // FIXME -- do we need this?
      // os << prefix;

      for (int i = 0; i < curr_print_indent_level; i++)
	os << " ";

      beginning_of_line = false;
    }
}

// All print() functions should use this to print new lines.

void
octave_base_value::newline (std::ostream& os) const
{
  os << "\n";

  beginning_of_line = true;
}

// For ressetting print state.

void
octave_base_value::reset (void) const
{
  beginning_of_line = true;
  curr_print_indent_level = 0;
}

CONVDECLX (matrix_conv)
{
  return new octave_matrix ();
}

CONVDECLX (complex_matrix_conv)
{
  return new octave_complex_matrix ();
}

CONVDECLX (string_conv)
{
  return new octave_char_matrix_str ();
}

CONVDECLX (cell_conv)
{
  return new octave_cell ();
}

void
install_base_type_conversions (void)
{
  INSTALL_ASSIGNCONV (octave_base_value, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_matrix, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex_matrix, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_range, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_char_matrix_str, octave_char_matrix_str);
  INSTALL_ASSIGNCONV (octave_base_value, octave_cell, octave_cell);

  INSTALL_WIDENOP (octave_base_value, octave_matrix, matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_complex_matrix, complex_matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_char_matrix_str, string_conv);
  INSTALL_WIDENOP (octave_base_value, octave_cell, cell_conv);
}

static int
print_answer_id_name (void)
{
  Vprint_answer_id_name = check_preference ("print_answer_id_name");

  return 0;
}

static int
silent_functions (void)
{
  Vsilent_functions = check_preference ("silent_functions");

  return 0;
}

void
symbols_of_ov_base (void)
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
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
