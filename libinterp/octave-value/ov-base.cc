////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <istream>
#include <limits>
#include <ostream>

#include "lo-ieee.h"
#include "lo-mappers.h"

#include "defun.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "mxarray.h"
#include "oct-hdf5.h"
#include "oct-lvalue.h"
#include "oct-map.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-cell.h"
#include "ov-ch-mat.h"
#include "ov-classdef.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-fcn-handle.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "ovl.h"
#include "parse.h"
#include "pr-flt-fmt.h"
#include "pr-output.h"
#include "utils.h"
#include "variables.h"

builtin_type_t btyp_mixed_numeric (builtin_type_t x, builtin_type_t y)
{
  builtin_type_t retval = btyp_unknown;

  if (x == btyp_bool)
    x = btyp_double;
  if (y == btyp_bool)
    y = btyp_double;

  if (x <= btyp_float_complex && y <= btyp_float_complex)
    retval = static_cast<builtin_type_t> (x | y);
  else if (x <= btyp_uint64 && y <= btyp_float)
    retval = x;
  else if (x <= btyp_float && y <= btyp_uint64)
    retval = y;
  else if ((x >= btyp_int8 && x <= btyp_int64
            && y >= btyp_int8 && y <= btyp_int64)
           || (x >= btyp_uint8 && x <= btyp_uint64
               && y >= btyp_uint8 && y <= btyp_uint64))
    retval = (x > y) ? x : y;

  return retval;
}

std::string btyp_class_name[btyp_num_types+1] =
{
  "double", "single", "double", "single",
  "int8", "int16", "int32", "int64",
  "uint8", "uint16", "uint32", "uint64",
  "logical", "char",
  "struct", "cell", "function_handle", "unknown"
};

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_base_value,
                                     "<unknown type>", "unknown");

// DEPRECATED in Octave 8.
bool Vsparse_auto_mutate = false;

octave_base_value *
octave_base_value::empty_clone (void) const
{
  return resize (dim_vector ()).clone ();
}

octave_value
octave_base_value::squeeze (void) const
{
  std::string nm = type_name ();
  error ("squeeze: invalid operation for %s type", nm.c_str ());
}

octave_value
octave_base_value::full_value (void) const
{
  err_wrong_type_arg ("full: invalid operation for %s type", type_name ());
}

octave_value
octave_base_value::as_double (void) const
{
  err_invalid_conversion (type_name (), "double");
}

octave_value
octave_base_value::as_single (void) const
{
  err_invalid_conversion (type_name (), "single");
}

octave_value
octave_base_value::as_int8 (void) const
{
  err_invalid_conversion (type_name (), "int8");
}

octave_value
octave_base_value::as_int16 (void) const
{
  err_invalid_conversion (type_name (), "int16");
}

octave_value
octave_base_value::as_int32 (void) const
{
  err_invalid_conversion (type_name (), "int32");
}

octave_value
octave_base_value::as_int64 (void) const
{
  err_invalid_conversion (type_name (), "int64");
}

octave_value
octave_base_value::as_uint8 (void) const
{
  err_invalid_conversion (type_name (), "uint8");
}

octave_value
octave_base_value::as_uint16 (void) const
{
  err_invalid_conversion (type_name (), "uint16");
}

octave_value
octave_base_value::as_uint32 (void) const
{
  err_invalid_conversion (type_name (), "uint32");
}

octave_value
octave_base_value::as_uint64 (void) const
{
  err_invalid_conversion (type_name (), "uint64");
}

Matrix
octave_base_value::size (void)
{
  const dim_vector dv = dims ();
  Matrix mdv (1, dv.ndims ());
  for (octave_idx_type i = 0; i < dv.ndims (); i++)
    mdv(i) = dv(i);
  return mdv;
}

octave_idx_type
octave_base_value::xnumel (const octave_value_list& idx)
{
  return octave::dims_to_numel (dims (), idx);
}

octave_value
octave_base_value::subsref (const std::string&,
                            const std::list<octave_value_list>&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
}

octave_value_list
octave_base_value::subsref (const std::string&,
                            const std::list<octave_value_list>&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
}

octave_value
octave_base_value::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            bool /* auto_add */)
{
  // This way we may get a more meaningful error message.
  return subsref (type, idx);
}

octave_value
octave_base_value::do_index_op (const octave_value_list&, bool)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
}

octave::idx_vector
octave_base_value::index_vector (bool /* require_integers */) const
{
  std::string nm = '<' + type_name () + '>';
  octave::err_invalid_index (nm.c_str ());
}

octave_value
octave_base_value::subsasgn (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             const octave_value& rhs)
{
  octave_value retval;

  if (is_defined ())
    {
      if (! isnumeric ())
        {
          std::string nm = type_name ();
          error ("can't perform indexed assignment for %s type", nm.c_str ());
        }

      switch (type[0])
        {
        case '(':
          {
            if (type.length () == 1)
              retval = numeric_assign (type, idx, rhs);
            else if (isempty ())
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
      // Create new object of appropriate type for given index and rhs
      // types and then call undef_subsasgn for that object.

      octave_value tmp = octave_value::empty_conv (type, rhs);

      retval = tmp.undef_subsasgn (type, idx, rhs);
    }

  return retval;
}

octave_value
octave_base_value::undef_subsasgn (const std::string& type,
                                   const std::list<octave_value_list>& idx,
                                   const octave_value& rhs)
{
  // In most cases, undef_subsasgn is handled the sams as subsasgn.  One
  // exception is octave_class objects.

  return subsasgn (type, idx, rhs);
}

octave_idx_type
octave_base_value::nnz (void) const
{
  err_wrong_type_arg ("octave_base_value::nnz ()", type_name ());
}

octave_idx_type
octave_base_value::nzmax (void) const
{
  return numel ();
}

octave_idx_type
octave_base_value::nfields (void) const
{
  err_wrong_type_arg ("octave_base_value::nfields ()", type_name ());
}

octave_value
octave_base_value::reshape (const dim_vector&) const
{
  err_wrong_type_arg ("octave_base_value::reshape ()", type_name ());
}

octave_value
octave_base_value::permute (const Array<int>&, bool) const
{
  err_wrong_type_arg ("octave_base_value::permute ()", type_name ());
}

octave_value
octave_base_value::resize (const dim_vector&, bool) const
{
  err_wrong_type_arg ("octave_base_value::resize ()", type_name ());
}

MatrixType
octave_base_value::matrix_type (void) const
{
  err_wrong_type_arg ("octave_base_value::matrix_type ()", type_name ());
}

MatrixType
octave_base_value::matrix_type (const MatrixType&) const
{
  err_wrong_type_arg ("octave_base_value::matrix_type ()", type_name ());
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

  if (! force && isnumeric ())
    warn_implicit_conversion ("Octave:num-to-str",
                              type_name (), retval.type_name ());

  return retval;
}

octave_value
octave_base_value::convert_to_str_internal (bool, bool, char) const
{
  err_wrong_type_arg ("octave_base_value::convert_to_str_internal ()",
                      type_name ());
}

void
octave_base_value::convert_to_row_or_column_vector (void)
{
  err_wrong_type_arg
  ("octave_base_value::convert_to_row_or_column_vector ()", type_name ());
}

void
octave_base_value::print (std::ostream&, bool)
{
  err_wrong_type_arg ("octave_base_value::print ()", type_name ());
}

void
octave_base_value::print_raw (std::ostream&, bool) const
{
  err_wrong_type_arg ("octave_base_value::print_raw ()", type_name ());
}

bool
octave_base_value::print_name_tag (std::ostream& os,
                                   const std::string& name) const
{
  indent (os);

  if (print_as_scalar ())
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      if (! Vcompact_format)
        newline (os);

      return true;
    }

  return false;
}

void
octave_base_value::print_with_name (std::ostream& output_buf,
                                    const std::string& name,
                                    bool print_padding)
{
  bool pad_after = print_name_tag (output_buf, name);

  print (output_buf);

  if (print_padding && pad_after && ! Vcompact_format)
    newline (output_buf);
}

float_display_format
octave_base_value::get_edit_display_format (void) const
{
  return float_display_format ();
}

void
octave_base_value::print_info (std::ostream& os,
                               const std::string& /* prefix */) const
{
  os << "no info for type: " << type_name () << "\n";
}

#define INT_CONV_METHOD(T, F)                                           \
  T                                                                     \
  octave_base_value::F ## _value (bool require_int, bool frc_str_conv) const \
  {                                                                     \
    T retval = 0;                                                       \
                                                                        \
    double d = 0.0;                                                     \
                                                                        \
    try                                                                 \
      {                                                                 \
        d = double_value (frc_str_conv);                                \
      }                                                                 \
    catch (octave::execution_exception& ee)                               \
      {                                                                 \
        err_wrong_type_arg (ee, "octave_base_value::" #F "_value ()", type_name ()); \
      }                                                                 \
                                                                        \
    static const double out_of_range_top                                \
      = static_cast<double>(std::numeric_limits<T>::max ()) + 1.;       \
    if (require_int && octave::math::x_nint (d) != d)                   \
      error_with_cfn ("conversion of %g to " #T " value failed", d);    \
    else if (d < std::numeric_limits<T>::min ())                        \
      retval = std::numeric_limits<T>::min ();                          \
    else if (d >= out_of_range_top)                                     \
      retval = std::numeric_limits<T>::max ();                          \
    else                                                                \
      retval = static_cast<T> (octave::math::fix (d));                  \
                                                                        \
    return retval;                                                      \
  }

INT_CONV_METHOD (short int, short)
INT_CONV_METHOD (unsigned short int, ushort)

INT_CONV_METHOD (int, int)
INT_CONV_METHOD (unsigned int, uint)

INT_CONV_METHOD (long int, long)
INT_CONV_METHOD (unsigned long int, ulong)

INT_CONV_METHOD (int64_t, int64)
INT_CONV_METHOD (uint64_t, uint64)

int
octave_base_value::nint_value (bool frc_str_conv) const
{
  double d = 0.0;

  try
    {
      d = double_value (frc_str_conv);
    }
  catch (octave::execution_exception& ee)
    {
      err_wrong_type_arg (ee, "octave_base_value::nint_value ()", type_name ());
    }

  if (octave::math::isnan (d))
    error ("conversion of NaN to integer value failed");

  return static_cast<int> (octave::math::fix (d));
}

double
octave_base_value::double_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::double_value ()", type_name ());
}

float
octave_base_value::float_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_value ()", type_name ());
}

Cell
octave_base_value::cell_value () const
{
  err_wrong_type_arg ("octave_base_value::cell_value()", type_name ());
}

Matrix
octave_base_value::matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::matrix_value()", type_name ());
}

FloatMatrix
octave_base_value::float_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_matrix_value()", type_name ());
}

NDArray
octave_base_value::array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::array_value()", type_name ());
}

FloatNDArray
octave_base_value::float_array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_array_value()", type_name ());
}

Complex
octave_base_value::complex_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::complex_value()", type_name ());
}

FloatComplex
octave_base_value::float_complex_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_complex_value()", type_name ());
}

ComplexMatrix
octave_base_value::complex_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::complex_matrix_value()",
                      type_name ());
}

FloatComplexMatrix
octave_base_value::float_complex_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_complex_matrix_value()",
                      type_name ());
}

ComplexNDArray
octave_base_value::complex_array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::complex_array_value()", type_name ());
}

FloatComplexNDArray
octave_base_value::float_complex_array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_complex_array_value()",
                      type_name ());
}

bool
octave_base_value::bool_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::bool_value()", type_name ());
}

boolMatrix
octave_base_value::bool_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::bool_matrix_value()", type_name ());
}

boolNDArray
octave_base_value::bool_array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::bool_array_value()", type_name ());
}

charMatrix
octave_base_value::char_matrix_value (bool force) const
{
  octave_value tmp = convert_to_str (false, force);

  return tmp.char_matrix_value ();
}

charNDArray
octave_base_value::char_array_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::char_array_value()", type_name ());
}

SparseMatrix
octave_base_value::sparse_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::sparse_matrix_value()", type_name ());
}

SparseComplexMatrix
octave_base_value::sparse_complex_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::sparse_complex_matrix_value()",
                      type_name ());
}

SparseBoolMatrix
octave_base_value::sparse_bool_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::sparse_bool_matrix_value()",
                      type_name ());
}

DiagMatrix
octave_base_value::diag_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::diag_matrix_value()", type_name ());
}

FloatDiagMatrix
octave_base_value::float_diag_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_diag_matrix_value()",
                      type_name ());
}

ComplexDiagMatrix
octave_base_value::complex_diag_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::complex_diag_matrix_value()",
                      type_name ());
}

FloatComplexDiagMatrix
octave_base_value::float_complex_diag_matrix_value (bool) const
{
  err_wrong_type_arg ("octave_base_value::float_complex_diag_matrix_value()",
                      type_name ());
}

PermMatrix
octave_base_value::perm_matrix_value (void) const
{
  err_wrong_type_arg ("octave_base_value::perm_matrix_value()", type_name ());
}

octave_int8
octave_base_value::int8_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int8_scalar_value()", type_name ());
}

octave_int16
octave_base_value::int16_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int16_scalar_value()", type_name ());
}

octave_int32
octave_base_value::int32_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int32_scalar_value()", type_name ());
}

octave_int64
octave_base_value::int64_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int64_scalar_value()", type_name ());
}

octave_uint8
octave_base_value::uint8_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint8_scalar_value()", type_name ());
}

octave_uint16
octave_base_value::uint16_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint16_scalar_value()", type_name ());
}

octave_uint32
octave_base_value::uint32_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint32_scalar_value()", type_name ());
}

octave_uint64
octave_base_value::uint64_scalar_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint64_scalar_value()", type_name ());
}

int8NDArray
octave_base_value::int8_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int8_array_value()", type_name ());
}

int16NDArray
octave_base_value::int16_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int16_array_value()", type_name ());
}

int32NDArray
octave_base_value::int32_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int32_array_value()", type_name ());
}

int64NDArray
octave_base_value::int64_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int64_array_value()", type_name ());
}

uint8NDArray
octave_base_value::uint8_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint8_array_value()", type_name ());
}

uint16NDArray
octave_base_value::uint16_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint16_array_value()", type_name ());
}

uint32NDArray
octave_base_value::uint32_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint32_array_value()", type_name ());
}

uint64NDArray
octave_base_value::uint64_array_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint64_array_value()", type_name ());
}

string_vector
octave_base_value::string_vector_value (bool pad) const
{
  octave_value tmp = convert_to_str (pad, true);

  return tmp.string_vector_value ();
}

std::string
octave_base_value::string_value (bool force) const
{
  octave_value tmp = convert_to_str (force);

  return tmp.string_value ();
}

std::string
octave_base_value::xstring_value (void) const
{
  wrong_type_arg_error ();

  return std::string ();
}

Array<std::string>
octave_base_value::cellstr_value (void) const
{
  err_wrong_type_arg ("octave_base_value::cellstr_value()", type_name ());
}

octave::range<double>
octave_base_value::range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::range_value()", type_name ());
}

// For now, disable all but range<double>.

#if 0

octave::range<float>
octave_base_value::float_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::float_range_value()", type_name ());
}

octave::range<octave_int8>
octave_base_value::int8_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int8_range_value()", type_name ());
}

octave::range<octave_int16>
octave_base_value::int16_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int16_range_value()", type_name ());
}

octave::range<octave_int32>
octave_base_value::int32_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int32_range_value()", type_name ());
}

octave::range<octave_int64>
octave_base_value::int64_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::int64_range_value()", type_name ());
}

octave::range<octave_uint8>
octave_base_value::uint8_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint8_range_value()", type_name ());
}

octave::range<octave_uint16>
octave_base_value::uint16_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint16_range_value()", type_name ());
}

octave::range<octave_uint32>
octave_base_value::uint32_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint32_range_value()", type_name ());
}

octave::range<octave_uint64>
octave_base_value::uint64_range_value (void) const
{
  err_wrong_type_arg ("octave_base_value::uint64_range_value()", type_name ());
}

#endif

octave_map
octave_base_value::map_value (void) const
{
  err_wrong_type_arg ("octave_base_value::map_value()", type_name ());
}

octave_scalar_map
octave_base_value::scalar_map_value (void) const
{
  octave_map tmp = map_value ();

  if (tmp.numel () != 1)
    error ("invalid conversion of multi-dimensional struct to scalar struct");

  return octave_scalar_map (tmp.checkelem (0));
}

string_vector
octave_base_value::map_keys (void) const
{
  err_wrong_type_arg ("octave_base_value::map_keys()", type_name ());
}

bool
octave_base_value::isfield (const std::string&) const
{
  err_wrong_type_arg ("octave_base_value::isfield()", type_name ());
}

std::size_t
octave_base_value::nparents (void) const
{
  err_wrong_type_arg ("octave_base_value::nparents()", type_name ());
}

std::list<std::string>
octave_base_value::parent_class_name_list (void) const
{
  err_wrong_type_arg ("octave_base_value::parent_class_name_list()",
                      type_name ());
}

string_vector
octave_base_value::parent_class_names (void) const
{
  err_wrong_type_arg ("octave_base_value::parent_class_names()", type_name ());
}

octave_classdef *
octave_base_value::classdef_object_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::classdef_object_value()",
                        type_name ());

  return nullptr;
}

octave_function *
octave_base_value::function_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::function_value()", type_name ());

  return nullptr;
}

octave_user_function *
octave_base_value::user_function_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::user_function_value()",
                        type_name ());
  return nullptr;
}

octave_user_script *
octave_base_value::user_script_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::user_script_value()", type_name ());

  return nullptr;
}

octave_user_code *
octave_base_value::user_code_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::user_code_value()", type_name ());

  return nullptr;
}

octave_fcn_handle *
octave_base_value::fcn_handle_value (bool silent)
{
  if (! silent)
    err_wrong_type_arg ("octave_base_value::fcn_handle_value()", type_name ());

  return nullptr;
}

octave_value_list
octave_base_value::list_value (void) const
{
  err_wrong_type_arg ("octave_base_value::list_value()", type_name ());
}

bool
octave_base_value::save_ascii (std::ostream&)
{
  err_wrong_type_arg ("octave_base_value::save_ascii()", type_name ());
}

bool
octave_base_value::load_ascii (std::istream&)
{
  err_wrong_type_arg ("octave_base_value::load_ascii()", type_name ());
}

bool
octave_base_value::save_binary (std::ostream&, bool)
{
  err_wrong_type_arg ("octave_base_value::save_binary()", type_name ());
}

bool
octave_base_value::load_binary (std::istream&, bool,
                                octave::mach_info::float_format)
{
  err_wrong_type_arg ("octave_base_value::load_binary()", type_name ());
}

bool
octave_base_value::save_hdf5 (octave_hdf5_id, const char *, bool)
{
  err_wrong_type_arg ("octave_base_value::save_binary()", type_name ());
}

bool
octave_base_value::load_hdf5 (octave_hdf5_id, const char *)
{
  err_wrong_type_arg ("octave_base_value::load_binary()", type_name ());
}

int
octave_base_value::write (octave::stream&, int, oct_data_conv::data_type,
                          int, octave::mach_info::float_format) const
{
  err_wrong_type_arg ("octave_base_value::write()", type_name ());
}

mxArray *
octave_base_value::as_mxArray (bool) const
{
  return nullptr;
}

octave_value
octave_base_value::diag (octave_idx_type) const
{
  err_wrong_type_arg ("octave_base_value::diag ()", type_name ());
}

octave_value
octave_base_value::diag (octave_idx_type, octave_idx_type) const
{
  err_wrong_type_arg ("octave_base_value::diag ()", type_name ());
}

octave_value
octave_base_value::sort (octave_idx_type, sortmode) const
{
  err_wrong_type_arg ("octave_base_value::sort ()", type_name ());
}

octave_value
octave_base_value::sort (Array<octave_idx_type>&,
                         octave_idx_type, sortmode) const
{
  err_wrong_type_arg ("octave_base_value::sort ()", type_name ());
}

sortmode
octave_base_value::issorted (sortmode) const
{
  err_wrong_type_arg ("octave_base_value::issorted ()", type_name ());
}

Array<octave_idx_type>
octave_base_value::sort_rows_idx (sortmode) const
{
  err_wrong_type_arg ("octave_base_value::sort_rows_idx ()", type_name ());
}

sortmode
octave_base_value::is_sorted_rows (sortmode) const
{
  err_wrong_type_arg ("octave_base_value::is_sorted_rows ()", type_name ());
}

const char *
octave_base_value::get_umap_name (unary_mapper_t umap)
{
  static const char *names[num_unary_mappers] =
  {
    "abs",
    "acos",
    "acosh",
    "angle",
    "arg",
    "asin",
    "asinh",
    "atan",
    "atanh",
    "cbrt",
    "ceil",
    "conj",
    "cos",
    "cosh",
    "erf",
    "erfinv",
    "erfcinv",
    "erfc",
    "erfcx",
    "erfi",
    "dawson",
    "exp",
    "expm1",
    "isfinite",
    "fix",
    "floor",
    "gamma",
    "imag",
    "isinf",
    "isna",
    "isnan",
    "lgamma",
    "log",
    "log2",
    "log10",
    "log1p",
    "real",
    "round",
    "roundb",
    "signum",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "isalnum",
    "isalpha",
    "isascii",
    "iscntrl",
    "isdigit",
    "isgraph",
    "islower",
    "isprint",
    "ispunct",
    "isspace",
    "isupper",
    "isxdigit",
    "signbit",
    "tolower",
    "toupper"
  };

  if (umap < 0 || umap >= num_unary_mappers)
    return "unknown";
  else
    return names[umap];
}

void
octave_base_value::warn_load (const char *type) const
{
  warning_with_id
  ("Octave:load-save-unavailable",
   "%s: loading %s files not available in this version of Octave",
   t_name.c_str (), type);
}

void
octave_base_value::warn_save (const char *type) const
{
  warning_with_id
  ("Octave:load-save-unavailable",
   "%s: saving %s files not available in this version of Octave",
   t_name.c_str (), type);
}

void
octave_base_value::wrong_type_arg_error (void) const
{
  err_wrong_type_arg (type_name ());
}

octave_value
octave_base_value::map (unary_mapper_t umap) const
{
  error ("%s: not defined for %s", get_umap_name (umap), type_name ().c_str ());
}

void
octave_base_value::lock (void)
{
  err_wrong_type_arg ("octave_base_value::lock ()", type_name ());
}

void
octave_base_value::unlock (void)
{
  err_wrong_type_arg ("octave_base_value::unlock ()", type_name ());
}

octave_value
octave_base_value::dump (void) const
{
  std::map<std::string, octave_value> m
  = {{ "class", this->class_name () },
    { "type", this->type_name () },
    { "dims", this->dims().as_array () }
  };

  return octave_value (m);
}

OCTAVE_NORETURN static
void
err_indexed_assignment (const std::string& tn1, const std::string& tn2)
{
  error ("assignment of '%s' to indexed '%s' not implemented",
         tn2.c_str (), tn1.c_str ());
}

OCTAVE_NORETURN static
void
err_assign_conversion_failed (const std::string& tn1, const std::string& tn2)
{
  error ("type conversion for assignment of '%s' to indexed '%s' failed",
         tn2.c_str (), tn1.c_str ());
}

OCTAVE_NORETURN static
void
err_no_conversion (const std::string& on, const std::string& tn1,
                   const std::string& tn2)
{
  error ("operator %s: no conversion for assignment of '%s' to indexed '%s'",
         on.c_str (), tn2.c_str (), tn1.c_str ());
}

octave_value
octave_base_value::numeric_assign (const std::string& type,
                                   const std::list<octave_value_list>& idx,
                                   const octave_value& rhs)
{
  octave_value retval;

  if (idx.front ().empty ())
    error ("missing index in indexed assignment");

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  octave::type_info& ti = octave::__get_type_info__ ();

  octave::type_info::assign_op_fcn f
    = ti.lookup_assign_op (octave_value::op_asn_eq, t_lhs, t_rhs);

  bool done = false;

  if (f)
    {
      f (*this, idx.front (), rhs.get_rep ());

      done = true;
    }

  if (done)
    {
      count++;
      retval = octave_value (this);
    }
  else
    {
      int t_result = ti.lookup_pref_assign_conv (t_lhs, t_rhs);

      if (t_result >= 0)
        {
          octave_base_value::type_conv_fcn cf
            = ti.lookup_widening_op (t_lhs, t_result);

          if (! cf)
            err_indexed_assignment (type_name (), rhs.type_name ());

          octave_base_value *tmp = cf (*this);

          if (! tmp)
            err_assign_conversion_failed (type_name (), rhs.type_name ());

          octave_value val (tmp);

          retval = val.subsasgn (type, idx, rhs);

          done = true;
        }

      if (! done)
        {
          octave_value tmp_rhs;

          octave_base_value::type_conv_info cf_rhs
            = rhs.numeric_conversion_function ();

          octave_base_value::type_conv_info cf_this
            = numeric_conversion_function ();

          // Try biased (one-sided) conversions first.
          if (cf_rhs.type_id () >= 0
              && (ti.lookup_assign_op (octave_value::op_asn_eq,
                                       t_lhs, cf_rhs.type_id ())
                  || ti.lookup_pref_assign_conv (t_lhs,
                                                 cf_rhs.type_id ()) >= 0))
            cf_this = nullptr;
          else if (cf_this.type_id () >= 0
                   && (ti.lookup_assign_op (octave_value::op_asn_eq,
                                            cf_this.type_id (), t_rhs)
                       || ti.lookup_pref_assign_conv (cf_this.type_id (),
                           t_rhs) >= 0))
            cf_rhs = nullptr;

          if (cf_rhs)
            {
              octave_base_value *tmp = cf_rhs (rhs.get_rep ());

              if (! tmp)
                err_assign_conversion_failed (type_name (), rhs.type_name ());

              tmp_rhs = octave_value (tmp);
            }
          else
            tmp_rhs = rhs;

          count++;
          octave_value tmp_lhs = octave_value (this);

          if (cf_this)
            {
              octave_base_value *tmp = cf_this (*this);

              if (! tmp)
                err_assign_conversion_failed (type_name (), rhs.type_name ());

              tmp_lhs = octave_value (tmp);
            }

          if (! cf_this && ! cf_rhs)
            err_no_conversion (octave_value::assign_op_as_string
                               (octave_value::op_asn_eq),
                               type_name (), rhs.type_name ());

          retval = tmp_lhs.subsasgn (type, idx, tmp_rhs);

          done = true;
        }
    }

  // The assignment may have converted to a type that is wider than necessary.

  retval.maybe_mutate ();

  return retval;
}

// Current indentation.
int octave_base_value::s_curr_print_indent_level = 0;

// TRUE means we are at the beginning of a line.
bool octave_base_value::s_beginning_of_line = true;

// Each print() function should call this before printing anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
octave_base_value::indent (std::ostream& os) const
{
  panic_unless (s_curr_print_indent_level >= 0);

  if (s_beginning_of_line)
    {
      // FIXME: do we need this?
      // os << prefix;

      for (int i = 0; i < s_curr_print_indent_level; i++)
        os << ' ';

      s_beginning_of_line = false;
    }
}

// All print() functions should use this to print new lines.

void
octave_base_value::newline (std::ostream& os) const
{
  os << "\n";

  s_beginning_of_line = true;
}

// For resetting print state.

void
octave_base_value::reset (void) const
{
  s_beginning_of_line = true;
  s_curr_print_indent_level = 0;
}

octave_value
octave_base_value::fast_elem_extract (octave_idx_type) const
{
  return octave_value ();
}

bool
octave_base_value::fast_elem_insert (octave_idx_type, const octave_value&)
{
  return false;
}

bool
octave_base_value::fast_elem_insert_self (void *, builtin_type_t) const
{
  return false;
}

static octave_base_value *
oct_conv_matrix_conv (const octave_base_value&)
{
  return new octave_matrix ();
}

static octave_base_value *
oct_conv_complex_matrix_conv (const octave_base_value&)
{
  return new octave_complex_matrix ();
}

static octave_base_value *
oct_conv_string_conv (const octave_base_value&)
{
  return new octave_char_matrix_str ();
}

static octave_base_value *
oct_conv_cell_conv (const octave_base_value&)
{
  return new octave_cell ();
}

static inline octave_value_list
sanitize (const octave_value_list& ovl)
{
  octave_value_list retval = ovl;

  for (octave_idx_type i = 0; i < ovl.length (); i++)
    {
      if (retval(i).is_magic_colon ())
        retval(i) = ":";
    }

  return retval;
}

octave_value
make_idx_args (const std::string& type,
               const std::list<octave_value_list>& idx,
               const std::string& who)
{
  std::size_t len = type.length ();

  if (len != idx.size ())
    error ("invalid index for %s", who.c_str ());

  Cell type_field (1, len);
  Cell subs_field (1, len);

  auto p = idx.begin ();

  for (std::size_t i = 0; i < len; i++)
    {
      char t = type[i];

      switch (t)
        {
        case '(':
          type_field(i) = "()";
          subs_field(i) = Cell (sanitize (*p++));
          break;

        case '{':
          type_field(i) = "{}";
          subs_field(i) = Cell (sanitize (*p++));
          break;

        case '.':
          {
            type_field(i) = ".";

            octave_value_list vlist = *p++;

            if (vlist.length () != 1)
              error ("only single argument permitted for '.' index");

            octave_value val = vlist(0);

            if (! val.is_string ())
              error ("string argument required for '.' index");

            subs_field(i) = val;
          }
          break;

        default:
          panic_impossible ();
          break;
        }
    }

  octave_map m;

  m.assign ("type", type_field);
  m.assign ("subs", subs_field);

  return m;
}

bool
called_from_builtin (void)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ();

  octave_function *fcn = tw.caller_function ();

  // FIXME: we probably need a better check here, or some other
  // mechanism to avoid overloaded functions when builtin is used.
  // For example, what if someone overloads the builtin function?
  // Also, are there other places where using builtin is not properly
  // avoiding dispatch?

  return (fcn && fcn->name () == "builtin");
}

OCTAVE_BEGIN_NAMESPACE(octave)

void
install_base_type_conversions (octave::type_info& ti)
{
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_matrix, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_complex,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_complex_matrix,
                         octave_complex_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_range, octave_matrix);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_char_matrix_str,
                         octave_char_matrix_str);
  INSTALL_ASSIGNCONV_TI (ti, octave_base_value, octave_cell, octave_cell);

  INSTALL_WIDENOP_TI (ti, octave_base_value, octave_matrix, matrix_conv);
  INSTALL_WIDENOP_TI (ti, octave_base_value, octave_complex_matrix,
                      complex_matrix_conv);
  INSTALL_WIDENOP_TI (ti, octave_base_value, octave_char_matrix_str, string_conv);
  INSTALL_WIDENOP_TI (ti, octave_base_value, octave_cell, cell_conv);
}

OCTAVE_END_NAMESPACE(octave)
