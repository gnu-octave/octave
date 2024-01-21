////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if ! defined (octave_ov_base_h)
#define octave_ov_base_h 1

#include "octave-config.h"

#include <cstdlib>

#include <iosfwd>
#include <list>
#include <memory>
#include <string>

#include "Range.h"
#include "data-conv.h"
#include "mx-base.h"
#include "str-vec.h"

#include "auto-shlib.h"
#include "oct-hdf5-types.h"
#include "oct-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class stack_frame;
class type_info;

// FIXME: This is not ideal, but it avoids including
// interpreter-private.h here and bringing in a lot of unnecessary
// symbols that require even more header files.  Since the typeinfo
// object is required to load a user-defined octave_value object,
// maybe this function should be declared in a public header file?

extern OCTINTERP_API type_info& __get_type_info__ ();

// For now just preserve the old interface and don't mark it as deprecated.
// This function is currently an internal, private function.  Additional
// changes may be made before version 8 is finally released.
inline type_info& __get_type_info__ (const std::string&) { return __get_type_info__ (); }

OCTAVE_END_NAMESPACE(octave)

class Cell;
class float_display_format;
class mxArray;
class octave_map;
class octave_scalar_map;
class octave_value;
class octave_value_list;
class octave_classdef;
class octave_function;
class octave_user_function;
class octave_user_script;
class octave_user_code;
class octave_fcn_handle;
class octave_value_list;

enum builtin_type_t
{
  btyp_double,
  btyp_float,
  btyp_complex,
  btyp_float_complex,
  btyp_int8,
  btyp_int16,
  btyp_int32,
  btyp_int64,
  btyp_uint8,
  btyp_uint16,
  btyp_uint32,
  btyp_uint64,
  btyp_bool,
  btyp_char,
  btyp_struct,
  btyp_cell,
  btyp_func_handle,
  btyp_unknown,
  btyp_num_types = btyp_unknown
};

extern OCTINTERP_API std::string btyp_class_name [];

inline bool btyp_isnumeric (builtin_type_t btyp)
{ return btyp <= btyp_uint64; }

inline bool btyp_isinteger (builtin_type_t btyp)
{ return btyp >= btyp_int8 && btyp <= btyp_uint64; }

inline bool btyp_isfloat (builtin_type_t btyp)
{ return btyp <= btyp_float_complex; }

inline bool btyp_isarray (builtin_type_t btyp)
{ return btyp <= btyp_char; }

//! Determine the resulting type for a possible mixed-type operation.
//!
//! Rules for the resulting type:
//!   - bool -> double
//!   - single + double -> single
//!   - real + complex -> complex
//!   - integer + real -> integer
//!   - uint + uint -> uint (the bigger one)
//!   - sint + sint -> sint (the bigger one)
//!
//! @return The resulting type or "unknown type", if the resulting type cannot
//!         be determined.

extern OCTINTERP_API
builtin_type_t btyp_mixed_numeric (builtin_type_t x, builtin_type_t y);

template <typename T>
struct class_to_btyp
{
  static const builtin_type_t btyp = btyp_unknown;
};

template <builtin_type_t BTYP>
struct btyp_to_class
{
  typedef void type;
};

#define DEF_BTYP_TRAITS(BTYP, CLASS)            \
  template <>                                   \
  struct class_to_btyp<CLASS>                   \
  {                                             \
    static const builtin_type_t btyp = BTYP;    \
  };                                            \
                                                \
  template <>                                   \
  struct btyp_to_class<BTYP>                    \
  {                                             \
    typedef CLASS type;                         \
  }

DEF_BTYP_TRAITS (btyp_double, double);
DEF_BTYP_TRAITS (btyp_float, float);
DEF_BTYP_TRAITS (btyp_complex, Complex);
DEF_BTYP_TRAITS (btyp_float_complex, FloatComplex);
DEF_BTYP_TRAITS (btyp_int8, octave_int8);
DEF_BTYP_TRAITS (btyp_int16, octave_int16);
DEF_BTYP_TRAITS (btyp_int32, octave_int32);
DEF_BTYP_TRAITS (btyp_int64, octave_int64);
DEF_BTYP_TRAITS (btyp_uint8, octave_uint8);
DEF_BTYP_TRAITS (btyp_uint16, octave_uint16);
DEF_BTYP_TRAITS (btyp_uint32, octave_uint32);
DEF_BTYP_TRAITS (btyp_uint64, octave_uint64);
DEF_BTYP_TRAITS (btyp_bool, bool);
DEF_BTYP_TRAITS (btyp_char, char);


// T_ID is the type id of struct objects, set by register_type().
// T_NAME is the type name of struct objects.

#define OCTAVE_EMPTY_CPP_ARG /* empty */

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA                          \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2 (OCTAVE_EMPTY_CPP_ARG)

#define DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA                     \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(virtual)

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(VIRTUAL)                \
  public:                                                             \
    VIRTUAL int type_id () const { return s_t_id; }                   \
    VIRTUAL std::string type_name () const { return s_t_name; }       \
    VIRTUAL std::string class_name () const { return s_c_name; }      \
    static int static_type_id () { return s_t_id; }                   \
    static std::string static_type_name () { return s_t_name; }       \
    static std::string static_class_name () { return s_c_name; }      \
    OCTINTERP_API static void register_type ();                       \
    OCTINTERP_API static void register_type (octave::type_info&);     \
                                                                      \
  private:                                                            \
    static OCTINTERP_API int s_t_id;                                  \
    static OCTINTERP_API const std::string s_t_name;                  \
    static OCTINTERP_API const std::string s_c_name;

#define DECLARE_TEMPLATE_OV_TYPEID_SPECIALIZATIONS(cls, type)         \
  template <> OCTINTERP_API void cls<type>::register_type ();         \
  template <> OCTINTERP_API void                                      \
  OCTINTERP_API cls<type>::register_type (octave::type_info&);        \
  template <> OCTINTERP_API int cls<type>::s_t_id;                    \
  template <> OCTINTERP_API const std::string cls<type>::s_t_name;    \
  template <> OCTINTERP_API const std::string cls<type>::s_c_name;

// FIXME: The 'new' operator below creates an 8-byte memory leak for every
// registered data type (of which there are 58 built-in to Octave, plus any
// user-defined data types).  The problem is user-defined types creating
// a crash on exit (see bug #53156).  See also the FIXME note in function
// register_type() in ov-typeinfo.cc.

#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA_INTERNAL(tspec, t, n, c)  \
  tspec int t::s_t_id (-1);                                           \
  tspec const std::string t::s_t_name (n);                            \
  tspec const std::string t::s_c_name (c);                            \
  tspec void t::register_type ()                                      \
  {                                                                   \
    octave::type_info& type_info = octave::__get_type_info__ ();      \
                                                                      \
    register_type (type_info);                                        \
  }                                                                   \
  tspec void t::register_type (octave::type_info& ti)                 \
  {                                                                   \
    octave_value v = (new t ());                                      \
    s_t_id = ti.register_type (t::s_t_name, t::s_c_name, v);          \
  }

#define DEFINE_TEMPLATE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n, c)         \
  DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA_INTERNAL (template <>, t, n, c)

#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n, c)                  \
  DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA_INTERNAL ( , t, n, c)

// A base value type, so that derived types only have to redefine what
// they need (if they are derived from octave_base_value instead of
// octave_value).

class OCTINTERP_API octave_base_value
{
public:

  typedef octave_base_value *(*type_conv_fcn) (const octave_base_value&);

  // type conversion, including result type information
  class type_conv_info
  {
  public:
    type_conv_info (type_conv_fcn f = nullptr, int t = -1)
      : m_fcn (f), m_type_id (t) { }

    operator type_conv_fcn () const { return m_fcn; }

    octave_base_value * operator () (const octave_base_value& v) const
    { return (*m_fcn) (v); }

    int type_id () const { return m_type_id; }

  private:
    type_conv_fcn m_fcn;
    int m_type_id;
  };

  friend class octave_value;

  OCTINTERP_API octave_base_value ();

  octave_base_value (const octave_base_value&) : octave_base_value () { }

  virtual ~octave_base_value () = default;

  // Unconditional clone.  Always clones.
  virtual octave_base_value *
  clone () const { return new octave_base_value (*this); }

  // Empty clone.
  virtual OCTINTERP_API octave_base_value *
  empty_clone () const;

  // Unique clone.  Usually clones, but may be overridden to fake the
  // cloning when sharing copies is to be controlled from within an
  // instance (see octave_class).
  virtual octave_base_value *
  unique_clone () { return clone (); }

  virtual void break_closure_cycles (const std::shared_ptr<octave::stack_frame>&) { }

  virtual type_conv_info
  numeric_conversion_function () const
  { return type_conv_info (); }

  virtual type_conv_info
  numeric_demotion_function () const
  { return type_conv_info (); }

  virtual OCTINTERP_API octave_value squeeze () const;

  virtual OCTINTERP_API octave_value full_value () const;

  // Will return a copy of it-self when the representation
  // allready is a scalar (.i.e. double). The const variant
  // as_double () would allocate a new octave value.
  virtual OCTINTERP_API octave_value as_double_or_copy ();

  virtual OCTINTERP_API octave_value as_double () const;
  virtual OCTINTERP_API octave_value as_single () const;

  virtual OCTINTERP_API octave_value as_int8 () const;
  virtual OCTINTERP_API octave_value as_int16 () const;
  virtual OCTINTERP_API octave_value as_int32 () const;
  virtual OCTINTERP_API octave_value as_int64 () const;

  virtual OCTINTERP_API octave_value as_uint8 () const;
  virtual OCTINTERP_API octave_value as_uint16 () const;
  virtual OCTINTERP_API octave_value as_uint32 () const;
  virtual OCTINTERP_API octave_value as_uint64 () const;

  virtual octave_base_value * try_narrowing_conversion ()
  { return nullptr; }

  virtual void maybe_economize () { }

  virtual OCTINTERP_API Matrix size ();

  virtual OCTINTERP_API octave_idx_type xnumel (const octave_value_list&);

  // FIXME: Do we really need all three of these versions of subsref?

  virtual OCTINTERP_API octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx);

  virtual OCTINTERP_API octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout);

  virtual OCTINTERP_API octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           bool auto_add);

  virtual OCTINTERP_API octave_value_list
  simple_subsref (char type, octave_value_list& idx, int nargout);

  virtual OCTINTERP_API octave_value
  do_index_op (const octave_value_list& idx, bool resize_ok = false);

  virtual void assign (const std::string&, const octave_value&) { }

  virtual OCTINTERP_API octave_value
  subsasgn (const std::string& type,
            const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  virtual OCTINTERP_API octave_value
  simple_subsasgn (char type, octave_value_list& idx,
                   const octave_value& rhs);

  virtual OCTINTERP_API octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  virtual OCTINTERP_API octave::idx_vector
  index_vector (bool require_integers = false) const;

  virtual dim_vector dims () const { return dim_vector (); }

  octave_idx_type rows () const
  {
    const dim_vector dv = dims ();

    return dv(0);
  }

  octave_idx_type columns () const
  {
    const dim_vector dv = dims ();

    return dv(1);
  }

  virtual int ndims () const
  { return dims ().ndims (); }

  virtual octave_idx_type numel () const { return dims ().numel (); }

  virtual std::size_t byte_size () const { return 0; }

  virtual OCTINTERP_API octave_idx_type nnz () const;

  virtual OCTINTERP_API octave_idx_type nzmax () const;

  virtual OCTINTERP_API octave_idx_type nfields () const;

  virtual OCTINTERP_API octave_value reshape (const dim_vector&) const;

  virtual OCTINTERP_API octave_value
  permute (const Array<int>& vec, bool = false) const;

  virtual OCTINTERP_API octave_value
  resize (const dim_vector&, bool fill = false) const;

  virtual OCTINTERP_API MatrixType matrix_type () const;

  virtual OCTINTERP_API MatrixType matrix_type (const MatrixType& typ) const;

  virtual bool is_defined () const { return false; }

  virtual bool is_storable () const { return true; }

  virtual bool is_legacy_object () const { return false; }

  bool isempty () const { return (dims ().any_zero ()); }

  bool is_zero_by_zero () const { return dims().zero_by_zero (); }

  virtual bool iscell () const { return false; }

  virtual bool iscellstr () const { return false; }

  virtual bool is_real_scalar () const { return false; }

  virtual bool is_real_matrix () const { return false; }

  virtual bool is_complex_scalar () const { return false; }

  virtual bool is_complex_matrix () const { return false; }

  virtual bool is_bool_scalar () const { return false; }

  virtual bool is_bool_matrix () const { return false; }

  virtual bool is_char_matrix () const { return false; }

  virtual bool is_diag_matrix () const { return false; }

  virtual bool is_perm_matrix () const { return false; }

  virtual bool is_string () const { return false; }

  virtual bool is_sq_string () const { return false; }

  virtual bool is_range () const { return false; }

  virtual bool isstruct () const { return false; }

  virtual bool isobject () const { return false; }

  virtual bool is_classdef_meta () const { return false; }

  virtual bool is_classdef_superclass_ref () const { return false; }

  virtual bool is_classdef_object () const { return false; }

  virtual bool is_package () const { return false; }

  virtual bool isjava () const { return false; }

  virtual bool is_cs_list () const { return false; }

  virtual bool is_magic_colon () const { return false; }

  virtual bool is_all_va_args () const { return false; }

  virtual OCTINTERP_API octave_value all (int = 0) const;

  virtual OCTINTERP_API octave_value any (int = 0) const;

  virtual builtin_type_t builtin_type () const { return btyp_unknown; }

  virtual bool is_double_type () const { return false; }

  virtual bool is_single_type () const { return false; }

  virtual bool isfloat () const { return false; }

  virtual bool is_int8_type () const { return false; }

  virtual bool is_int16_type () const { return false; }

  virtual bool is_int32_type () const { return false; }

  virtual bool is_int64_type () const { return false; }

  virtual bool is_uint8_type () const { return false; }

  virtual bool is_uint16_type () const { return false; }

  virtual bool is_uint32_type () const { return false; }

  virtual bool is_uint64_type () const { return false; }

  virtual bool islogical () const { return false; }

  virtual bool isinteger () const { return false; }

  virtual bool isreal () const { return false; }

  virtual bool iscomplex () const { return false; }

  // Would be nice to get rid of the next four functions:

  virtual bool is_scalar_type () const { return false; }

  virtual bool is_matrix_type () const { return false; }

  virtual bool is_full_num_matrix () const { return false; }

  virtual bool isnumeric () const { return false; }

  virtual bool issparse () const { return false; }

  virtual bool is_true () const { return false; }

  virtual bool is_magic_int () const { return false; }

  virtual bool isnull () const { return false; }

  virtual bool is_constant () const { return false; }

  virtual bool is_function_handle () const { return false; }

  virtual bool is_anonymous_function () const { return false; }

  virtual bool is_inline_function () const { return false; }

  virtual bool is_function () const { return false; }

  virtual bool is_user_script () const { return false; }

  virtual bool is_user_function () const { return false; }

  virtual bool is_user_code () const { return false; }

  virtual bool is_builtin_function () const { return false; }

  virtual bool is_dld_function () const { return false; }

  virtual bool is_mex_function () const { return false; }

  virtual bool is_function_cache () const { return false; }

  // Checks if the ov could be a function. If it is undefined,
  // the name associated with the ov could be a function to call.
  virtual bool is_maybe_function () const
  { return !is_defined () || is_function (); }

  virtual bool has_function_cache () const { return false; }

  virtual octave_function * get_cached_fcn (const octave_value_list&) { return nullptr; }

  virtual octave_function * get_cached_fcn (void*, void*) { return nullptr; }

  virtual void erase_subfunctions () { }

  virtual OCTINTERP_API short int
  short_value (bool = false, bool = false) const;

  virtual OCTINTERP_API unsigned short int
  ushort_value (bool = false, bool = false) const;

  virtual OCTINTERP_API int
  int_value (bool = false, bool = false) const;

  virtual OCTINTERP_API unsigned int
  uint_value (bool = false, bool = false) const;

  virtual OCTINTERP_API int nint_value (bool = false) const;

  virtual OCTINTERP_API long int
  long_value (bool = false, bool = false) const;

  virtual OCTINTERP_API unsigned long int
  ulong_value (bool = false, bool = false) const;

  virtual OCTINTERP_API int64_t int64_value (bool = false, bool = false) const;

  virtual OCTINTERP_API uint64_t uint64_value (bool = false, bool = false) const;

  virtual OCTINTERP_API double double_value (bool = false) const;

  virtual OCTINTERP_API float float_value (bool = false) const;

  virtual double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  virtual float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  virtual OCTINTERP_API Cell cell_value () const;

  virtual OCTINTERP_API Matrix matrix_value (bool = false) const;

  virtual OCTINTERP_API FloatMatrix float_matrix_value (bool = false) const;

  virtual OCTINTERP_API NDArray array_value (bool = false) const;

  virtual OCTINTERP_API FloatNDArray float_array_value (bool = false) const;

  virtual OCTINTERP_API Complex complex_value (bool = false) const;

  virtual OCTINTERP_API FloatComplex float_complex_value (bool = false) const;

  virtual OCTINTERP_API ComplexMatrix complex_matrix_value (bool = false) const;

  virtual OCTINTERP_API FloatComplexMatrix float_complex_matrix_value (bool = false) const;

  virtual OCTINTERP_API ComplexNDArray complex_array_value (bool = false) const;

  virtual OCTINTERP_API FloatComplexNDArray float_complex_array_value (bool = false) const;

  virtual OCTINTERP_API bool bool_value (bool = false) const;

  virtual OCTINTERP_API boolMatrix bool_matrix_value (bool = false) const;

  virtual OCTINTERP_API boolNDArray bool_array_value (bool = false) const;

  virtual OCTINTERP_API charMatrix char_matrix_value (bool force = false) const;

  virtual OCTINTERP_API charNDArray char_array_value (bool = false) const;

  virtual OCTINTERP_API SparseMatrix sparse_matrix_value (bool = false) const;

  virtual OCTINTERP_API SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  virtual OCTINTERP_API SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  virtual OCTINTERP_API DiagMatrix diag_matrix_value (bool = false) const;

  virtual OCTINTERP_API FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  virtual OCTINTERP_API ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;

  virtual OCTINTERP_API FloatComplexDiagMatrix
  float_complex_diag_matrix_value (bool = false) const;

  virtual OCTINTERP_API PermMatrix perm_matrix_value () const;

  virtual OCTINTERP_API octave_int8 int8_scalar_value () const;

  virtual OCTINTERP_API octave_int16 int16_scalar_value () const;

  virtual OCTINTERP_API octave_int32 int32_scalar_value () const;

  virtual OCTINTERP_API octave_int64 int64_scalar_value () const;

  virtual OCTINTERP_API octave_uint8 uint8_scalar_value () const;

  virtual OCTINTERP_API octave_uint16 uint16_scalar_value () const;

  virtual OCTINTERP_API octave_uint32 uint32_scalar_value () const;

  virtual OCTINTERP_API octave_uint64 uint64_scalar_value () const;

  virtual OCTINTERP_API int8NDArray int8_array_value () const;

  virtual OCTINTERP_API int16NDArray int16_array_value () const;

  virtual OCTINTERP_API int32NDArray int32_array_value () const;

  virtual OCTINTERP_API int64NDArray int64_array_value () const;

  virtual OCTINTERP_API uint8NDArray uint8_array_value () const;

  virtual OCTINTERP_API uint16NDArray uint16_array_value () const;

  virtual OCTINTERP_API uint32NDArray uint32_array_value () const;

  virtual OCTINTERP_API uint64NDArray uint64_array_value () const;

  virtual OCTINTERP_API string_vector string_vector_value (bool pad = false) const;

  virtual OCTINTERP_API std::string string_value (bool force = false) const;

  virtual OCTINTERP_API Array<std::string> cellstr_value () const;

  virtual OCTINTERP_API octave::range<double> range_value () const;

  // For now, enable only range<double>.

  virtual OCTINTERP_API octave_map map_value () const;

  virtual OCTINTERP_API octave_scalar_map scalar_map_value () const;

  virtual OCTINTERP_API string_vector map_keys () const;

  virtual OCTINTERP_API bool isfield (const std::string&) const;

  virtual OCTINTERP_API std::size_t nparents () const;

  virtual OCTINTERP_API std::list<std::string> parent_class_name_list () const;

  virtual OCTINTERP_API string_vector parent_class_names () const;

  virtual octave_base_value * find_parent_class (const std::string&)
  { return nullptr; }

  virtual octave_base_value * unique_parent_class (const std::string&)
  { return nullptr; }

  virtual bool is_instance_of (const std::string&) const
  { return false; }

  virtual OCTINTERP_API octave_classdef * classdef_object_value (bool silent = false);

  virtual OCTINTERP_API octave_function * function_value (bool silent = false);

  virtual OCTINTERP_API octave_user_function * user_function_value (bool silent = false);

  virtual OCTINTERP_API octave_user_script * user_script_value (bool silent = false);

  virtual OCTINTERP_API octave_user_code * user_code_value (bool silent = false);

  virtual OCTINTERP_API octave_fcn_handle * fcn_handle_value (bool silent = false);

  virtual OCTINTERP_API octave_value_list list_value () const;

  virtual OCTINTERP_API octave_value
  convert_to_str (bool pad = false, bool force = false, char type = '\'') const;
  virtual OCTINTERP_API octave_value
  convert_to_str_internal (bool pad, bool force, char type) const;

  virtual OCTINTERP_API void convert_to_row_or_column_vector ();

  // The following extractor functions don't perform any implicit type
  // conversions.

  virtual OCTINTERP_API std::string xstring_value () const;

  virtual bool print_as_scalar () const { return false; }

  virtual OCTINTERP_API void
  print (std::ostream& os, bool pr_as_read_syntax = false);

  virtual OCTINTERP_API void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual OCTINTERP_API bool
  print_name_tag (std::ostream& os, const std::string& name) const;

  virtual OCTINTERP_API void
  print_with_name (std::ostream& output_buf, const std::string& name,
                   bool print_padding = true);

  virtual void short_disp (std::ostream& os) const { os << "..."; }

  virtual OCTINTERP_API float_display_format get_edit_display_format () const;

  virtual std::string edit_display (const float_display_format&,
                                    octave_idx_type, octave_idx_type) const
  { return "#VAL"; }

  virtual OCTINTERP_API void
  print_info (std::ostream& os, const std::string& prefix) const;

  virtual OCTINTERP_API bool save_ascii (std::ostream& os);

  virtual OCTINTERP_API bool load_ascii (std::istream& is);

  virtual OCTINTERP_API bool save_binary (std::ostream& os, bool save_as_floats);

  virtual OCTINTERP_API bool
  load_binary (std::istream& is, bool swap, octave::mach_info::float_format fmt);

  virtual OCTINTERP_API bool
  save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  virtual OCTINTERP_API bool
  load_hdf5 (octave_hdf5_id loc_id, const char *name);

  virtual OCTINTERP_API int
  write (octave::stream& os, int block_size,
         oct_data_conv::data_type output_type, int skip,
         octave::mach_info::float_format flt_fmt) const;

  virtual const void * mex_get_data () const { return nullptr; }

  virtual const octave_idx_type * mex_get_ir () const { return nullptr; }

  virtual const octave_idx_type * mex_get_jc () const { return nullptr; }

  virtual OCTINTERP_API mxArray * as_mxArray (bool interleaved) const;

  virtual OCTINTERP_API octave_value diag (octave_idx_type k = 0) const;

  virtual OCTINTERP_API octave_value diag (octave_idx_type m, octave_idx_type n) const;

  virtual OCTINTERP_API octave_value
  sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;

  virtual OCTINTERP_API octave_value
  sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
        sortmode mode = ASCENDING) const;

  virtual OCTINTERP_API sortmode issorted (sortmode mode = UNSORTED) const;

  virtual OCTINTERP_API Array<octave_idx_type>
  sort_rows_idx (sortmode mode = ASCENDING) const;

  virtual OCTINTERP_API sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  virtual OCTINTERP_API void lock ();

  virtual OCTINTERP_API void unlock ();

  virtual bool islocked () const { return false; }

  virtual void call_object_destructor () { }

  virtual void maybe_call_dtor () { }

  virtual OCTINTERP_API octave_value dump () const;

  virtual OCTINTERP_API octave_value storable_value ();

  virtual OCTINTERP_API octave_base_value * make_storable_value ();

  // Standard mappers.  Register new ones here.
  enum unary_mapper_t
  {
    umap_abs,
    umap_acos,
    umap_acosh,
    umap_angle,
    umap_arg,
    umap_asin,
    umap_asinh,
    umap_atan,
    umap_atanh,
    umap_cbrt,
    umap_ceil,
    umap_conj,
    umap_cos,
    umap_cosh,
    umap_erf,
    umap_erfinv,
    umap_erfcinv,
    umap_erfc,
    umap_erfcx,
    umap_erfi,
    umap_dawson,
    umap_exp,
    umap_expm1,
    umap_isfinite,
    umap_fix,
    umap_floor,
    umap_gamma,
    umap_imag,
    umap_isinf,
    umap_isna,
    umap_isnan,
    umap_lgamma,
    umap_log,
    umap_log2,
    umap_log10,
    umap_log1p,
    umap_real,
    umap_round,
    umap_roundb,
    umap_signum,
    umap_sin,
    umap_sinh,
    umap_sqrt,
    umap_tan,
    umap_tanh,
    umap_xisalnum,
    umap_xisalpha,
    umap_xisascii,
    umap_xiscntrl,
    umap_xisdigit,
    umap_xisgraph,
    umap_xislower,
    umap_xisprint,
    umap_xispunct,
    umap_xisspace,
    umap_xisupper,
    umap_xisxdigit,
    umap_xsignbit,
    umap_xtolower,
    umap_xtoupper,
    umap_unknown,
    num_unary_mappers = umap_unknown
  };

  virtual OCTINTERP_API octave_value map (unary_mapper_t) const;

  // These are fast indexing & assignment shortcuts for extracting
  // or inserting a single scalar from/to an array.

  // Extract the n-th element, aka val(n).  Result is undefined if val is not
  // an array type or n is out of range.  Never error.
  virtual OCTINTERP_API octave_value
  fast_elem_extract (octave_idx_type n) const;

  // Assign the n-th element, aka val(n) = x.  Returns false if val is not an
  // array type, x is not a matching scalar type, or n is out of range.
  // Never error.
  virtual OCTINTERP_API bool
  fast_elem_insert (octave_idx_type n, const octave_value& x);

  // This is a helper for the above, to be overridden in scalar types.  The
  // whole point is to handle the insertion efficiently with just *two* VM
  // calls, which is basically the theoretical minimum.
  virtual OCTINTERP_API bool
  fast_elem_insert_self (void *where, builtin_type_t btyp) const;

protected:

  // This should only be called for derived types.

  OCTINTERP_API octave_value
  numeric_assign (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  void reset_indent_level () const
  { s_curr_print_indent_level = 0; }

  void increment_indent_level () const
  { s_curr_print_indent_level += 2; }

  void decrement_indent_level () const
  { s_curr_print_indent_level -= 2; }

  int current_print_indent_level () const
  { return s_curr_print_indent_level; }

  OCTINTERP_API void indent (std::ostream& os) const;

  OCTINTERP_API void newline (std::ostream& os) const;

  OCTINTERP_API void reset () const;

  // A reference count.
  // NOTE: the declaration is octave_idx_type because with 64-bit indexing,
  // it is well possible to have more than MAX_INT copies of a single value
  // (think of an empty cell array with >2G elements).
  octave::refcount<octave_idx_type> m_count;

  // FIXME: Create an alias "count" to the real member variable m_count.
  // This name is deprecated in Octave 9 and will be removed in Octave 11.
  OCTAVE_DEPRECATED (9, "use octave_base_value::m_count instead")
  octave::refcount<octave_idx_type>& count;

  OCTINTERP_API static const char * get_umap_name (unary_mapper_t);

  OCTINTERP_API void warn_load (const char *type) const;
  OCTINTERP_API void warn_save (const char *type) const;

private:

  OCTINTERP_API void wrong_type_arg_error () const;

  //--------

  static int s_curr_print_indent_level;
  static bool s_beginning_of_line;

  DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA
};

class OCTINTERP_TEMPLATE_API octave_base_dld_value : public octave_base_value
{
public:

  octave_base_dld_value () = default;

  ~octave_base_dld_value ()
  {
    m_containing_dynamic_library.delete_later ();
  }

  octave_base_dld_value (const octave_base_dld_value&) = default;

private:

  octave::auto_shlib m_containing_dynamic_library;
};

// Utility function to convert C++ arguments used in subsref/subsasgn into an
// octave_value_list object that can be used to call a function/method in the
// interpreter.
extern OCTINTERP_API octave_value
make_idx_args (const std::string& type,
               const std::list<octave_value_list>& idx,
               const std::string& who);

// Tells whether some regular octave_value_base methods are being called from
// within the "builtin" function.
extern OCTINTERP_API bool called_from_builtin ();

#endif
