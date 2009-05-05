/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_base_value_h)
#define octave_base_value_h 1

#include <cstdlib>

#include <iosfwd>
#include <list>
#include <string>

#include "Range.h"
#include "data-conv.h"
#include "mxarray.h"
#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-hdf5.h"

class Cell;
class Octave_map;
class octave_value;
class octave_value_list;
class octave_stream;
class octave_function;
class octave_user_function;
class octave_user_script;
class octave_user_code;
class octave_fcn_handle;
class octave_fcn_inline;
class octave_value_list;
class octave_lvalue;

class tree_walker;

// T_ID is the type id of struct objects, set by register_type().
// T_NAME is the type name of struct objects.

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2 (OCTAVE_EMPTY_CPP_ARG)

#define DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(virtual)

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(VIRTUAL) \
  public: \
    VIRTUAL int type_id (void) const { return t_id; } \
    VIRTUAL std::string type_name (void) const { return t_name; } \
    VIRTUAL std::string class_name (void) const { return c_name; } \
    static int static_type_id (void) { return t_id; } \
    static std::string static_type_name (void) { return t_name; } \
    static std::string static_class_name (void) { return c_name; } \
    static void register_type (void); \
 \
  private: \
    static int t_id; \
    static const std::string t_name; \
    static const std::string c_name;


#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n, c) \
  int t::t_id (-1); \
  const std::string t::t_name (n); \
  const std::string t::c_name (c); \
  void t::register_type (void) \
    { \
      t_id = octave_value_typeinfo::register_type (t::t_name, \
						   t::c_name, \
						   octave_value (new t ())); \
    }

// A base value type, so that derived types only have to redefine what
// they need (if they are derived from octave_base_value instead of
// octave_value).

class
OCTINTERP_API
octave_base_value
{
public:

  typedef octave_base_value * (*type_conv_fcn) (const octave_base_value&);

  // type conversion, including result type information
  class type_conv_info
  {
  public:
    type_conv_info (type_conv_fcn f = 0, int t = -1) : _fcn (f), _type_id (t) { }

    operator type_conv_fcn (void) const { return _fcn; }

    octave_base_value * operator () (const octave_base_value &v) const 
      { return (*_fcn) (v); }

    int type_id (void) const { return _type_id; }

  private:
    type_conv_fcn _fcn;
    int _type_id;
  };

  friend class octave_value;

  octave_base_value (void) : count (1) { }

  octave_base_value (const octave_base_value&) { }

  virtual ~octave_base_value (void) { }

  virtual octave_base_value *
  clone (void) const { return new octave_base_value (*this); }

  virtual octave_base_value *
  empty_clone (void) const { return new octave_base_value (); }

  virtual type_conv_info
  numeric_conversion_function (void) const
    { return type_conv_info (); }

  virtual type_conv_info
  numeric_demotion_function (void) const
    { return type_conv_info (); }

  virtual octave_value squeeze (void) const;

  virtual octave_value full_value (void) const;

  virtual octave_base_value *try_narrowing_conversion (void) { return 0; }

  virtual void maybe_economize (void) { }

  virtual octave_value
  subsref (const std::string& type,
	   const std::list<octave_value_list>& idx);

  virtual octave_value_list
  subsref (const std::string& type,
	   const std::list<octave_value_list>& idx,
	   int nargout);

  virtual octave_value
  subsref (const std::string& type,
	   const std::list<octave_value_list>& idx,
           bool auto_add);

  virtual octave_value
  do_index_op (const octave_value_list& idx, bool resize_ok = false);

  virtual octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx);

  virtual void assign (const std::string&, const octave_value&) { }

  virtual octave_value
  subsasgn (const std::string& type,
	    const std::list<octave_value_list>& idx,
	    const octave_value& rhs);

  virtual idx_vector index_vector (void) const;

  virtual dim_vector dims (void) const { return dim_vector (-1, -1); }

  octave_idx_type rows (void) const
    {
      dim_vector dv = dims ();

      return (dv.length () > 0) ? dv(0) : -1;
    }

  octave_idx_type columns (void) const
    {
      dim_vector dv = dims ();

      return (dv.length () > 1) ? dv(1) : -1;
    }

  virtual int ndims (void) const;

  virtual octave_idx_type numel (void) const { return dims ().numel (); }

  virtual octave_idx_type capacity (void) const { return numel (); }

  virtual size_t byte_size (void) const { return 0; }

  virtual octave_idx_type nnz (void) const;

  virtual octave_idx_type nzmax (void) const;

  virtual octave_idx_type nfields (void) const;

  virtual octave_value reshape (const dim_vector&) const;

  virtual octave_value permute (const Array<int>& vec, bool = false) const;

  virtual octave_value resize (const dim_vector&, bool fill = false) const;

  virtual MatrixType matrix_type (void) const;

  virtual MatrixType matrix_type (const MatrixType& typ) const;

  virtual bool is_defined (void) const { return false; }

  bool is_empty (void) const { return numel () == 0; }

  virtual bool is_cell (void) const { return false; }

  virtual bool is_cellstr (void) const { return false; }

  virtual bool is_real_scalar (void) const { return false; }

  virtual bool is_real_matrix (void) const { return false; }

  virtual bool is_real_nd_array (void) const { return false; }

  virtual bool is_complex_scalar (void) const { return false; }

  virtual bool is_complex_matrix (void) const { return false; }

  virtual bool is_bool_scalar (void) const { return false; }

  virtual bool is_bool_matrix (void) const { return false; }

  virtual bool is_char_matrix (void) const { return false; }

  virtual bool is_diag_matrix (void) const { return false; }

  virtual bool is_perm_matrix (void) const { return false; }

  virtual bool is_string (void) const { return false; }

  virtual bool is_sq_string (void) const { return false; }

  virtual bool is_range (void) const { return false; }

  virtual bool is_map (void) const { return false; }

  virtual bool is_object (void) const { return false; }

  virtual bool is_cs_list (void) const { return false; }

  virtual bool is_list (void) const { return false; }

  virtual bool is_magic_colon (void) const { return false; }

  virtual bool is_all_va_args (void) const { return false; }

  virtual octave_value all (int = 0) const;

  virtual octave_value any (int = 0) const;

  virtual bool is_double_type (void) const { return false; }

  virtual bool is_single_type (void) const { return false; }

  virtual bool is_float_type (void) const { return false; }

  virtual bool is_int8_type (void) const { return false; }

  virtual bool is_int16_type (void) const { return false; }

  virtual bool is_int32_type (void) const { return false; }

  virtual bool is_int64_type (void) const { return false; }

  virtual bool is_uint8_type (void) const { return false; }

  virtual bool is_uint16_type (void) const { return false; }

  virtual bool is_uint32_type (void) const { return false; }

  virtual bool is_uint64_type (void) const { return false; }

  virtual bool is_bool_type (void) const { return false; }

  virtual bool is_integer_type (void) const { return false; }

  virtual bool is_real_type (void) const { return false; }

  virtual bool is_complex_type (void) const { return false; }

  // Would be nice to get rid of the next four functions:

  virtual bool is_scalar_type (void) const { return false; }

  virtual bool is_matrix_type (void) const { return false; }

  virtual bool is_numeric_type (void) const { return false; }

  virtual bool is_sparse_type (void) const { return false; }

  virtual bool is_true (void) const { return false; }

  virtual bool is_null_value (void) const { return false; }

  virtual bool is_constant (void) const { return false; }

  virtual bool is_function_handle (void) const { return false; }

  virtual bool is_inline_function (void) const { return false; }

  virtual bool is_function (void) const { return false; }

  virtual bool is_user_script (void) const { return false; }

  virtual bool is_user_function (void) const { return false; }

  virtual bool is_user_code (void) const { return false; }

  virtual bool is_builtin_function (void) const { return false; }

  virtual bool is_dld_function (void) const { return false; }

  virtual bool is_mex_function (void) const { return false; }

  virtual void erase_subfunctions (void) { }

  virtual short int short_value (bool = false, bool = false) const;

  virtual unsigned short int ushort_value (bool = false, bool = false) const;

  virtual int int_value (bool = false, bool = false) const;

  virtual unsigned int uint_value (bool = false, bool = false) const;

  virtual int nint_value (bool = false) const;

  virtual long int long_value (bool = false, bool = false) const;

  virtual unsigned long int ulong_value (bool = false, bool = false) const;

  virtual double double_value (bool = false) const;

  virtual float float_value (bool = false) const;

  virtual double scalar_value (bool frc_str_conv = false) const
    { return double_value (frc_str_conv); }

  virtual float float_scalar_value (bool frc_str_conv = false) const
    { return float_value (frc_str_conv); }

  virtual Cell cell_value (void) const;

  virtual Matrix matrix_value (bool = false) const;

  virtual FloatMatrix float_matrix_value (bool = false) const;

  virtual NDArray array_value (bool = false) const;

  virtual FloatNDArray float_array_value (bool = false) const;

  virtual Complex complex_value (bool = false) const;

  virtual FloatComplex float_complex_value (bool = false) const;

  virtual ComplexMatrix complex_matrix_value (bool = false) const;

  virtual FloatComplexMatrix float_complex_matrix_value (bool = false) const;

  virtual ComplexNDArray complex_array_value (bool = false) const;

  virtual FloatComplexNDArray float_complex_array_value (bool = false) const;

  virtual bool bool_value (bool = false) const;

  virtual boolMatrix bool_matrix_value (bool = false) const;

  virtual boolNDArray bool_array_value (bool = false) const;

  virtual charMatrix char_matrix_value (bool force = false) const;

  virtual charNDArray char_array_value (bool = false) const;

  virtual SparseMatrix sparse_matrix_value (bool = false) const;

  virtual SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  virtual SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  virtual DiagMatrix diag_matrix_value (bool = false) const;

  virtual FloatDiagMatrix float_diag_matrix_value (bool = false) const;
  
  virtual ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;
  
  virtual FloatComplexDiagMatrix float_complex_diag_matrix_value (bool = false) const;
  
  virtual PermMatrix perm_matrix_value (void) const;

  virtual octave_int8 int8_scalar_value (void) const;

  virtual octave_int16 int16_scalar_value (void) const;

  virtual octave_int32 int32_scalar_value (void) const;

  virtual octave_int64 int64_scalar_value (void) const;

  virtual octave_uint8 uint8_scalar_value (void) const;

  virtual octave_uint16 uint16_scalar_value (void) const;

  virtual octave_uint32 uint32_scalar_value (void) const;

  virtual octave_uint64 uint64_scalar_value (void) const;

  virtual int8NDArray int8_array_value (void) const;

  virtual int16NDArray int16_array_value (void) const;

  virtual int32NDArray int32_array_value (void) const;

  virtual int64NDArray int64_array_value (void) const;

  virtual uint8NDArray uint8_array_value (void) const;

  virtual uint16NDArray uint16_array_value (void) const;

  virtual uint32NDArray uint32_array_value (void) const;

  virtual uint64NDArray uint64_array_value (void) const;

  virtual string_vector all_strings (bool pad = false) const;

  virtual std::string string_value (bool force = false) const;

  virtual Array<std::string> cellstr_value (void) const;

  virtual Range range_value (void) const;

  virtual Octave_map map_value (void) const;

  virtual string_vector map_keys (void) const;

  virtual size_t nparents (void) const;

  virtual std::list<std::string> parent_class_name_list (void) const;

  virtual string_vector parent_class_names (void) const;

  virtual octave_base_value *find_parent_class (const std::string&)
    { return 0; }

  virtual octave_function *function_value (bool silent = false);

  virtual const octave_function *function_value (bool silent = false) const;

  virtual octave_user_function *user_function_value (bool silent = false);

  virtual octave_user_script *user_script_value (bool silent = false);

  virtual octave_user_code *user_code_value (bool silent = false);

  virtual octave_fcn_handle *fcn_handle_value (bool silent = false);

  virtual octave_fcn_inline *fcn_inline_value (bool silent = false);

  virtual octave_value_list list_value (void) const;

  virtual octave_value convert_to_str (bool pad = false, bool force = false,
				       char type = '"') const;
  virtual octave_value
  convert_to_str_internal (bool pad, bool force, char type) const;

  virtual void convert_to_row_or_column_vector (void);

  virtual bool print_as_scalar (void) const { return false; }

  virtual void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual bool
  print_name_tag (std::ostream& os, const std::string& name) const;

  virtual void
  print_with_name (std::ostream& output_buf, const std::string& name, 
		   bool print_padding = true) const;

  virtual void print_info (std::ostream& os, const std::string& prefix) const;

  virtual bool save_ascii (std::ostream& os);

  virtual bool load_ascii (std::istream& is);

  virtual bool save_binary (std::ostream& os, bool& save_as_floats);

  virtual bool load_binary (std::istream& is, bool swap, 
			    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  virtual bool
  save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  virtual bool
  load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  virtual int
  write (octave_stream& os, int block_size,
	 oct_data_conv::data_type output_type, int skip,
	 oct_mach_info::float_format flt_fmt) const;

  virtual void *mex_get_data (void) const { return 0; }

  virtual octave_idx_type *mex_get_ir (void) const { return 0; }

  virtual octave_idx_type *mex_get_jc (void) const { return 0; }

  virtual mxArray *as_mxArray (void) const;

  virtual octave_value diag (octave_idx_type k = 0) const;

  virtual octave_value sort (octave_idx_type dim = 0, 
			     sortmode mode = ASCENDING) const;
  virtual octave_value sort (Array<octave_idx_type> &sidx, 
			     octave_idx_type dim = 0,
			     sortmode mode = ASCENDING) const;

  virtual sortmode is_sorted (sortmode mode = UNSORTED) const;

  virtual Array<octave_idx_type>
  sort_rows_idx (sortmode mode = ASCENDING) const;

  virtual sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  virtual void lock (void);

  virtual void unlock (void);

  virtual bool islocked (void) const { return false; }

  virtual void dump (std::ostream& os) const;

  virtual octave_value abs (void) const;
  virtual octave_value acos (void) const;
  virtual octave_value acosh (void) const;
  virtual octave_value angle (void) const;
  virtual octave_value arg (void) const;
  virtual octave_value asin (void) const;
  virtual octave_value asinh (void) const;
  virtual octave_value atan (void) const;
  virtual octave_value atanh (void) const;
  virtual octave_value ceil (void) const;
  virtual octave_value conj (void) const;
  virtual octave_value cos (void) const;
  virtual octave_value cosh (void) const;
  virtual octave_value erf (void) const;
  virtual octave_value erfc (void) const;
  virtual octave_value exp (void) const;
  virtual octave_value expm1 (void) const;
  virtual octave_value finite (void) const;
  virtual octave_value fix (void) const;
  virtual octave_value floor (void) const;
  virtual octave_value gamma (void) const;
  virtual octave_value imag (void) const;
  virtual octave_value isinf (void) const;
  virtual octave_value isna (void) const;
  virtual octave_value isnan (void) const;
  virtual octave_value lgamma (void) const;
  virtual octave_value log (void) const;
  virtual octave_value log2 (void) const;
  virtual octave_value log10 (void) const;
  virtual octave_value log1p (void) const;
  virtual octave_value real (void) const;
  virtual octave_value round (void) const;
  virtual octave_value roundb (void) const;
  virtual octave_value signum (void) const;
  virtual octave_value sin (void) const;
  virtual octave_value sinh (void) const;
  virtual octave_value sqrt (void) const;
  virtual octave_value tan (void) const;
  virtual octave_value tanh (void) const;

  // These functions are prefixed with X to avoid potential macro
  // conflicts.

  virtual octave_value xisalnum (void) const;
  virtual octave_value xisalpha (void) const;
  virtual octave_value xisascii (void) const;
  virtual octave_value xiscntrl (void) const;
  virtual octave_value xisdigit (void) const;
  virtual octave_value xisgraph (void) const;
  virtual octave_value xislower (void) const;
  virtual octave_value xisprint (void) const;
  virtual octave_value xispunct (void) const;
  virtual octave_value xisspace (void) const;
  virtual octave_value xisupper (void) const;
  virtual octave_value xisxdigit (void) const;
  virtual octave_value xtoascii (void) const;
  virtual octave_value xtolower (void) const;
  virtual octave_value xtoupper (void) const;

protected:

  // This should only be called for derived types.

  octave_value numeric_assign (const std::string& type,
			       const std::list<octave_value_list>& idx,
			       const octave_value& rhs);

  void reset_indent_level (void) const
    { curr_print_indent_level = 0; }

  void increment_indent_level (void) const
    { curr_print_indent_level += 2; }

  void decrement_indent_level (void) const
    { curr_print_indent_level -= 2; }

  int current_print_indent_level (void) const
    { return curr_print_indent_level; }

  void indent (std::ostream& os) const;

  void newline (std::ostream& os) const;

  void reset (void) const;

  // A reference count.
  int count;

private:

  static int curr_print_indent_level;
  static bool beginning_of_line;

  DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA
};

// TRUE means to perform automatic sparse to real mutation if there
// is memory to be saved
extern bool Vsparse_auto_mutate;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
