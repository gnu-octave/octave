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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_value_h)
#define octave_value_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "Range.h"
#include "idx-vector.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

class Cell;
class Octave_map;
class octave_stream;
class octave_function;
class octave_value_list;
class octave_lvalue;

// Constants.

// This just provides a way to avoid infinite recursion when building
// octave_value objects.

class
octave_xvalue
{
public:

  octave_xvalue (void) { }
};

class octave_value;

// XXX FIXME XXX -- these should probably really be inside the scope
// of the octave_value class, but the cygwin32 beta16 version of g++
// can't handle that.

typedef octave_value (*unary_op_fcn)
  (const octave_value&);

typedef void (*non_const_unary_op_fcn)
  (octave_value&);

typedef octave_value (*binary_op_fcn)
  (const octave_value&, const octave_value&);

typedef octave_value (*assign_op_fcn)
  (octave_value&, const octave_value_list&, const octave_value&);

typedef octave_value * (*type_conv_fcn) (const octave_value&);

class
octave_value
{
public:

  enum unary_op
  {
    not,
    uminus,
    transpose,
    hermitian,
    incr,
    decr,
    num_unary_ops,
    unknown_unary_op
  };

  enum binary_op
  {
    add,
    sub,
    mul,
    div,
    pow,
    ldiv,
    lshift,
    rshift,
    lt,
    le,
    eq,
    ge,
    gt,
    ne,
    el_mul,
    el_div,
    el_pow,
    el_ldiv,
    el_and,
    el_or,
    struct_ref,
    num_binary_ops,
    unknown_binary_op
  };

  enum assign_op
  {
    asn_eq,
    add_eq,
    sub_eq,
    mul_eq,
    div_eq,
    ldiv_eq,
    lshift_eq,
    rshift_eq,
    el_mul_eq,
    el_div_eq,
    el_ldiv_eq,
    el_and_eq,
    el_or_eq,
    num_assign_ops,
    unknown_assign_op
  };

  static string unary_op_as_string (unary_op);

  static string binary_op_as_string (binary_op);

  static string assign_op_as_string (assign_op);

  enum magic_colon { magic_colon_t };
  enum all_va_args { all_va_args_t };

  octave_value (void);
  octave_value (double d);
  octave_value (const Cell& m);
  octave_value (const Matrix& m);
  octave_value (const DiagMatrix& d);
  octave_value (const RowVector& v);
  octave_value (const ColumnVector& v);
  octave_value (const Complex& C);
  octave_value (const ComplexMatrix& m);
  octave_value (const ComplexDiagMatrix& d);
  octave_value (const ComplexRowVector& v);
  octave_value (const ComplexColumnVector& v);
  octave_value (bool b);
  octave_value (const boolMatrix& bm);
  octave_value (char c);
  octave_value (const char *s);
  octave_value (const string& s);
  octave_value (const string_vector& s);
  octave_value (const charMatrix& chm, bool is_string = false);
  octave_value (double base, double limit, double inc);
  octave_value (const Range& r);
  octave_value (const Octave_map& m);
  octave_value (const octave_stream& s, int n);
  octave_value (octave_function *f);
  octave_value (const octave_value_list& m);
  octave_value (octave_value::magic_colon);
  octave_value (octave_value::all_va_args);

  octave_value (octave_value *new_rep);

  // Copy constructor.

  octave_value (const octave_value& a)
    {
      rep = a.rep;
      rep->count++;
    }

  // Delete the representation of this constant if the count drops to
  // zero.

  virtual ~octave_value (void);

  // This should only be called for derived types.

  virtual octave_value *clone (void);

  void make_unique (void)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = rep->clone ();
	  rep->count = 1;
	}
    }

  // Simple assignment.

  octave_value& operator = (const octave_value& a)
    {
      if (rep != a.rep)
	{
	  if (--rep->count == 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;
	}

      return *this;
    }

  int get_count (void) { return rep->count; }

  virtual type_conv_fcn numeric_conversion_function (void) const
    { return rep->numeric_conversion_function (); }

  void maybe_mutate (void);

  virtual octave_value *try_narrowing_conversion (void)
    { return rep->try_narrowing_conversion (); }

  virtual octave_value do_index_op (const octave_value_list& idx)
    { return rep->do_index_op (idx); }

  virtual octave_value_list
  do_index_op (int nargout, const octave_value_list& idx);

  void assign (assign_op, const octave_value& rhs);

  void assign (assign_op, const octave_value_list& idx,
	       const octave_value& rhs);

  virtual void
  assign_struct_elt (assign_op, const string& elt_nm,
		     const octave_value& rhs);

  virtual void
  assign_struct_elt (assign_op, const string& elt_nm,
		     const octave_value_list& idx, const octave_value& rhs);

  virtual idx_vector index_vector (void) const
    { return rep->index_vector (); }

  virtual octave_value
  do_struct_elt_index_op (const string& nm, bool silent = false)
    { return rep->do_struct_elt_index_op (nm, silent); }

  virtual octave_value
  do_struct_elt_index_op (const string& nm, const octave_value_list& idx,
			  bool silent = false)
    { return rep->do_struct_elt_index_op (nm, idx, silent); }

  octave_lvalue struct_elt_ref (const string& nm);

  virtual octave_lvalue
  struct_elt_ref (octave_value *parent, const string& nm);

  // Size.

  virtual int rows (void) const
    { return rep->rows (); }

  virtual int columns (void) const
    { return rep->columns (); }

  virtual int length (void) const
    { return rep->length (); }

  // Does this constant have a type?  Both of these are provided since
  // it is sometimes more natural to write is_undefined() instead of
  // ! is_defined().

  virtual bool is_defined (void) const
    { return rep->is_defined (); }

  bool is_undefined (void) const
    { return ! is_defined (); }

  virtual bool is_cell (void) const
    { return rep->is_cell (); }

  virtual bool is_real_scalar (void) const
    { return rep->is_real_scalar (); }

  virtual bool is_real_matrix (void) const
    { return rep->is_real_matrix (); }

  virtual bool is_complex_scalar (void) const
    { return rep->is_complex_scalar (); }

  virtual bool is_complex_matrix (void) const
    { return rep->is_complex_matrix (); }

  virtual bool is_char_matrix (void) const
    { return rep->is_char_matrix (); }

  virtual bool is_string (void) const
    { return rep->is_string (); }

  virtual bool is_range (void) const
    { return rep->is_range (); }

  virtual bool is_map (void) const
    { return rep->is_map (); }

  virtual bool is_stream (void) const
    { return rep->is_stream (); }

  virtual bool is_list (void) const
    { return rep->is_list (); }

  virtual bool is_magic_colon (void) const
    { return rep->is_magic_colon (); }

  virtual bool is_all_va_args (void) const
    { return rep->is_all_va_args (); }

  // Are any or all of the elements in this constant nonzero?

  virtual octave_value all (void) const
    { return rep->all (); }

  virtual octave_value any (void) const
    { return rep->any (); }

  // Other type stuff.

  virtual bool is_bool_type (void) const
    { return rep->is_bool_type (); }

  virtual bool is_real_type (void) const
    { return rep->is_real_type (); }

  virtual bool is_complex_type (void) const
    { return rep->is_complex_type (); }

  virtual bool is_scalar_type (void) const
    { return rep->is_scalar_type (); }

  virtual bool is_matrix_type (void) const
    { return rep->is_matrix_type (); }

  virtual bool is_numeric_type (void) const
    { return rep->is_numeric_type (); }

  virtual bool valid_as_scalar_index (void) const
    { return rep->valid_as_scalar_index (); }

  virtual bool valid_as_zero_index (void) const
    { return rep->valid_as_zero_index (); }

  // Does this constant correspond to a truth value?

  virtual bool is_true (void) const
    { return rep->is_true (); }

  // Is at least one of the dimensions of this constant zero?

  virtual bool is_empty (void) const
    { return rep->is_empty (); }

  // Are the dimensions of this constant zero by zero?

  virtual bool is_zero_by_zero (void) const
    { return rep->is_zero_by_zero (); }

  virtual bool is_constant (void) const
    { return rep->is_constant (); }

  virtual bool is_function (void) const
    { return rep->is_function (); }

  virtual bool is_builtin_function (void) const
    { return rep->is_builtin_function (); }

  virtual bool is_dld_function (void) const
    { return rep->is_dld_function (); }

  // Values.

  octave_value eval (void) { return *this; }

  virtual int int_value (bool req_int = false, bool frc_str_conv = false) const
    { return rep->int_value (req_int, frc_str_conv); }

  virtual int nint_value (bool frc_str_conv = false) const
    { return rep->nint_value (frc_str_conv); }

  virtual double double_value (bool frc_str_conv = false) const
    { return rep->double_value (frc_str_conv); }

  virtual double scalar_value (bool frc_str_conv = false) const
    { return rep->scalar_value (frc_str_conv); }

  virtual Cell cell_value (void) const;

  virtual Matrix matrix_value (bool frc_str_conv = false) const
    { return rep->matrix_value (frc_str_conv); }

  virtual Complex complex_value (bool frc_str_conv = false) const
    { return rep->complex_value (frc_str_conv); }

  virtual ComplexMatrix complex_matrix_value (bool frc_str_conv = false) const
    { return rep->complex_matrix_value (frc_str_conv); }

  virtual charMatrix char_matrix_value (bool frc_str_conv = false) const
    { return rep->char_matrix_value (frc_str_conv); }

  virtual string_vector all_strings (void) const
    { return rep->all_strings (); }

  virtual string string_value (void) const
    { return rep->string_value (); }

  virtual Range range_value (void) const
    { return rep->range_value (); }

  virtual Octave_map map_value (void) const;

  virtual octave_stream stream_value (void) const;

  virtual int stream_number (void) const;

  virtual octave_function *function_value (bool silent = false);

  virtual octave_value_list list_value (void) const;

  virtual bool bool_value (void) const
    { return rep->bool_value (); }

  virtual boolMatrix bool_matrix_value (void) const
    { return rep->bool_matrix_value (); }

  ColumnVector column_vector_value (bool frc_str_conv = false,
			     bool frc_vec_conv = false) const;

  ComplexColumnVector
  complex_column_vector_value (bool frc_str_conv = false,
			bool frc_vec_conv = false) const;

  RowVector row_vector_value (bool frc_str_conv = false,
			      bool frc_vec_conv = false) const;

  ComplexRowVector
  complex_row_vector_value (bool frc_str_conv = false,
			    bool frc_vec_conv = false) const;

  Array<double> vector_value (bool frc_str_conv = false,
			      bool frc_vec_conv = false) const;

  Array<Complex> complex_vector_value (bool frc_str_conv = false,
				       bool frc_vec_conv = false) const;

  // Conversions.  These should probably be private.  If a user of this
  // class wants a certain kind of constant, he should simply ask for
  // it, and we should convert it if possible.

  virtual octave_value convert_to_str (void) const
    { return rep->convert_to_str (); }

  virtual void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

  virtual void print (ostream& os, bool pr_as_read_syntax = false) const
    { rep->print (os, pr_as_read_syntax); }

  virtual void print_raw (ostream& os, bool pr_as_read_syntax = false) const
    { rep->print_raw (os, pr_as_read_syntax); }

  virtual bool print_name_tag (ostream& os, const string& name) const
    { return rep->print_name_tag (os, name); }

  void print_with_name (ostream& os, const string& name,
			bool print_padding = true) const;

  virtual int type_id (void) const { return rep->type_id (); }

  virtual string type_name (void) const { return rep->type_name (); }

  // Unary and binary operations.

  friend octave_value do_unary_op (octave_value::unary_op,
				   const octave_value&);

  void do_non_const_unary_op (octave_value::unary_op);

  void do_non_const_unary_op (octave_value::unary_op,
			      const octave_value_list& idx);

  friend octave_value do_binary_op (octave_value::binary_op,
				    const octave_value&,
				    const octave_value&);

  const octave_value& get_rep (void) const { return *rep; }

protected:

  octave_value (const octave_xvalue&) : rep (0) { }

  void reset_indent_level (void) const
    { curr_print_indent_level = 0; }

  void increment_indent_level (void) const
    { curr_print_indent_level += 2; }

  void decrement_indent_level (void) const
    { curr_print_indent_level -= 2; }

  int current_print_indent_level (void) const
    { return curr_print_indent_level; }

  void newline (ostream& os) const;

  void indent (ostream& os) const;

  void reset (void) const;

private:

  union
    {
      octave_value *rep;      // The real representation.
      int count;              // A reference count.
    };

  bool convert_and_assign (assign_op, const octave_value_list& idx,
			   const octave_value& rhs);

  bool try_assignment_with_conversion (assign_op,
				       const octave_value_list& idx,
				       const octave_value& rhs);

  bool try_assignment (assign_op, const octave_value_list& idx,
		       const octave_value& rhs);

  static int curr_print_indent_level;
  static bool beginning_of_line;

  assign_op unary_op_to_assign_op (unary_op op);

  binary_op op_eq_to_binary_op (assign_op op);

  void simple_assign (assign_op orig_op, const octave_value_list& idx,
		      const octave_value& rhs);

  DECLARE_OCTAVE_ALLOCATOR
};

#define OV_UNOP_FN(name) \
  inline octave_value \
  name (const octave_value& a) \
  { \
    return do_unary_op (octave_value::name, a); \
  }

#define OV_UNOP_OP(name, op) \
  inline octave_value \
  operator op (const octave_value& a) \
  { \
    return name (a); \
  }

#define OV_UNOP_FN_OP(name, op) \
  OV_UNOP_FN (name) \
  OV_UNOP_OP (name, op)

OV_UNOP_FN_OP (not, !)
OV_UNOP_FN_OP (uminus, -)

OV_UNOP_FN (transpose)
OV_UNOP_FN (hermitian)

// No simple way to define these for prefix and suffix ops?
//
//   incr
//   decr

#define OV_BINOP_FN(name) \
  inline octave_value \
  name (const octave_value& a1, const octave_value& a2) \
  { \
    return do_binary_op (octave_value::name, a1, a2); \
  }

#define OV_BINOP_OP(name, op) \
  inline octave_value \
  operator op (const octave_value& a1, const octave_value& a2) \
  { \
    return name (a1, a2); \
  }

#define OV_BINOP_FN_OP(name, op) \
  OV_BINOP_FN (name) \
  OV_BINOP_OP (name, op)

OV_BINOP_FN_OP (add, +)
OV_BINOP_FN_OP (sub, -)
OV_BINOP_FN_OP (mul, *)
OV_BINOP_FN_OP (div, /)

OV_BINOP_FN (pow)
OV_BINOP_FN (ldiv)
OV_BINOP_FN (lshift)
OV_BINOP_FN (rshift)

OV_BINOP_FN_OP (lt, <)
OV_BINOP_FN_OP (le, <=)
OV_BINOP_FN_OP (eq, ==)
OV_BINOP_FN_OP (ge, >=)
OV_BINOP_FN_OP (gt, >)
OV_BINOP_FN_OP (ne, !=)

OV_BINOP_FN (el_mul)
OV_BINOP_FN (el_div)
OV_BINOP_FN (el_pow)
OV_BINOP_FN (el_ldiv)
OV_BINOP_FN (el_and)
OV_BINOP_FN (el_or)

OV_BINOP_FN (struct_ref)

// T_ID is the type id of struct objects, set by register_type().
// T_NAME is the type name of struct objects.
#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA \
  public: \
    int type_id (void) const { return t_id; } \
    string type_name (void) const { return t_name; } \
    static volatile int static_type_id (void) { return t_id; } \
    static void register_type (void) \
      { t_id = octave_value_typeinfo::register_type (t_name); } \
 \
  private: \
    static volatile int t_id; \
    static const string t_name;

#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n) \
  volatile int t::t_id (-1); \
  const string t::t_name (n)

// If TRUE, allow assignments like
//
//   octave> A(1) = 3; A(2) = 5
//
// for A already defined and a matrix type.
extern bool Vdo_fortran_indexing;

// Should we allow things like:
//
//   octave> 'abc' + 0
//   97 98 99
//
// to happen?  A positive value means yes.  A negative value means
// yes, but print a warning message.  Zero means it should be
// considered an error.
extern int Vimplicit_str_to_num_ok;

// Should we allow silent conversion of complex to real when a real
// type is what we're really looking for?  A positive value means yes.
// A negative value means yes, but print a warning message.  Zero
// means it should be considered an error.
extern int Vok_to_lose_imaginary_part;

// If TRUE, print the name along with the value.
extern bool Vprint_answer_id_name;

// Should operations on empty matrices return empty matrices or an
// error?  A positive value means yes.  A negative value means yes,
// but print a warning message.  Zero means it should be considered an
// error.
extern int Vpropagate_empty_matrices;

// How many levels of structure elements should we print?
extern int Vstruct_levels_to_print;

// Allow divide by zero errors to be suppressed.
extern bool Vwarn_divide_by_zero;

// If TRUE, resize matrices when performing and indexed assignment and
// the indices are outside the current bounds.
extern bool Vresize_on_range_error;

// Indentation level for structures.
extern int struct_indent;

extern void increment_struct_indent (void);
extern void decrement_struct_indent (void);

// Indentation level for lists.
extern int list_indent;

extern void increment_list_indent (void);
extern void decrement_list_indent (void);

extern void install_types (void);

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
