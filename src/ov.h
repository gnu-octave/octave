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

#include <string>

class ostream;

#include "Range.h"
#include "idx-vector.h"
#include "mx-base.h"
#include "oct-alloc.h"
#include "oct-sym.h"
#include "str-vec.h"

class Octave_map;
class octave_value_list;

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
// can't handlt that.

typedef octave_value (*binary_op_fcn)
  (const octave_value&, const octave_value&);

typedef octave_value (*assign_op_fcn)
  (octave_value&, const octave_value_list&, const octave_value&);

typedef octave_value * (*type_conv_fcn) (const octave_value&);

class
octave_value : public octave_symbol
{
public:

  enum binary_op
  {
    add,
    sub,
    mul,
    div,
    pow,
    ldiv,
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
    el_mul_eq,
    el_div_eq,
    el_and_eq,
    el_or_eq,
    num_assign_ops,
    unknown_assign_op
  };

  static string binary_op_as_string (binary_op);

  static string assign_op_as_string (assign_op);

  enum magic_colon { magic_colon_t };
  enum all_va_args { all_va_args_t };

  octave_value (void);
  octave_value (double d);
  octave_value (const Matrix& m);
  octave_value (const DiagMatrix& d);
  octave_value (const RowVector& v, int pcv = -1);
  octave_value (const ColumnVector& v, int pcv = -1);
  octave_value (const Complex& C);
  octave_value (const ComplexMatrix& m);
  octave_value (const ComplexDiagMatrix& d);
  octave_value (const ComplexRowVector& v, int pcv = -1);
  octave_value (const ComplexColumnVector& v, int pcv = -1);
  octave_value (bool b);
  octave_value (const boolMatrix& bm);
  octave_value (const char *s);
  octave_value (const string& s);
  octave_value (const string_vector& s);
  octave_value (const charMatrix& chm, bool is_string = false);
  octave_value (double base, double limit, double inc);
  octave_value (const Range& r);
  octave_value (const Octave_map& m);
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

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

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

  virtual type_conv_fcn numeric_conversion_function (void) const
    { return rep->numeric_conversion_function (); }

  void maybe_mutate (void);

  virtual octave_value *try_narrowing_conversion (void)
    { return rep->try_narrowing_conversion (); }

  virtual octave_value index (const octave_value_list& idx) const
    { return rep->index (idx); }

  octave_value& assign (assign_op, const octave_value& rhs);

  octave_value& assign (assign_op, const octave_value_list& idx,
			const octave_value& rhs);

  virtual idx_vector index_vector (void) const
    { return rep->index_vector (); }

  virtual octave_value
  struct_elt_val (const string& nm, bool silent = false) const
    { return rep->struct_elt_val (nm, silent); }

  virtual octave_value& struct_elt_ref (const string& nm)
    { return rep->struct_elt_ref (nm); }

  // Size.

  virtual int rows (void) const
    { return rep->rows (); }

  virtual int columns (void) const
    { return rep->columns (); }

  // Does this constant have a type?  Both of these are provided since
  // it is sometimes more natural to write is_undefined() instead of
  // ! is_defined().

  virtual bool is_defined (void) const
    { return rep->is_defined (); }

  bool is_undefined (void) const
    { return ! is_defined (); }

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

  bool is_constant (void) const
    { return true; }

  // Values.

  octave_value eval (void) { return *this; }

  octave_value_list eval (int, const octave_value_list& idx);

  virtual double double_value (bool frc_str_conv = false) const
    { return rep->double_value (frc_str_conv); }

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

  virtual octave_value_list list_value (void) const;

  virtual bool bool_value (void) const
    { return rep->bool_value (); }

  virtual boolMatrix bool_matrix_value (void) const
    { return rep->bool_matrix_value (); }

  // Unary ops.

  virtual octave_value not (void) const { return rep->not (); }

  virtual octave_value uminus (void) const { return rep->uminus (); }

  virtual octave_value transpose (void) const { return rep->transpose (); }

  virtual octave_value hermitian (void) const { return rep->hermitian (); }

  virtual void increment (void)
    {
      make_unique ();
      rep->increment ();
    }

  virtual void decrement (void)
    {
      make_unique ();
      rep->decrement ();
    }

  ColumnVector vector_value (bool frc_str_conv = false,
			     bool frc_vec_conv = false) const;

  ComplexColumnVector
  complex_vector_value (bool frc_str_conv = false,
			bool frc_vec_conv = false) const;

  // Conversions.  These should probably be private.  If a user of this
  // class wants a certain kind of constant, he should simply ask for
  // it, and we should convert it if possible.

  virtual octave_value convert_to_str (void) const
    { return rep->convert_to_str (); }

  virtual void convert_to_row_or_column_vector (void)
    { rep->convert_to_row_or_column_vector (); }

  void print (bool pr_as_read_syntax = false);

  virtual void print (ostream& os, bool pr_as_read_syntax = false)
    { rep->print (os, pr_as_read_syntax); }

  void print_with_name (const string& name, bool print_padding = true);

  void print_with_name (ostream& os, const string& name,
			bool print_padding = true);

  virtual int type_id (void) const { return rep->type_id (); }

  virtual string type_name (void) const { return rep->type_name (); }

  // Binary and unary operations.

  friend octave_value do_binary_op (octave_value::binary_op,
				    const octave_value&,
				    const octave_value&);

  // Can we make these go away?

  bool print_as_scalar (void);

protected:

  octave_value (const octave_xvalue&) : rep (0) { }

private:

  static octave_allocator allocator;

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
};

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

// If TRUE, create column vectors when doing assignments like:
//
//   octave> A(1) = 3; A(2) = 5
//
// (for A undefined).  Only matters when resize_on_range_error is also
// TRUE.
extern bool Vprefer_column_vectors;

// If TRUE, print the name along with the value.
extern bool Vprint_answer_id_name;

// Should operations on empty matrices return empty matrices or an
// error?  A positive value means yes.  A negative value means yes,
// but print a warning message.  Zero means it should be considered an
// error.
extern int Vpropagate_empty_matrices;

// If TRUE, resize matrices when performing and indexed assignment and
// the indices are outside the current bounds.
extern bool Vresize_on_range_error;

// How many levels of structure elements should we print?
extern int Vstruct_levels_to_print;

// Allow divide by zero errors to be suppressed.
extern bool Vwarn_divide_by_zero;

// Indentation level for structures.
extern int struct_indent;

extern void increment_struct_indent (void);
extern void decrement_struct_indent (void);

// Indentation level for lists.
extern int list_indent;

extern void increment_list_indent (void);
extern void decrement_list_indent (void);

extern void symbols_of_value (void);

extern void install_types (void);

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
