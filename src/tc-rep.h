// tc-rep.h                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_tree_const_rep_h)
#define octave_tree_const_rep_h 1

// The actual representation of the tree_constant.

class
tree_constant_rep
{
friend class tree_constant;

private:

  enum constant_type
    {
      unknown_constant,
      scalar_constant,
      matrix_constant,
      complex_scalar_constant,
      complex_matrix_constant,
      string_constant,
      range_constant,
      map_constant,
      magic_colon,
      all_va_args,
    };

  enum force_orient
    {
      no_orient,
      row_orient,
      column_orient,
    };

  tree_constant_rep (void);

  tree_constant_rep (double d);
  tree_constant_rep (const Matrix& m);
  tree_constant_rep (const DiagMatrix& d);
  tree_constant_rep (const RowVector& v, int pcv);
  tree_constant_rep (const ColumnVector& v, int pcv);

  tree_constant_rep (const Complex& c);
  tree_constant_rep (const ComplexMatrix& m);
  tree_constant_rep (const ComplexDiagMatrix& d);
  tree_constant_rep (const ComplexRowVector& v, int pcv);
  tree_constant_rep (const ComplexColumnVector& v, int pcv);

  tree_constant_rep (const char *s);

  tree_constant_rep (double base, double limit, double inc);
  tree_constant_rep (const Range& r);

  tree_constant_rep (const Octave_map& m);

  tree_constant_rep (tree_constant_rep::constant_type t);

  tree_constant_rep (const tree_constant_rep& t);

  ~tree_constant_rep (void);

#if defined (MDEBUG)
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  int rows (void) const;
  int columns (void) const;

  int is_defined (void) const
    { return type_tag != tree_constant_rep::unknown_constant; }

  int is_undefined (void) const
    { return type_tag == tree_constant_rep::unknown_constant; }

  int is_unknown (void) const
    { return type_tag == tree_constant_rep::unknown_constant; }

  int is_real_scalar (void) const
    { return type_tag == tree_constant_rep::scalar_constant; }

  int is_real_matrix (void) const
    { return type_tag == tree_constant_rep::matrix_constant; }

  int is_complex_scalar (void) const
    { return type_tag == tree_constant_rep::complex_scalar_constant; }

  int is_complex_matrix (void) const
    { return type_tag == tree_constant_rep::complex_matrix_constant; }

  int is_string (void) const
    { return type_tag == tree_constant_rep::string_constant; }

  int is_range (void) const
    { return type_tag == tree_constant_rep::range_constant; }

  int is_map (void) const
    { return type_tag == tree_constant_rep::map_constant; }

  int is_magic_colon (void) const
    { return type_tag == tree_constant_rep::magic_colon; }

  int is_all_va_args (void) const
    { return type_tag == tree_constant_rep::all_va_args; }

  tree_constant all (void) const;
  tree_constant any (void) const;

  int is_real_type (void) const
    {
      return (type_tag == scalar_constant
	      || type_tag == matrix_constant
	      || type_tag == range_constant
	      || type_tag == string_constant);
    }

  int is_complex_type (void) const
    {
      return (type_tag == complex_matrix_constant
	      || type_tag == complex_scalar_constant);
    }

// Would be nice to get rid of the next four functions:

  int is_scalar_type (void) const
    {
      return (type_tag == scalar_constant
	      || type_tag == complex_scalar_constant);
    }

  int is_matrix_type (void) const
    {
      return (type_tag == matrix_constant
	      || type_tag == complex_matrix_constant);
    }

  int is_numeric_type (void) const
    {
      return (type_tag == scalar_constant
	      || type_tag == matrix_constant
	      || type_tag == complex_matrix_constant
	      || type_tag == complex_scalar_constant);
    }

  int is_numeric_or_range_type (void) const
    {
      return (type_tag == scalar_constant
	      || type_tag == matrix_constant
	      || type_tag == complex_matrix_constant
	      || type_tag == complex_scalar_constant
	      || type_tag == range_constant);
    }

  int valid_as_scalar_index (void) const;

  int is_true (void) const;

  double double_value (int force_string_conversion = 0) const;
  Matrix matrix_value (int force_string_conversion = 0) const;
  Complex complex_value (int force_string_conversion = 0) const;
  ComplexMatrix complex_matrix_value (int force_string_conversion = 0) const;
  char *string_value (void) const;
  Range range_value (void) const;
  Octave_map map_value (void) const;

  tree_constant& lookup_map_element (const char *name, int insert = 0);

  ColumnVector vector_value (int force_string_conversion = 0,
			     int force_vector_conversion = 0) const;

  ComplexColumnVector complex_vector_value (int force_string_conv = 0,
					    int force_vec_conv = 0) const;

  tree_constant convert_to_str (void) const;

  void convert_to_row_or_column_vector (void);

  void bump_value (tree_expression::type);

  void resize (int i, int j);
  void resize (int i, int j, double val);

  void maybe_resize (int imax, force_orient fo = no_orient);
  void maybe_resize (int imax, int jmax);

  void stash_original_text (char *s);

  void maybe_mutate (void);

  void print (void);

  void print_code (ostream& os);

  void gripe_wrong_type_arg (const char *name,
			     const tree_constant_rep& tcr) const;

  char *type_as_string (void) const;

// Binary and unary operations.

  friend tree_constant do_binary_op (tree_constant& a, tree_constant& b,
				     tree_expression::type t);

  friend tree_constant do_unary_op (tree_constant& a,
				    tree_expression::type t);

// -------------------------------------------------------------------

// We want to eliminate this.

  constant_type const_type (void) const { return type_tag; }

// We want to get rid of these too:

  void force_numeric (int force_str_conv = 0);
  tree_constant make_numeric (int force_str_conv = 0) const;

// Indexing.

  tree_constant do_index (const Octave_object& args);

  tree_constant do_scalar_index (const Octave_object& args) const;

  tree_constant do_matrix_index (const Octave_object& args) const;

  tree_constant do_matrix_index (const tree_constant& i_arg) const;

  tree_constant do_matrix_index (const tree_constant& i_arg,
				 const tree_constant& j_arg) const; 

  tree_constant do_matrix_index (constant_type i) const;

  tree_constant fortran_style_matrix_index (const tree_constant& i_arg) const;
  tree_constant fortran_style_matrix_index (const Matrix& mi) const;

  tree_constant do_vector_index (const tree_constant& i_arg) const;

  tree_constant do_matrix_index (int i, const tree_constant& i_arg) const;
  tree_constant do_matrix_index (const idx_vector& i,
				 const tree_constant& i_arg) const; 
  tree_constant do_matrix_index (const Range& i,
				 const tree_constant& i_arg) const;
  tree_constant do_matrix_index (constant_type i,
				 const tree_constant& i_arg) const;

  tree_constant do_matrix_index (int i, int j) const;
  tree_constant do_matrix_index (int i, const idx_vector& j) const;
  tree_constant do_matrix_index (int i, const Range& j) const;
  tree_constant do_matrix_index (int i, constant_type cj) const;

  tree_constant do_matrix_index (const idx_vector& i, int j) const;
  tree_constant do_matrix_index (const idx_vector& i,
				 const idx_vector& j) const;
  tree_constant do_matrix_index (const idx_vector& i, const Range& j) const;
  tree_constant do_matrix_index (const idx_vector& i, constant_type j) const;

  tree_constant do_matrix_index (const Range& i, int j) const;
  tree_constant do_matrix_index (const Range& i, const idx_vector& j) const;
  tree_constant do_matrix_index (const Range& i, const Range& j) const;
  tree_constant do_matrix_index (const Range& i, constant_type j) const;

  tree_constant do_matrix_index (constant_type i, int j) const;
  tree_constant do_matrix_index (constant_type i, const idx_vector& j) const;
  tree_constant do_matrix_index (constant_type i, const Range& j) const;
  tree_constant do_matrix_index (constant_type i, constant_type j) const;

// Assignment.

  void assign (tree_constant& rhs, const Octave_object& args);

  void do_scalar_assignment (const tree_constant& rhs,
			     const Octave_object& args);

  void do_matrix_assignment (const tree_constant& rhs,
			     const Octave_object& args);

  void do_matrix_assignment (const tree_constant& rhs,
			     const tree_constant& i_arg);

  void fortran_style_matrix_assignment (const tree_constant& rhs,
					const tree_constant& i_arg);

  void fortran_style_matrix_assignment (const tree_constant& rhs,
					constant_type ci);

  void fortran_style_matrix_assignment (const tree_constant& rhs,
					idx_vector& i);

  void vector_assignment (const tree_constant& rhs,
			  const tree_constant& i_arg);

  void check_vector_assign (int rhs_nr, int rhs_nc, int ilen,
			    const char *rm);

  void do_vector_assign (const tree_constant& rhs, int i);
  void do_vector_assign (const tree_constant& rhs, idx_vector& i);
  void do_vector_assign (const tree_constant& rhs, Range& i);

  void do_matrix_assignment (const tree_constant& rhs,
			     const tree_constant& i_arg,
			     const tree_constant& j_arg);

  void do_matrix_assignment (const tree_constant& rhs, int i,
			     const tree_constant& j_arg);
  void do_matrix_assignment (const tree_constant& rhs, idx_vector& i,
			     const tree_constant& j_arg);
  void do_matrix_assignment (const tree_constant& rhs, Range& i,
			     const tree_constant& j_arg);
  void do_matrix_assignment (const tree_constant& rhs, constant_type i,
			     const tree_constant& j_arg);

  void do_matrix_assignment (const tree_constant& rhs, int i, int j);
  void do_matrix_assignment (const tree_constant& rhs, int i, idx_vector& jv);
  void do_matrix_assignment (const tree_constant& rhs, int i, Range& j);
  void do_matrix_assignment (const tree_constant& rhs, int i, constant_type cj);

  void do_matrix_assignment (const tree_constant& rhs, idx_vector& iv,
			     int j);
  void do_matrix_assignment (const tree_constant& rhs, idx_vector& iv,
			     idx_vector& jv);
  void do_matrix_assignment (const tree_constant& rhs, idx_vector& iv,
			     Range& j);
  void do_matrix_assignment (const tree_constant& rhs, idx_vector& iv,
			     constant_type j);

  void do_matrix_assignment (const tree_constant& rhs, Range& i, int j);
  void do_matrix_assignment (const tree_constant& rhs, Range& i,
			     idx_vector& jv);
  void do_matrix_assignment (const tree_constant& rhs, Range& i,
			     Range& j);
  void do_matrix_assignment (const tree_constant& rhs, Range& i,
			     constant_type j);

  void do_matrix_assignment (const tree_constant& rhs, constant_type i, int j);
  void do_matrix_assignment (const tree_constant& rhs, constant_type i,
			     idx_vector& jv);
  void do_matrix_assignment (const tree_constant& rhs, constant_type i,
			     Range& j);
  void do_matrix_assignment (const tree_constant& rhs,
			     const constant_type i,
			     constant_type j);

  void delete_row (int);
  void delete_rows (idx_vector& i);
  void delete_rows (Range& i);

  void delete_column (int);
  void delete_columns (idx_vector& j);
  void delete_columns (Range& j);

// Data.

  int count;

  constant_type type_tag;

  union
    {
      double scalar;			// A real scalar constant.
      Matrix *matrix;			// A real matrix constant.
      Complex *complex_scalar;		// A real scalar constant.
      ComplexMatrix *complex_matrix;	// A real matrix constant.
      char *string;			// A character string constant.
      Range *range;			// A set of evenly spaced values.
      Octave_map *a_map;		// An associative array.
    };

  char *orig_text;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
