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

#include <octave/config.h>

#include <cstdlib>

#include <string>

class ostream;

#include <octave/lo-utils.h>
#include <octave/mx-base.h>
#include <octave/str-vec.h>

#include <octave/defun-dld.h>
#include <octave/error.h>
#include <octave/gripes.h>
#include <octave/mappers.h>
#include <octave/oct-obj.h>
#include <octave/ops.h>
#include <octave/ov-base.h>
#include <octave/ov-typeinfo.h>
#include <octave/ov.h>
#include <octave/ov-scalar.h>
#include <octave/pager.h>
#include <octave/pr-output.h>
#include <octave/symtab.h>
#include <octave/variables.h>

class Octave_map;
class octave_value_list;

class tree_walker;

// Integer values.

class
octave_integer : public octave_base_value
{
public:

  octave_integer (void)
    : octave_base_value (), scalar (0) { }

  octave_integer (int i)
    : octave_base_value (), scalar (i) { }

  octave_integer (const octave_integer& s)
    : octave_base_value (), scalar (s.scalar) { }

  ~octave_integer (void) { }

  octave_value *clone (void) { return new octave_integer (*this); }

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  idx_vector index_vector (void) const { return idx_vector ((double) scalar); }

  int rows (void) const { return 1; }
  int columns (void) const { return 1; }

  bool is_defined (void) const { return true; }
  bool is_real_scalar (void) const { return true; }

  octave_value all (void) const { return (scalar != 0); }
  octave_value any (void) const { return (scalar != 0); }

  bool is_real_type (void) const { return true; }
  bool is_scalar_type (void) const { return true; }
  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const
    { return scalar == 1; }

  bool valid_as_zero_index (void) const
    { return scalar == 0; }

  bool is_true (void) const { return (scalar != 0); }

  double double_value (bool = false) const { return (double) scalar; }

  int integer_value (bool = false) const { return scalar; }

  Matrix matrix_value (bool = false) const { return Matrix (1, 1, scalar); }

  Complex complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
    { return  ComplexMatrix (1, 1, Complex (scalar)); }

  octave_value not (void) const { return octave_value (! scalar); }

  octave_value uminus (void) const { return octave_value (- scalar); }

  octave_value transpose (void) const { return octave_value (scalar); }

  octave_value hermitian (void) const { return octave_value (scalar); }

  void increment (void) { ++scalar; }

  void decrement (void) { --scalar; }

  void print (ostream& os);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  int scalar;

  static int t_id;

  static const string t_name;
};

int octave_integer::t_id = -1;

const string octave_integer::t_name ("integer");

void
octave_integer::print (ostream& os)
{
  octave_print_internal (os, scalar, false);
}

// integer by integer ops.

static octave_value
add (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () + v2.integer_value ());
}

static octave_value
sub (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () - v2.integer_value ());
}

static octave_value
mul (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () * v2.integer_value ());
}

static octave_value
div (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v2.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v1.integer_value () / d);
}

static octave_value
i_s_div (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return new octave_scalar (v1.double_value () / d);
}

static octave_value
ldiv (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v1.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v2.integer_value () / d);
}

static octave_value
lt (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () < v2.integer_value ());
}

static octave_value
le (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () <= v2.integer_value ());
}

static octave_value
eq (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () == v2.integer_value ());
}

static octave_value
ge (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () >= v2.integer_value ());
}

static octave_value
gt (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () > v2.integer_value ());
}

static octave_value
ne (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () != v2.integer_value ());
}

static octave_value
el_mul (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () * v2.integer_value ());
}

static octave_value
el_div (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v2.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v1.integer_value () / d);
}

static octave_value
el_ldiv (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v1.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v2.integer_value () / d);
}

static octave_value
el_and (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () && v2.integer_value ());
}

static octave_value
el_or (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  return new octave_integer (v1.integer_value () || v2.integer_value ());
}

DEFUN_DLD (make_int, args, ,
  "int_val = make_int (val)\n\
\n\
Creates an integer variable from VAL.")
{
  static bool type_loaded = false;

  if (! type_loaded)
    {
      octave_integer::register_type ();

      cerr << "installing integer type at type-id = "
	   << octave_integer::static_type_id () << "\n";

      INSTALL_BINOP (add, octave_integer, octave_integer, add);
      INSTALL_BINOP (sub, octave_integer, octave_integer, sub);
      INSTALL_BINOP (mul, octave_integer, octave_integer, mul);
      INSTALL_BINOP (div, octave_integer, octave_integer, div);
      INSTALL_BINOP (ldiv, octave_integer, octave_integer, ldiv);
      INSTALL_BINOP (lt, octave_integer, octave_integer, lt);
      INSTALL_BINOP (le, octave_integer, octave_integer, le);
      INSTALL_BINOP (eq, octave_integer, octave_integer, eq);
      INSTALL_BINOP (ge, octave_integer, octave_integer, ge);
      INSTALL_BINOP (gt, octave_integer, octave_integer, gt);
      INSTALL_BINOP (ne, octave_integer, octave_integer, ne);
      INSTALL_BINOP (el_mul, octave_integer, octave_integer, el_mul);
      INSTALL_BINOP (el_div, octave_integer, octave_integer, el_div);
      INSTALL_BINOP (el_ldiv, octave_integer, octave_integer, el_ldiv);
      INSTALL_BINOP (el_and, octave_integer, octave_integer, el_and);
      INSTALL_BINOP (el_or, octave_integer, octave_integer, el_or);

      INSTALL_BINOP (div, octave_integer, octave_scalar, i_s_div);
    }

  octave_value retval;

  if (args.length () == 1)
    {
      double d = args(0).double_value ();

      if (! error_state)
	retval = octave_value (new octave_integer (NINT (d)));
    }
  else
    usage ("make_int");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
