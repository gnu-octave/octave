#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <ostream>
#include <string>

#include <octave/lo-mappers.h>
#include <octave/lo-utils.h>
#include <octave/mx-base.h>
#include <octave/str-vec.h>

#include <octave/defun-dld.h>
#include <octave/interpreter.h>
#include <octave/ops.h>
#include <octave/ov-base.h>
#include <octave/ov-scalar.h>
#include <octave/ov-typeinfo.h>
#include <octave/ov.h>
#include <octave/ovl.h>
#include <octave/pager.h>
#include <octave/pr-output.h>
#include <octave/variables.h>


// Integer values.

// Derive from octave_base_dld_value instead of octave_base_value
// so that octave_integer values created by that dynamically loaded
// make_int.oct file will be tracked automatically and the .oct file
// will not be closed until all functions and values that it creates are
// deleted.

class
octave_integer : public octave_base_dld_value
{
public:

  octave_integer (void)
    : octave_base_dld_value (), scalar (0) { }

  octave_integer (int i)
    : octave_base_dld_value (), scalar (i) { }

  octave_integer (const octave_integer& s)
    : octave_base_dld_value (), scalar (s.scalar) { }

  ~octave_integer (void) = default;

  octave_base_value * clone (void) { return new octave_integer (*this); }

#if 0
  void *operator new (std::size_t size);
  void operator delete (void *p, std::size_t size);
#endif

  octave::idx_vector index_vector (bool) const
  { return octave::idx_vector ((double) scalar); }

  int rows (void) const { return 1; }
  int columns (void) const { return 1; }

  bool is_constant (void) const { return true; }

  bool is_defined (void) const { return true; }
  bool is_real_scalar (void) const { return true; }

  octave_value all (void) const { return (double) (scalar != 0); }
  octave_value any (void) const { return (double) (scalar != 0); }

  bool is_real_type (void) const { return true; }
  bool is_scalar_type (void) const { return true; }
  bool isnumeric (void) const { return true; }

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
  { return ComplexMatrix (1, 1, Complex (scalar)); }

  octave_value gnot (void) const { return octave_value ((double) ! scalar); }

  octave_value uminus (void) const { return new octave_integer (- scalar); }

  octave_value transpose (void) const { return new octave_integer (scalar); }

  octave_value hermitian (void) const { return new octave_integer (scalar); }

  void increment (void) { ++scalar; }

  void decrement (void) { --scalar; }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

private:

  int scalar;


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

void
octave_integer::print (std::ostream& os, bool pr_as_read_syntax)
{
  os << scalar;
  newline (os);
}

#if defined (DEFUNOP_OP)
#undef DEFUNOP_OP
#endif

#define DEFUNOP_OP(name, t, op)                                         \
  static octave_value                                                   \
  CONCAT2(oct_unop_, name) (const octave_base_value& a)                 \
  {                                                                     \
    const octave_ ## t& v = dynamic_cast<const octave_ ## t&> (a);      \
    return octave_value (new octave_integer (op v.t ## _value ()));     \
  }

DEFUNOP_OP (gnot, integer, !)
DEFUNOP_OP (uminus, integer, -)
DEFUNOP_OP (transpose, integer, /* no-op */)
DEFUNOP_OP (hermitian, integer, /* no-op */)

DEFNCUNOP_METHOD (incr, integer, increment)
DEFNCUNOP_METHOD (decr, integer, decrement)

#if defined (DEFBINOP_OP)
#undef DEFBINOP_OP
#endif

#define DEFBINOP_OP(name, t1, t2, op)                                   \
  static octave_value                                                   \
  CONCAT2(oct_binop_, name) (const octave_base_value& a1,               \
                             const octave_base_value& a2)               \
  {                                                                     \
    const octave_ ## t1& v1 = dynamic_cast<const octave_ ## t1&> (a1);  \
    const octave_ ## t2& v2 = dynamic_cast<const octave_ ## t2&> (a2);  \
    return octave_value                                                 \
      (new octave_integer (v1.t1 ## _value () op v2.t2 ## _value ()));  \
  }

// integer by integer ops.

DEFBINOP_OP (add, integer, integer, +)
DEFBINOP_OP (sub, integer, integer, -)
DEFBINOP_OP (mul, integer, integer, *)

DEFBINOP (div, integer, integer)
{
  const octave_integer& v1 = dynamic_cast<const octave_integer&> (a1);
  const octave_integer& v2 = dynamic_cast<const octave_integer&> (a2);

  return new octave_integer (v1.integer_value () / v2.integer_value ());
}


DEFBINOP (i_s_div, integer, scalar)
{
  const octave_integer& v1 = dynamic_cast<const octave_integer&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  return new octave_scalar (v1.double_value () / v2.double_value ());
}

DEFBINOP (ldiv, integer, integer)
{
  const octave_integer& v1 = dynamic_cast<const octave_integer&> (a1);
  const octave_integer& v2 = dynamic_cast<const octave_integer&> (a2);

  return new octave_integer (v2.integer_value () / v1.integer_value ());
}

DEFBINOP_OP (lt, integer, integer, <)
DEFBINOP_OP (le, integer, integer, <=)
DEFBINOP_OP (eq, integer, integer, ==)
DEFBINOP_OP (ge, integer, integer, >=)
DEFBINOP_OP (gt, integer, integer, >)
DEFBINOP_OP (ne, integer, integer, !=)

DEFBINOP_OP (el_mul, integer, integer, !=)

DEFBINOP (el_div, integer, integer)
{
  const octave_integer& v1 = dynamic_cast<const octave_integer&> (a1);
  const octave_integer& v2 = dynamic_cast<const octave_integer&> (a2);

  return new octave_integer (v1.integer_value () / v2.integer_value ());
}

DEFBINOP (el_ldiv, integer, integer)
{
  const octave_integer& v1 = dynamic_cast<const octave_integer&> (a1);
  const octave_integer& v2 = dynamic_cast<const octave_integer&> (a2);

  return new octave_integer (v2.integer_value () / v1.integer_value ());
}

DEFBINOP_OP (el_and, integer, integer, &&)
DEFBINOP_OP (el_or, integer, integer, ||)

DEFMETHOD_DLD (make_int, interp, args, ,
               "int_val = make_int (val)\n\
\n\
Creates an integer variable from VAL.")
{
  static bool type_loaded = false;

  if (! type_loaded)
    {
      octave_integer::register_type ();

      octave_stdout << "installing integer type at type-id = "
                    << octave_integer::static_type_id () << "\n";

      octave::type_info& ti = interp.get_type_info ();

      INSTALL_UNOP_TI (ti, op_not, octave_integer, gnot);
      INSTALL_UNOP_TI (ti, op_uminus, octave_integer, uminus);
      INSTALL_UNOP_TI (ti, op_transpose, octave_integer, transpose);
      INSTALL_UNOP_TI (ti, op_hermitian, octave_integer, hermitian);

      INSTALL_NCUNOP_TI (ti, op_incr, octave_integer, incr);
      INSTALL_NCUNOP_TI (ti, op_decr, octave_integer, decr);

      INSTALL_BINOP_TI (ti, op_add, octave_integer, octave_integer, add);
      INSTALL_BINOP_TI (ti, op_sub, octave_integer, octave_integer, sub);
      INSTALL_BINOP_TI (ti, op_mul, octave_integer, octave_integer, mul);
      INSTALL_BINOP_TI (ti, op_div, octave_integer, octave_integer, div);
      INSTALL_BINOP_TI (ti, op_ldiv, octave_integer, octave_integer, ldiv);
      INSTALL_BINOP_TI (ti, op_lt, octave_integer, octave_integer, lt);
      INSTALL_BINOP_TI (ti, op_le, octave_integer, octave_integer, le);
      INSTALL_BINOP_TI (ti, op_eq, octave_integer, octave_integer, eq);
      INSTALL_BINOP_TI (ti, op_ge, octave_integer, octave_integer, ge);
      INSTALL_BINOP_TI (ti, op_gt, octave_integer, octave_integer, gt);
      INSTALL_BINOP_TI (ti, op_ne, octave_integer, octave_integer, ne);
      INSTALL_BINOP_TI (ti, op_el_mul, octave_integer, octave_integer, el_mul);
      INSTALL_BINOP_TI (ti, op_el_div, octave_integer, octave_integer, el_div);
      INSTALL_BINOP_TI (ti, op_el_ldiv, octave_integer, octave_integer, el_ldiv);
      INSTALL_BINOP_TI (ti, op_el_and, octave_integer, octave_integer, el_and);
      INSTALL_BINOP_TI (ti, op_el_or, octave_integer, octave_integer, el_or);

      INSTALL_BINOP_TI (ti, op_div, octave_integer, octave_scalar, i_s_div);

      type_loaded = true;
    }

  octave_value retval;

  if (args.length () == 1)
    {
      double d = args(0).double_value ();

      retval = octave_value (new octave_integer (octave::math::nint (d)));
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (doit, args, ,
           "doit (I)")
{
  octave_value_list retval;

  if (args.length () != 1)
    {
      print_usage ();
      return retval;
    }

  if (args(0).type_id () == octave_integer::static_type_id ())
    {
      // At this point, we know we have a handle for an octave_integer
      // object, so we can peek at the representation and extract the
      // data.

      const octave_base_value& rep = args(0).get_rep ();

      int my_value = ((const octave_integer&) rep) . integer_value ();

      message ("doit", "your lucky number is: %d", my_value);
    }
  else
    err_wrong_type_arg ("doit", args(0));

  return retval;
}


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_integer, "integer", "integer");
