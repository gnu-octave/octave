////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

//! @file op-class.cc
//! Unary and binary operators for classdef and old style classes.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-time.h"

#include "errwarn.h"
#include "interpreter-private.h"
#include "load-path.h"
#include "ovl.h"
#include "ov.h"
#include "ov-class.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "symtab.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

//! Default unary class operator.
//!
//! @param a operand
//! @param opname operator name

static octave_value
oct_unop_default (const octave_value& a, const std::string& opname)
{
  std::string class_name = a.class_name ();

  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  octave_value meth = symtab.find_method (opname, class_name);

  if (meth.is_defined ())
    {
      // Call overloaded unary class operator.
      octave_value_list tmp = octave::feval (meth.function_value (),
                                             ovl (a), 1);

      // Return first element if present.
      if (tmp.length () > 0)
        return tmp(0);

      return octave_value ();
    }

  // Matlab compatibility:  If (conjugate) transpose is not overloaded and
  // the number of dimensions is maximal two, just transpose the array of
  // that class.

  if ((opname == "transpose") || (opname == "ctranspose"))
    {
      if (a.ndims () > 2)
        error ("%s not defined for N-D objects of %s class", opname.c_str (),
               class_name.c_str ());

      if (a.is_classdef_object ())
        {
          // FIXME: Default transposition for classdef arrays.

          error ("%s method not defined for %s class", opname.c_str (),
                 class_name.c_str ());
        }
      else
        {
          const octave_class& v
            = dynamic_cast<const octave_class&> (a.get_rep ());

          return octave_value (v.map_value ().transpose (),
                               v.class_name (),
                               v.parent_class_name_list ());
        }
    }
  else
    error ("%s method not defined for %s class", opname.c_str (),
           class_name.c_str ());
}

//! Helper macro to define unary class operators.

#define DEF_CLASS_UNOP(opname)                 \
  static octave_value                          \
  oct_unop_ ## opname (const octave_value& a)  \
  {                                            \
    return oct_unop_default (a, #opname);      \
  }

DEF_CLASS_UNOP (not)         // !a or ~a
DEF_CLASS_UNOP (uplus)       // +a
DEF_CLASS_UNOP (uminus)      // -a
DEF_CLASS_UNOP (transpose)   //  a.'
DEF_CLASS_UNOP (ctranspose)  //  a'
#undef DEF_CLASS_UNOP

//! Default binary class operator.
//!
//! @param a1 first  operand
//! @param a2 second operand
//! @param opname operator name
//!
//! The operator precedence is as follows:
//!
//! 1.   If exactly one of the operands is a user defined class object, then
//!      the class method of that operand is invoked.
//!
//! 2.   If both operands are user defined class objects, then
//! 2.1  The superior class method is invoked.
//! 2.2  The leftmost class method is invoked if both classes are the same
//!      or their precedence is not defined by superiorto/inferiorto.

static octave_value
oct_binop_default (const octave_value& a1, const octave_value& a2,
                   const std::string& opname)
{
  octave::symbol_table& symtab = octave::__get_symbol_table__ ();

  // Dispatch to first (leftmost) operand by default.
  std::string dispatch_type = a1.class_name ();

  // Determine, if second operand takes precedence (see rules above).
  if (! a1.isobject ()
      || (a1.isobject () && a2.isobject ()
          && symtab.is_superiorto (a2.class_name (), dispatch_type)))
    dispatch_type = a2.class_name ();

  octave_value meth = symtab.find_method (opname, dispatch_type);

  if (meth.is_undefined ())
    error ("%s method not defined for %s class", opname.c_str (),
           dispatch_type.c_str ());

  octave_value_list tmp = octave::feval (meth.function_value (),
                                         ovl (a1, a2), 1);

  if (tmp.length () > 0)
    return tmp(0);

  return octave_value ();
}

//! Helper macro to define binary class operators.

#define DEF_CLASS_BINOP(opname)                                          \
  static octave_value                                                    \
  oct_binop_ ## opname (const octave_value& a1, const octave_value& a2)  \
  {                                                                      \
    return oct_binop_default (a1, a2, #opname);                          \
  }

DEF_CLASS_BINOP (plus)     // a1 + a2
DEF_CLASS_BINOP (minus)    // a1 - a2
DEF_CLASS_BINOP (mtimes)   // a1 * a2
DEF_CLASS_BINOP (mrdivide) // a1 / a2
DEF_CLASS_BINOP (mpower)   // a1 ^ a2
DEF_CLASS_BINOP (mldivide) // a1 \ a2
DEF_CLASS_BINOP (lt)       // a1 <  a2
DEF_CLASS_BINOP (le)       // a1 <= a2
DEF_CLASS_BINOP (eq)       // a1 <= a2
DEF_CLASS_BINOP (ge)       // a1 >= a2
DEF_CLASS_BINOP (gt)       // a1 >  a2
DEF_CLASS_BINOP (ne)       // a1 ~= a2 or a1 != a2
DEF_CLASS_BINOP (times)    // a1 .* a2
DEF_CLASS_BINOP (rdivide)  // a1 ./ a2
DEF_CLASS_BINOP (power)    // a1 .^ a2
DEF_CLASS_BINOP (ldivide)  // a1 .\ a2
DEF_CLASS_BINOP (and)      // a1 & a2
DEF_CLASS_BINOP (or)       // a1 | a2
#undef DEF_CLASS_BINOP

void
install_class_ops (octave::type_info& ti)
{
  ti.install_unary_class_op (octave_value::op_not,       oct_unop_not);
  ti.install_unary_class_op (octave_value::op_uplus,     oct_unop_uplus);
  ti.install_unary_class_op (octave_value::op_uminus,    oct_unop_uminus);
  ti.install_unary_class_op (octave_value::op_transpose, oct_unop_transpose);
  ti.install_unary_class_op (octave_value::op_hermitian, oct_unop_ctranspose);

  ti.install_binary_class_op (octave_value::op_add,     oct_binop_plus);
  ti.install_binary_class_op (octave_value::op_sub,     oct_binop_minus);
  ti.install_binary_class_op (octave_value::op_mul,     oct_binop_mtimes);
  ti.install_binary_class_op (octave_value::op_div,     oct_binop_mrdivide);
  ti.install_binary_class_op (octave_value::op_pow,     oct_binop_mpower);
  ti.install_binary_class_op (octave_value::op_ldiv,    oct_binop_mldivide);
  ti.install_binary_class_op (octave_value::op_lt,      oct_binop_lt);
  ti.install_binary_class_op (octave_value::op_le,      oct_binop_le);
  ti.install_binary_class_op (octave_value::op_eq,      oct_binop_eq);
  ti.install_binary_class_op (octave_value::op_ge,      oct_binop_ge);
  ti.install_binary_class_op (octave_value::op_gt,      oct_binop_gt);
  ti.install_binary_class_op (octave_value::op_ne,      oct_binop_ne);
  ti.install_binary_class_op (octave_value::op_el_mul,  oct_binop_times);
  ti.install_binary_class_op (octave_value::op_el_div,  oct_binop_rdivide);
  ti.install_binary_class_op (octave_value::op_el_pow,  oct_binop_power);
  ti.install_binary_class_op (octave_value::op_el_ldiv, oct_binop_ldivide);
  ti.install_binary_class_op (octave_value::op_el_and,  oct_binop_and);
  ti.install_binary_class_op (octave_value::op_el_or,   oct_binop_or);
}

OCTAVE_END_NAMESPACE(octave)
