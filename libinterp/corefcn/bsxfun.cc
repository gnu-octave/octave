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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <list>
#include <string>
#include <vector>

#include "lo-mappers.h"

#include "defun.h"
#include "interpreter.h"
#include "oct-map.h"
#include "ov-colon.h"
#include "ov-fcn-handle.h"
#include "parse.h"
#include "unwind-prot.h"
#include "variables.h"

// Optimized bsxfun operations
enum bsxfun_builtin_op
{
  bsxfun_builtin_plus = 0,
  bsxfun_builtin_minus,
  bsxfun_builtin_times,
  bsxfun_builtin_divide,
  bsxfun_builtin_max,
  bsxfun_builtin_min,
  bsxfun_builtin_eq,
  bsxfun_builtin_ne,
  bsxfun_builtin_lt,
  bsxfun_builtin_le,
  bsxfun_builtin_gt,
  bsxfun_builtin_ge,
  bsxfun_builtin_and,
  bsxfun_builtin_or,
  bsxfun_builtin_power,
  bsxfun_builtin_unknown,
  bsxfun_num_builtin_ops = bsxfun_builtin_unknown
};

const char *bsxfun_builtin_names[] =
{
  "plus",
  "minus",
  "times",
  "rdivide",
  "max",
  "min",
  "eq",
  "ne",
  "lt",
  "le",
  "gt",
  "ge",
  "and",
  "or",
  "power"
};

static bsxfun_builtin_op
bsxfun_builtin_lookup (const std::string& name)
{
  for (int i = 0; i < bsxfun_num_builtin_ops; i++)
    if (name == bsxfun_builtin_names[i])
      return static_cast<bsxfun_builtin_op> (i);

  return bsxfun_builtin_unknown;
}

typedef octave_value (*bsxfun_handler) (const octave_value&,
                                        const octave_value&);

// Static table of handlers.
bsxfun_handler bsxfun_handler_table[bsxfun_num_builtin_ops][btyp_num_types];

template <typename NDA, NDA (bsxfun_op) (const NDA&, const NDA&)>
static octave_value
bsxfun_forward_op (const octave_value& x, const octave_value& y)
{
  NDA xa = octave_value_extract<NDA> (x);
  NDA ya = octave_value_extract<NDA> (y);
  return octave_value (bsxfun_op (xa, ya));
}

template <typename NDA, boolNDArray (bsxfun_rel) (const NDA&, const NDA&)>
static octave_value
bsxfun_forward_rel (const octave_value& x, const octave_value& y)
{
  NDA xa = octave_value_extract<NDA> (x);
  NDA ya = octave_value_extract<NDA> (y);
  return octave_value (bsxfun_rel (xa, ya));
}

// pow() needs a special handler for reals
// because of the potentially complex result.
template <typename NDA, typename CNDA>
static octave_value
do_bsxfun_real_pow (const octave_value& x, const octave_value& y)
{
  NDA xa = octave_value_extract<NDA> (x);
  NDA ya = octave_value_extract<NDA> (y);
  if (! ya.all_integers () && xa.any_element_is_negative ())
    return octave_value (bsxfun_pow (CNDA (xa), ya));
  else
    return octave_value (bsxfun_pow (xa, ya));
}

static void maybe_fill_table (void)
{
  static bool filled = false;
  if (filled)
    return;

#define REGISTER_OP_HANDLER(OP, BTYP, NDA, FUNOP)                       \
  bsxfun_handler_table[OP][BTYP] = bsxfun_forward_op<NDA, FUNOP>

#define REGISTER_REL_HANDLER(REL, BTYP, NDA, FUNREL)                    \
  bsxfun_handler_table[REL][BTYP] = bsxfun_forward_rel<NDA, FUNREL>

#define REGISTER_STD_HANDLERS(BTYP, NDA)                                \
  REGISTER_OP_HANDLER (bsxfun_builtin_plus, BTYP, NDA, bsxfun_add);     \
  REGISTER_OP_HANDLER (bsxfun_builtin_minus, BTYP, NDA, bsxfun_sub);    \
  REGISTER_OP_HANDLER (bsxfun_builtin_times, BTYP, NDA, bsxfun_mul);    \
  REGISTER_OP_HANDLER (bsxfun_builtin_divide, BTYP, NDA, bsxfun_div);   \
  REGISTER_OP_HANDLER (bsxfun_builtin_max, BTYP, NDA, bsxfun_max);      \
  REGISTER_OP_HANDLER (bsxfun_builtin_min, BTYP, NDA, bsxfun_min);      \
  REGISTER_REL_HANDLER (bsxfun_builtin_eq, BTYP, NDA, bsxfun_eq);       \
  REGISTER_REL_HANDLER (bsxfun_builtin_ne, BTYP, NDA, bsxfun_ne);       \
  REGISTER_REL_HANDLER (bsxfun_builtin_lt, BTYP, NDA, bsxfun_lt);       \
  REGISTER_REL_HANDLER (bsxfun_builtin_le, BTYP, NDA, bsxfun_le);       \
  REGISTER_REL_HANDLER (bsxfun_builtin_gt, BTYP, NDA, bsxfun_gt);       \
  REGISTER_REL_HANDLER (bsxfun_builtin_ge, BTYP, NDA, bsxfun_ge)

  REGISTER_STD_HANDLERS (btyp_double, NDArray);
  REGISTER_STD_HANDLERS (btyp_float, FloatNDArray);
  REGISTER_STD_HANDLERS (btyp_complex, ComplexNDArray);
  REGISTER_STD_HANDLERS (btyp_float_complex, FloatComplexNDArray);
  REGISTER_STD_HANDLERS (btyp_int8,  int8NDArray);
  REGISTER_STD_HANDLERS (btyp_int16, int16NDArray);
  REGISTER_STD_HANDLERS (btyp_int32, int32NDArray);
  REGISTER_STD_HANDLERS (btyp_int64, int64NDArray);
  REGISTER_STD_HANDLERS (btyp_uint8,  uint8NDArray);
  REGISTER_STD_HANDLERS (btyp_uint16, uint16NDArray);
  REGISTER_STD_HANDLERS (btyp_uint32, uint32NDArray);
  REGISTER_STD_HANDLERS (btyp_uint64, uint64NDArray);

  // For bools, we register and/or.
  REGISTER_OP_HANDLER (bsxfun_builtin_and, btyp_bool, boolNDArray, bsxfun_and);
  REGISTER_OP_HANDLER (bsxfun_builtin_or, btyp_bool, boolNDArray, bsxfun_or);

  // Register power handlers.
  bsxfun_handler_table[bsxfun_builtin_power][btyp_double]
    = do_bsxfun_real_pow<NDArray, ComplexNDArray>;
  bsxfun_handler_table[bsxfun_builtin_power][btyp_float]
    = do_bsxfun_real_pow<FloatNDArray, FloatComplexNDArray>;

  REGISTER_OP_HANDLER (bsxfun_builtin_power, btyp_complex, ComplexNDArray,
                       bsxfun_pow);
  REGISTER_OP_HANDLER (bsxfun_builtin_power, btyp_float_complex,
                       FloatComplexNDArray, bsxfun_pow);

  // For chars, we want just relational handlers.
  REGISTER_REL_HANDLER (bsxfun_builtin_eq, btyp_char, charNDArray, bsxfun_eq);
  REGISTER_REL_HANDLER (bsxfun_builtin_ne, btyp_char, charNDArray, bsxfun_ne);
  REGISTER_REL_HANDLER (bsxfun_builtin_lt, btyp_char, charNDArray, bsxfun_lt);
  REGISTER_REL_HANDLER (bsxfun_builtin_le, btyp_char, charNDArray, bsxfun_le);
  REGISTER_REL_HANDLER (bsxfun_builtin_gt, btyp_char, charNDArray, bsxfun_gt);
  REGISTER_REL_HANDLER (bsxfun_builtin_ge, btyp_char, charNDArray, bsxfun_ge);

  filled = true;
}

static octave_value
maybe_optimized_builtin (const std::string& name,
                         const octave_value& a, const octave_value& b)
{
  octave_value retval;

  maybe_fill_table ();

  bsxfun_builtin_op op = bsxfun_builtin_lookup (name);
  if (op != bsxfun_builtin_unknown)
    {
      builtin_type_t btyp_a = a.builtin_type ();
      builtin_type_t btyp_b = b.builtin_type ();

      // Simplify single/double combinations.
      if (btyp_a == btyp_float && btyp_b == btyp_double)
        btyp_b = btyp_float;
      else if (btyp_a == btyp_double && btyp_b == btyp_float)
        btyp_a = btyp_float;
      else if (btyp_a == btyp_float_complex && btyp_b == btyp_complex)
        btyp_b = btyp_float_complex;
      else if (btyp_a == btyp_complex && btyp_b == btyp_float_complex)
        btyp_a = btyp_float_complex;

      if (btyp_a == btyp_b && btyp_a != btyp_unknown)
        {
          bsxfun_handler handler = bsxfun_handler_table[op][btyp_a];
          if (handler)
            retval = handler (a, b);
        }
    }

  return retval;
}

static bool
maybe_update_column (octave_value& Ac, const octave_value& A,
                     const dim_vector& dva, const dim_vector& dvc,
                     octave_idx_type i, octave_value_list& idx)
{
  octave_idx_type nd = dva.ndims ();

  if (i == 0)
    {
      idx(0) = octave_value (':');
      for (octave_idx_type j = 1; j < nd; j++)
        {
          if (dva(j) == 1)
            idx(j) = octave_value (1);
          else
            idx(j) = octave_value ((i % dvc(j)) + 1);

          i /= dvc(j);
        }

      Ac = A;
      Ac = Ac.single_subsref ("(", idx);
      return true;
    }
  else
    {
      bool is_changed = false;
      octave_idx_type k = i;
      octave_idx_type k1 = i - 1;
      for (octave_idx_type j = 1; j < nd; j++)
        {
          if (dva(j) != 1 && k % dvc(j) != k1 % dvc(j))
            {
              idx (j) = octave_value ((k % dvc(j)) + 1);
              is_changed = true;
            }

          k /= dvc(j);
          k1 /= dvc(j);
        }

      if (is_changed)
        {
          Ac = A;
          Ac = Ac.single_subsref ("(", idx);
          return true;
        }
      else
        return false;
    }
}

#if 0
// FIXME: this function is not used; is it OK to delete it?
static void
update_index (octave_value_list& idx, const dim_vector& dv, octave_idx_type i)
{
  octave_idx_type nd = dv.ndims ();

  if (i == 0)
    {
      for (octave_idx_type j = nd - 1; j > 0; j--)
        idx(j) = octave_value (1.0);
      idx(0) = octave_value (':');
    }
  else
    {
      for (octave_idx_type j = 1; j < nd; j++)
        {
          idx (j) = octave_value (i % dv(j) + 1);
          i /= dv(j);
        }
    }
}
#endif

static void
update_index (Array<int>& idx, const dim_vector& dv, octave_idx_type i)
{
  octave_idx_type nd = dv.ndims ();

  idx(0) = 0;
  for (octave_idx_type j = 1; j < nd; j++)
    {
      idx(j) = i % dv(j);
      i /= dv(j);
    }
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFMETHOD (bsxfun, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} bsxfun (@var{f}, @var{A}, @var{B})
Apply a binary function @var{f} element-by-element to two array arguments
@var{A} and @var{B}, expanding singleton dimensions in either input argument as
necessary.

@var{f} is a function handle, inline function, or string containing the name
of the function to evaluate.  The function @var{f} must be capable of accepting
two column-vector arguments of equal length, or one column vector argument and
a scalar.

The dimensions of @var{A} and @var{B} must be equal or singleton.  The
singleton dimensions of the arrays will be expanded to the same dimensionality
as the other array.
@seealso{arrayfun, cellfun}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_value fcn = args(0);
  if (fcn.is_string ())
    {
      std::string name = fcn.string_value ();

      symbol_table& symtab = interp.get_symbol_table ();

      fcn = symtab.find_function (name);

      if (fcn.is_undefined ())
        error ("bsxfun: invalid function name: %s", name.c_str ());
    }
  else if (! (args(0).is_function_handle () || args(0).is_inline_function ()))
    error ("bsxfun: F must be a string or function handle");

  octave_value_list retval;

  const octave_value A = args(1);
  const octave_value B = args(2);

  if (fcn.is_builtin_function ()
      || (fcn.is_function_handle () && ! A.isobject () && ! B.isobject ()))
    {
      // This may break if the default behavior is overridden.  But if you
      // override arithmetic operators for builtin classes, you should expect
      // mayhem anyway (constant folding etc).  Querying is_overloaded() may
      // not be exactly what we need here.
      octave_function *fcn_val = fcn.function_value ();
      if (fcn_val)
        {
          octave_value tmp = maybe_optimized_builtin (fcn_val->name (), A, B);
          if (tmp.is_defined ())
            retval(0) = tmp;
        }
    }

  if (retval.empty ())
    {
      dim_vector dva = A.dims ();
      octave_idx_type nda = dva.ndims ();
      dim_vector dvb = B.dims ();
      octave_idx_type ndb = dvb.ndims ();
      octave_idx_type nd = nda;

      if (nda > ndb)
        dvb.resize (nda, 1);
      else if (nda < ndb)
        {
          dva.resize (ndb, 1);
          nd = ndb;
        }

      for (octave_idx_type i = 0; i < nd; i++)
        if (dva(i) != dvb(i) && dva(i) != 1 && dvb(i) != 1)
          error ("bsxfun: dimensions of A and B must match");

      // Find the size of the output
      dim_vector dvc;
      dvc.resize (nd);

      for (octave_idx_type i = 0; i < nd; i++)
        dvc(i) = (dva(i) < 1 ? dva(i)
                  : (dvb(i) < 1 ? dvb(i)
                     : (dva(i) > dvb(i) ? dva(i)
                        : dvb(i))));

      if (dva == dvb || dva.numel () == 1 || dvb.numel () == 1)
        {
          octave_value_list inputs (2);
          inputs(0) = A;
          inputs(1) = B;
          retval = feval (fcn, inputs, 1);
        }
      else if (dvc.numel () < 1)
        {
          octave_value_list inputs (2);
          inputs(0) = A.resize (dvc);
          inputs(1) = B.resize (dvc);
          retval = feval (fcn, inputs, 1);
        }
      else
        {
          octave_idx_type ncount = 1;
          for (octave_idx_type i = 1; i < nd; i++)
            ncount *= dvc(i);

#define BSXDEF(T)                               \
          T result_ ## T;                       \
          bool have_ ## T = false;

          BSXDEF(NDArray);
          BSXDEF(ComplexNDArray);
          BSXDEF(FloatNDArray);
          BSXDEF(FloatComplexNDArray);
          BSXDEF(boolNDArray);
          BSXDEF(int8NDArray);
          BSXDEF(int16NDArray);
          BSXDEF(int32NDArray);
          BSXDEF(int64NDArray);
          BSXDEF(uint8NDArray);
          BSXDEF(uint16NDArray);
          BSXDEF(uint32NDArray);
          BSXDEF(uint64NDArray);

          octave_value Ac;
          octave_value_list idxA;
          octave_value Bc;
          octave_value_list idxB;
          octave_value C;
          octave_value_list inputs (2);
          Array<int> ra_idx (dim_vector (dvc.ndims (), 1), 0);

          for (octave_idx_type i = 0; i < ncount; i++)
            {
              if (maybe_update_column (Ac, A, dva, dvc, i, idxA))
                inputs(0) = Ac;

              if (maybe_update_column (Bc, B, dvb, dvc, i, idxB))
                inputs(1) = Bc;

              octave_value_list tmp = feval (fcn, inputs, 1);

#define BSXINIT(T, CLS, EXTRACTOR)                                      \
              (result_type == CLS)                                      \
                {                                                       \
                  have_ ## T = true;                                    \
                  result_ ## T = tmp(0). EXTRACTOR ## _array_value ();  \
                  result_ ## T .resize (dvc);                           \
                }

              if (i == 0)
                {
                  if (! tmp(0).issparse ())
                    {
                      std::string result_type = tmp(0).class_name ();
                      if (result_type == "double")
                        {
                          if (tmp(0).isreal ())
                            {
                              have_NDArray = true;
                              result_NDArray = tmp(0).array_value ();
                              result_NDArray.resize (dvc);
                            }
                          else
                            {
                              have_ComplexNDArray = true;
                              result_ComplexNDArray
                                = tmp(0).complex_array_value ();
                              result_ComplexNDArray.resize (dvc);
                            }
                        }
                      else if (result_type == "single")
                        {
                          if (tmp(0).isreal ())
                            {
                              have_FloatNDArray = true;
                              result_FloatNDArray
                                = tmp(0).float_array_value ();
                              result_FloatNDArray.resize (dvc);
                            }
                          else
                            {
                              have_FloatComplexNDArray = true;
                              result_FloatComplexNDArray
                                = tmp(0).float_complex_array_value ();
                              result_FloatComplexNDArray.resize (dvc);
                            }
                        }
                      else if BSXINIT(boolNDArray, "logical", bool)
                        else if BSXINIT(int8NDArray, "int8", int8)
                          else if BSXINIT(int16NDArray, "int16", int16)
                            else if BSXINIT(int32NDArray, "int32", int32)
                              else if BSXINIT(int64NDArray, "int64", int64)
                                else if BSXINIT(uint8NDArray, "uint8", uint8)
                                  else if BSXINIT(uint16NDArray, "uint16", uint16)
                                    else if BSXINIT(uint32NDArray, "uint32", uint32)
                                      else if BSXINIT(uint64NDArray, "uint64", uint64)
                                        else
                                          {
                                            C = tmp(0);
                                            C = C.resize (dvc);
                                          }
                    }
                  else  // Skip semi-fast path for sparse matrices
                    {
                      C = tmp (0);
                      C = C.resize (dvc);
                    }
                }
              else
                {
                  update_index (ra_idx, dvc, i);

                  if (have_NDArray)
                    {
                      if (! tmp(0).isfloat ())
                        {
                          have_NDArray = false;
                          C = result_NDArray;
                          C = cat_op (C, tmp(0), ra_idx);
                        }
                      else if (tmp(0).isreal ())
                        result_NDArray.insert (tmp(0).array_value (), ra_idx);
                      else
                        {
                          result_ComplexNDArray
                            = ComplexNDArray (result_NDArray);
                          result_ComplexNDArray.insert
                          (tmp(0).complex_array_value (), ra_idx);
                          have_NDArray = false;
                          have_ComplexNDArray = true;
                        }
                    }
                  else if (have_FloatNDArray)
                    {
                      if (! tmp(0).isfloat ())
                        {
                          have_FloatNDArray = false;
                          C = result_FloatNDArray;
                          C = cat_op (C, tmp(0), ra_idx);
                        }
                      else if (tmp(0).isreal ())
                        result_FloatNDArray.insert
                        (tmp(0).float_array_value (), ra_idx);
                      else
                        {
                          result_FloatComplexNDArray
                            = FloatComplexNDArray (result_FloatNDArray);
                          result_FloatComplexNDArray.insert
                          (tmp(0).float_complex_array_value (), ra_idx);
                          have_FloatNDArray = false;
                          have_FloatComplexNDArray = true;
                        }
                    }

#define BSXLOOP(T, CLS, EXTRACTOR)                                      \
                  (have_ ## T)                                          \
                    {                                                   \
                      if (tmp(0).class_name () != CLS)                  \
                        {                                               \
                          have_ ## T = false;                           \
                          C = result_ ## T;                             \
                          C = cat_op (C, tmp(0), ra_idx);       \
                        }                                               \
                      else                                              \
                        result_ ## T .insert (tmp(0). EXTRACTOR ## _array_value (), ra_idx); \
                    }

                  else if BSXLOOP(ComplexNDArray, "double", complex)
                    else if BSXLOOP(FloatComplexNDArray, "single", float_complex)
                      else if BSXLOOP(boolNDArray, "logical", bool)
                        else if BSXLOOP(int8NDArray, "int8", int8)
                          else if BSXLOOP(int16NDArray, "int16", int16)
                            else if BSXLOOP(int32NDArray, "int32", int32)
                              else if BSXLOOP(int64NDArray, "int64", int64)
                                else if BSXLOOP(uint8NDArray, "uint8", uint8)
                                  else if BSXLOOP(uint16NDArray, "uint16", uint16)
                                    else if BSXLOOP(uint32NDArray, "uint32", uint32)
                                      else if BSXLOOP(uint64NDArray, "uint64", uint64)
                                        else
                                          C = cat_op (C, tmp(0), ra_idx);
                }
            }

#define BSXEND(T)                               \
          (have_ ## T)                          \
            retval(0) = result_ ## T;

          if BSXEND(NDArray)
            else if BSXEND(ComplexNDArray)
              else if BSXEND(FloatNDArray)
                else if BSXEND(FloatComplexNDArray)
                  else if BSXEND(boolNDArray)
                    else if BSXEND(int8NDArray)
                      else if BSXEND(int16NDArray)
                        else if BSXEND(int32NDArray)
                          else if BSXEND(int64NDArray)
                            else if BSXEND(uint8NDArray)
                              else if BSXEND(uint16NDArray)
                                else if BSXEND(uint32NDArray)
                                  else if BSXEND(uint64NDArray)
                                    else
                                      retval(0) = C;
        }
    }

  return retval;
}

/*

%!shared a, b, c, f
%! a = randn (4, 4);
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error bsxfun (f)
%!error bsxfun (f, a)
%!error bsxfun (a, b)
%!error bsxfun (a, b, c)
%!error bsxfun (f, a, b, c)
%!error bsxfun (f, ones (4, 0), ones (4, 4))
%!assert (bsxfun (f, ones (4, 0), ones (4, 1)), zeros (4, 0))
%!assert (bsxfun (f, ones (1, 4), ones (4, 1)), zeros (4, 4))
%!assert (bsxfun (f, a, b), a - repmat (b, 4, 1))
%!assert (bsxfun (f, a, c), a - repmat (c, 1, 4))
%!assert (bsxfun ("minus", ones (1, 4), ones (4, 1)), zeros (4, 4))

%!shared a, b, c, f
%! a = randn (4, 4);
%! a(1) *= 1i;
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error bsxfun (f)
%!error bsxfun (f, a)
%!error bsxfun (a, b)
%!error bsxfun (a, b, c)
%!error bsxfun (f, a, b, c)
%!error bsxfun (f, ones (4, 0), ones (4, 4))
%!assert (bsxfun (f, ones (4, 0), ones (4, 1)), zeros (4, 0))
%!assert (bsxfun (f, ones (1, 4), ones (4, 1)), zeros (4, 4))
%!assert (bsxfun (f, a, b), a - repmat (b, 4, 1))
%!assert (bsxfun (f, a, c), a - repmat (c, 1, 4))
%!assert (bsxfun ("minus", ones (1, 4), ones (4, 1)), zeros (4, 4))

%!shared a, b, c, f
%! a = randn (4, 4);
%! a(end) *= 1i;
%! b = mean (a, 1);
%! c = mean (a, 2);
%! f = @minus;
%!error bsxfun (f)
%!error bsxfun (f, a)
%!error bsxfun (a, b)
%!error bsxfun (a, b, c)
%!error bsxfun (f, a, b, c)
%!error bsxfun (f, ones (4, 0), ones (4, 4))
%!assert (bsxfun (f, ones (4, 0), ones (4, 1)), zeros (4, 0))
%!assert (bsxfun (f, ones (1, 4), ones (4, 1)), zeros (4, 4))
%!assert (bsxfun (f, a, b), a - repmat (b, 4, 1))
%!assert (bsxfun (f, a, c), a - repmat (c, 1, 4))
%!assert (bsxfun ("minus", ones (1, 4), ones (4, 1)), zeros (4, 4))

%!shared a, b, c, f
%! a = randn (4, 4);
%! b = a (1, :);
%! c = a (:, 1);
%! f = @(x, y) x == y;
%!error bsxfun (f)
%!error bsxfun (f, a)
%!error bsxfun (a, b)
%!error bsxfun (a, b, c)
%!error bsxfun (f, a, b, c)
%!error bsxfun (f, ones (4, 0), ones (4, 4))
%!assert (bsxfun (f, ones (4, 0), ones (4, 1)), zeros (4, 0, "logical"))
%!assert (bsxfun (f, ones (1, 4), ones (4, 1)), ones (4, 4, "logical"))
%!assert (bsxfun (f, a, b), a == repmat (b, 4, 1))
%!assert (bsxfun (f, a, c), a == repmat (c, 1, 4))

%!shared a, b, c, d, f
%! a = randn (4, 4, 4);
%! b = mean (a, 1);
%! c = mean (a, 2);
%! d = mean (a, 3);
%! f = @minus;
%!error bsxfun (f, ones ([4, 0, 4]), ones ([4, 4, 4]))
%!assert (bsxfun (f, ones ([4, 0, 4]), ones ([4, 1, 4])), zeros ([4, 0, 4]))
%!assert (bsxfun (f, ones ([4, 4, 0]), ones ([4, 1, 1])), zeros ([4, 4, 0]))
%!assert (bsxfun (f, ones ([1, 4, 4]), ones ([4, 1, 4])), zeros ([4, 4, 4]))
%!assert (bsxfun (f, ones ([4, 4, 1]), ones ([4, 1, 4])), zeros ([4, 4, 4]))
%!assert (bsxfun (f, ones ([4, 1, 4]), ones ([1, 4, 4])), zeros ([4, 4, 4]))
%!assert (bsxfun (f, ones ([4, 1, 4]), ones ([1, 4, 1])), zeros ([4, 4, 4]))
%!assert (bsxfun (f, a, b), a - repmat (b, [4, 1, 1]))
%!assert (bsxfun (f, a, c), a - repmat (c, [1, 4, 1]))
%!assert (bsxfun (f, a, d), a - repmat (d, [1, 1, 4]))
%!assert (bsxfun ("minus", ones ([4, 0, 4]), ones ([4, 1, 4])),
%!        zeros ([4, 0, 4]))

## The test below is a very hard case to treat
%!assert (bsxfun (f, ones ([4, 1, 4, 1]), ones ([1, 4, 1, 4])),
%!        zeros ([4, 4, 4, 4]))

%!shared a, b, aa, bb
%! ## FIXME: Set a known "good" random seed.  See bug #51779.
%! old_nstate = randn ("state");
%! restore_nstate = onCleanup (@() randn ("state", old_nstate));
%! randn ("state", 42); # initialize generator to make behavior reproducible
%! a = randn (3, 1, 3);
%! aa = a(:, ones (1, 3), :, ones (1, 3));
%! b = randn (1, 3, 3, 3);
%! bb = b(ones (1, 3), :, :, :);
%!assert (bsxfun (@plus, a, b), aa + bb)
%!assert (bsxfun (@minus, a, b), aa - bb)
%!assert (bsxfun (@times, a, b), aa .* bb)
%!assert (bsxfun (@rdivide, a, b), aa ./ bb)
%!assert (bsxfun (@ldivide, a, b), aa .\ bb)
%!assert (bsxfun (@power, a, b), aa .^ bb)
%!assert (bsxfun (@power, abs (a), b), abs (aa) .^ bb)
%!assert (bsxfun (@eq, round (a), round (b)), round (aa) == round (bb))
%!assert (bsxfun (@ne, round (a), round (b)), round (aa) != round (bb))
%!assert (bsxfun (@lt, a, b), aa < bb)
%!assert (bsxfun (@le, a, b), aa <= bb)
%!assert (bsxfun (@gt, a, b), aa > bb)
%!assert (bsxfun (@ge, a, b), aa >= bb)
%!assert (bsxfun (@min, a, b), min (aa, bb))
%!assert (bsxfun (@max, a, b), max (aa, bb))
%!assert (bsxfun (@and, a > 0, b > 0), (aa > 0) & (bb > 0))
%!assert (bsxfun (@or, a > 0, b > 0), (aa > 0) | (bb > 0))

## Test automatic bsxfun
%
%!test
%! fcns = {@plus, @minus, @times, @rdivide, @ldivide, @power, @max, @min, ...
%!         @rem, @mod, @atan2, @hypot, @eq, @ne, @lt, @le, @gt, @ge, ...
%!         @and, @or, @xor };
%!
%! float_types = {@single, @double};
%! int_types = {@int8, @int16, @int32, @int64, ...
%!              @uint8, @uint16, @uint32, @uint64};
%!
%! ## FIXME: Set a known "good" random seed.  See bug #51779.
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42);  # initialize generator to make behavior reproducible
%!
%! x = rand (3) * 10-5;
%! y = rand (3,1) * 10-5;
%!
%! for i=1:length (fcns)
%!   for j = 1:length (float_types)
%!     for k = 1:length (int_types)
%!
%!       fcn = fcns{i};
%!       f_type = float_types{j};
%!       i_type = int_types{k};
%!
%!         assert (bsxfun (fcn, f_type (x), i_type (y)), ...
%!                 fcn (f_type(x), i_type (y)));
%!         assert (bsxfun (fcn, f_type (y), i_type (x)), ...
%!                 fcn (f_type(y), i_type (x)));
%!
%!         assert (bsxfun (fcn, i_type (x), i_type (y)), ...
%!                 fcn (i_type (x), i_type (y)));
%!         assert (bsxfun (fcn, i_type (y), i_type (x)), ...
%!                 fcn (i_type (y), i_type (x)));
%!
%!         assert (bsxfun (fcn, f_type (x), f_type (y)), ...
%!                 fcn (f_type (x), f_type (y)));
%!         assert (bsxfun (fcn, f_type(y), f_type(x)), ...
%!                 fcn (f_type (y), f_type (x)));
%!     endfor
%!   endfor
%! endfor

## Automatic broadcasting with zero length dimensions
%!assert <*47085> ([1 2 3] + zeros (0, 3), zeros (0, 3))
%!assert <*47085> (rand (3, 3, 1) + rand (3, 3, 0), zeros (3, 3, 0))

## In-place broadcasting with zero length dimensions
%!test <*47085>
%! a = zeros (0, 3);
%! a += [1 2 3];
%! assert (a, zeros (0, 3));

%!test <*53179>
%! im = ones (4,4,2) + single (i);
%! mask = true (4,4);
%! mask(:,1:2) = false;
%! r = bsxfun (@times, im, mask);
%! assert (r(:,:,1), repmat (single ([0, 0, 1+i, 1+i]), [4, 1]));

## automatic broadcasting with inplace times operator
%!test <*38466>
%! a = ones (2, 2, 2);
%! b = 2 * ones (2, 1);
%! a .*= b;
%! assert (a, 2 * ones (2, 2, 2));

%!test <*38466>
%! a = ones (2, 2, 2);
%! b = 2 * ones (1, 2);
%! a .*= b;
%! assert (a, 2 * ones (2, 2, 2));

%!test <*38466>
%! a = ones (2, 2, 2);
%! b = 2 * ones (2, 2);
%! a .*= b;
%! assert (a, 2 * ones (2, 2, 2));

%!test <*38466>
%! a = ones (2, 2, 2);
%! b = 2 * ones (1, 1, 2);
%! a .*= b;
%! assert (a, 2 * ones (2, 2, 2));

%!assert (ones (2,2,2) .* ones (1,2), ones (2,2,2));

*/

OCTAVE_END_NAMESPACE(octave)
