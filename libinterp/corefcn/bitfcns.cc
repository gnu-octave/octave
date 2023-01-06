////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2023 The Octave Project Developers
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

#include <limits>

#include "str-vec.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ov.h"
#include "ov-uint64.h"
#include "ov-uint32.h"
#include "ov-uint16.h"
#include "ov-uint8.h"
#include "ov-int64.h"
#include "ov-int32.h"
#include "ov-int16.h"
#include "ov-int8.h"
#include "ov-float.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-bool.h"

#include <functional>

#if ! defined (HAVE_CXX_BITWISE_OP_TEMPLATES)
OCTAVE_BEGIN_NAMESPACE(std)

template <typename T>
struct bit_and
{
public:
  T operator() (const T& op1, const T& op2) const { return (op1 & op2); }
};

template <typename T>
struct bit_or
{
public:
  T operator() (const T& op1, const T& op2) const { return (op1 | op2); }
};

template <typename T>
struct bit_xor
{
public:
  T operator() (const T& op1, const T& op2) const { return (op1 ^ op2); }
};

OCTAVE_END_NAMESPACE(std)
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename OP, typename T>
octave_value
bitopxx (const OP& op, const std::string& fname,
         const Array<T>& x, const Array<T>& y)
{
  int nelx = x.numel ();
  int nely = y.numel ();

  bool is_scalar_op = (nelx == 1 || nely == 1);

  dim_vector dvx = x.dims ();
  dim_vector dvy = y.dims ();

  bool is_array_op = (dvx == dvy);

  if (! is_array_op && ! is_scalar_op)
    error ("%s: size of X and Y must match, or one operand must be a scalar",
           fname.c_str ());

  Array<T> result;

  if (nelx != 1)
    result.resize (dvx);
  else
    result.resize (dvy);

  for (int i = 0; i < nelx; i++)
    if (is_scalar_op)
      for (int k = 0; k < nely; k++)
        result(i+k) = op (x(i), y(k));
    else
      result(i) = op (x(i), y(i));

  return result;
}

// Trampoline function, instantiates the proper template above, with
// reflective information hardwired.  We can't hardwire this information
// in Fbitxxx DEFUNs below, because at that moment, we still don't have
// information about which integer types we need to instantiate.
template <typename T>
octave_value
bitopx (const std::string& fname, const Array<T>& x, const Array<T>& y)
{
  if (fname == "bitand")
    return bitopxx (std::bit_and<T>(), fname, x, y);
  if (fname == "bitor")
    return bitopxx (std::bit_or<T>(), fname, x, y);

  //else (fname == "bitxor")
  return bitopxx (std::bit_xor<T>(), fname, x, y);
}

static inline int
bitop_arg_is_int (const octave_value& arg)
{
  return (arg.class_name () != octave_scalar::static_class_name ()
          && arg.class_name () != octave_float_scalar::static_class_name ()
          && arg.class_name () != octave_bool::static_class_name ());
}

static inline int
bitop_arg_is_bool (const octave_value& arg)
{
  return arg.class_name () == octave_bool::static_class_name ();
}

static inline int
bitop_arg_is_float (const octave_value& arg)
{
  return arg.class_name () == octave_float_scalar::static_class_name ();
}

octave_value
bitop (const std::string& fname, const octave_value_list& args)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  if (args(0).class_name () == octave_scalar::static_class_name ()
      || args(0).class_name () == octave_float_scalar::static_class_name ()
      || args(0).class_name () == octave_bool::static_class_name ()
      || args(1).class_name () == octave_scalar::static_class_name ()
      || args(1).class_name () == octave_float_scalar::static_class_name ()
      || args(1).class_name () == octave_bool::static_class_name ())
    {
      bool arg0_is_int = bitop_arg_is_int (args(0));
      bool arg1_is_int = bitop_arg_is_int (args(1));

      bool arg0_is_bool = bitop_arg_is_bool (args(0));
      bool arg1_is_bool = bitop_arg_is_bool (args(1));

      bool arg0_is_float = bitop_arg_is_float (args(0));
      bool arg1_is_float = bitop_arg_is_float (args(1));

      if (! (arg0_is_int || arg1_is_int))
        {
          if (arg0_is_bool && arg1_is_bool)
            {
              boolNDArray x (args(0).bool_array_value ());
              boolNDArray y (args(1).bool_array_value ());

              retval = bitopx (fname, x, y).bool_array_value ();
            }
          else if (arg0_is_float && arg1_is_float)
            {
              uint64NDArray x (args(0).float_array_value ());
              uint64NDArray y (args(1).float_array_value ());

              retval = bitopx (fname, x, y).float_array_value ();
            }
          else if (! (arg0_is_float || arg1_is_float))
            {
              uint64NDArray x (args(0).array_value ());
              uint64NDArray y (args(1).array_value ());

              retval = bitopx (fname, x, y).array_value ();
            }
          else
            {
              int p = (arg0_is_float ? 1 : 0);
              int q = (arg0_is_float ? 0 : 1);

              uint64NDArray x (args(p).array_value ());
              uint64NDArray y (args(q).float_array_value ());

              retval = bitopx (fname, x, y).float_array_value ();
            }
        }
      else
        {
          int p = (arg0_is_int ? 1 : 0);
          int q = (arg0_is_int ? 0 : 1);

          NDArray dx = args(p).array_value ();

          if (args(q).type_id () == octave_uint64_matrix::static_type_id ()
              || args(q).type_id () == octave_uint64_scalar::static_type_id ())
            {
              uint64NDArray x (dx);
              uint64NDArray y = args(q).uint64_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_uint32_matrix::static_type_id ()
                   || args(q).type_id () == octave_uint32_scalar::static_type_id ())
            {
              uint32NDArray x (dx);
              uint32NDArray y = args(q).uint32_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_uint16_matrix::static_type_id ()
                   || args(q).type_id () == octave_uint16_scalar::static_type_id ())
            {
              uint16NDArray x (dx);
              uint16NDArray y = args(q).uint16_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_uint8_matrix::static_type_id ()
                   || args(q).type_id () == octave_uint8_scalar::static_type_id ())
            {
              uint8NDArray x (dx);
              uint8NDArray y = args(q).uint8_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_int64_matrix::static_type_id ()
                   || args(q).type_id () == octave_int64_scalar::static_type_id ())
            {
              int64NDArray x (dx);
              int64NDArray y = args(q).int64_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_int32_matrix::static_type_id ()
                   || args(q).type_id () == octave_int32_scalar::static_type_id ())
            {
              int32NDArray x (dx);
              int32NDArray y = args(q).int32_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_int16_matrix::static_type_id ()
                   || args(q).type_id () == octave_int16_scalar::static_type_id ())
            {
              int16NDArray x (dx);
              int16NDArray y = args(q).int16_array_value ();

              retval = bitopx (fname, x, y);
            }
          else if (args(q).type_id () == octave_int8_matrix::static_type_id ()
                   || args(q).type_id () == octave_int8_scalar::static_type_id ())
            {
              int8NDArray x (dx);
              int8NDArray y = args(q).int8_array_value ();

              retval = bitopx (fname, x, y);
            }
          else
            error ("%s: invalid operand type", fname.c_str ());
        }
    }
  else if (args(0).class_name () == args(1).class_name ())
    {
      if (args(0).type_id () == octave_uint64_matrix::static_type_id ()
          || args(0).type_id () == octave_uint64_scalar::static_type_id ())
        {
          uint64NDArray x = args(0).uint64_array_value ();
          uint64NDArray y = args(1).uint64_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_uint32_matrix::static_type_id ()
               || args(0).type_id () == octave_uint32_scalar::static_type_id ())
        {
          uint32NDArray x = args(0).uint32_array_value ();
          uint32NDArray y = args(1).uint32_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_uint16_matrix::static_type_id ()
               || args(0).type_id () == octave_uint16_scalar::static_type_id ())
        {
          uint16NDArray x = args(0).uint16_array_value ();
          uint16NDArray y = args(1).uint16_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_uint8_matrix::static_type_id ()
               || args(0).type_id () == octave_uint8_scalar::static_type_id ())
        {
          uint8NDArray x = args(0).uint8_array_value ();
          uint8NDArray y = args(1).uint8_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_int64_matrix::static_type_id ()
               || args(0).type_id () == octave_int64_scalar::static_type_id ())
        {
          int64NDArray x = args(0).int64_array_value ();
          int64NDArray y = args(1).int64_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_int32_matrix::static_type_id ()
               || args(0).type_id () == octave_int32_scalar::static_type_id ())
        {
          int32NDArray x = args(0).int32_array_value ();
          int32NDArray y = args(1).int32_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_int16_matrix::static_type_id ()
               || args(0).type_id () == octave_int16_scalar::static_type_id ())
        {
          int16NDArray x = args(0).int16_array_value ();
          int16NDArray y = args(1).int16_array_value ();

          retval = bitopx (fname, x, y);
        }
      else if (args(0).type_id () == octave_int8_matrix::static_type_id ()
               || args(0).type_id () == octave_int8_scalar::static_type_id ())
        {
          int8NDArray x = args(0).int8_array_value ();
          int8NDArray y = args(1).int8_array_value ();

          retval = bitopx (fname, x, y);
        }
      else
        error ("%s: invalid operand type", fname.c_str ());
    }
  else
    error ("%s: must have matching operand types", fname.c_str ());

  return retval;
}

DEFUN (bitand, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{z} =} bitand (@var{x}, @var{y})
Return the bitwise AND of non-negative integers.

@var{x}, @var{y} must be in the range [0,intmax]
@seealso{bitor, bitxor, bitset, bitget, bitcmp, bitshift, intmax, flintmax}
@end deftypefn */)
{
  return bitop ("bitand", args);
}

/*
%!# Function bitand is tested as part of bitxor BIST tests
*/

DEFUN (bitor, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{z} =} bitor (@var{x}, @var{y})
Return the bitwise OR of non-negative integers @var{x} and @var{y}.

@seealso{bitor, bitxor, bitset, bitget, bitcmp, bitshift, intmax, flintmax}
@end deftypefn */)
{
  return bitop ("bitor", args);
}

/*
%!# Function bitor is tested as part of bitxor BIST tests
*/

DEFUN (bitxor, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{z} =} bitxor (@var{x}, @var{y})
Return the bitwise XOR of non-negative integers @var{x} and @var{y}.

@seealso{bitand, bitor, bitset, bitget, bitcmp, bitshift, intmax, flintmax}
@end deftypefn */)
{
  return bitop ("bitxor", args);
}

/*
%!assert (bitand (true, false), false)
%!assert (bitor  (true, false), true)
%!assert (bitxor (true, false), true)

%!assert (bitand (true, true), true)
%!assert (bitor  (true, true), true)
%!assert (bitxor (true, true), false)

%!assert (bitand (true, 5), 1)

%!assert (bitand (true, false), false)
%!assert (bitand (true, true), true)
%!assert (bitand (true, false), false)
%!assert (bitand (true, false), false)

## Test idx_arg.length () == 0
%!error <size of X and Y must match> bitand ([0 0 0], [1 0])
%!error <size of X and Y must match> bitand ([0; 0; 0], [0 0 0])
*/

template <typename T>
static int64_t
max_mantissa_value ()
{
  return (static_cast<int64_t> (1) << std::numeric_limits<T>::digits) - 1;
}

static int64_t
bitshift (double a, int n, int64_t mask)
{
  // In the name of bug-for-bug compatibility.
  if (a < 0)
    return -bitshift (-a, n, mask);

  if (n > 0)
    return (static_cast<int64_t> (a) << n) & mask;
  else if (n < 0)
    return (static_cast<int64_t> (a) >> -n) & mask;
  else
    return static_cast<int64_t> (a) & mask;
}

static int64_t
bitshift (float a, int n, int64_t mask)
{
  // In the name of bug-for-bug compatibility.
  if (a < 0)
    return -bitshift (-a, n, mask);

  if (n > 0)
    return (static_cast<int64_t> (a) << n) & mask;
  else if (n < 0)
    return (static_cast<int64_t> (a) >> -n) & mask;
  else
    return static_cast<int64_t> (a) & mask;
}

// Note that the bitshift operators are undefined if shifted by more
// bits than in the type, so we need to test for the size of the
// shift.

#define DO_BITSHIFT(T)                                                  \
  double d1, d2;                                                        \
                                                                        \
  if (! n.all_integers (d1, d2))                                        \
    error ("bitshift: K must be a scalar or array of integers");        \
                                                                        \
  int m_nel = m.numel ();                                               \
  int n_nel = n.numel ();                                               \
                                                                        \
  bool is_scalar_op = (m_nel == 1 || n_nel == 1);                       \
                                                                        \
  dim_vector m_dv = m.dims ();                                          \
  dim_vector n_dv = n.dims ();                                          \
                                                                        \
  bool is_array_op = (m_dv == n_dv);                                    \
                                                                        \
  if (! is_array_op && ! is_scalar_op)                                  \
    error ("bitshift: size of A and N must match, or one operand must be a scalar"); \
                                                                        \
  T ## NDArray result;                                                  \
                                                                        \
  if (m_nel != 1)                                                       \
    result.resize (m_dv);                                               \
  else                                                                  \
    result.resize (n_dv);                                               \
                                                                        \
  for (int i = 0; i < m_nel; i++)                                       \
    if (is_scalar_op)                                                   \
      for (int k = 0; k < n_nel; k++)                                   \
        if (static_cast<int> (n(k)) >= bits_in_type)                    \
          result(i+k) = 0;                                              \
        else                                                            \
          result(i+k) = bitshift (m(i), static_cast<int> (n(k)), mask); \
    else                                                                \
      if (static_cast<int> (n(i)) >= bits_in_type)                      \
        result(i) = 0;                                                  \
      else                                                              \
        result(i) = bitshift (m(i), static_cast<int> (n(i)), mask);     \
                                                                        \
  retval = result;

#define DO_UBITSHIFT(T, N)                              \
  do                                                    \
    {                                                   \
      int bits_in_type = octave_ ## T :: nbits ();      \
      T ## NDArray m = m_arg.T ## _array_value ();      \
      octave_ ## T mask = octave_ ## T::max ();         \
      if ((N) < bits_in_type)                           \
        mask = bitshift (mask, (N) - bits_in_type);     \
      else if ((N) < 1)                                 \
        mask = 0;                                       \
      DO_BITSHIFT (T);                                  \
    }                                                   \
  while (0)

#define DO_SBITSHIFT(T, N)                              \
  do                                                    \
    {                                                   \
      int bits_in_type = octave_ ## T :: nbits ();      \
      T ## NDArray m = m_arg.T ## _array_value ();      \
      octave_ ## T mask = octave_ ## T::max ();         \
      if ((N) < bits_in_type)                           \
        mask = bitshift (mask, (N) - bits_in_type);     \
      else if ((N) < 1)                                 \
        mask = 0;                                       \
      /* FIXME: 2's complement only? */                 \
      mask = mask | octave_ ## T :: min ();             \
      DO_BITSHIFT (T);                                  \
    }                                                   \
  while (0)

DEFUN (bitshift, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{B} =} bitshift (@var{A}, @var{k})
@deftypefnx {} {@var{B} =} bitshift (@var{A}, @var{k}, @var{n})
Return a @var{k} bit shift of @var{n}-digit unsigned integers in @var{A}.

A positive @var{k} leads to a left shift; A negative value to a right shift.

If @var{n} is omitted it defaults to 64.  @var{n} must be in the range [1,64].

@example
@group
bitshift (eye (3), 1)
@result{}
@group
2 0 0
0 2 0
0 0 2
@end group

bitshift (10, [-2, -1, 0, 1, 2])
@result{} 2   5  10  20  40
@c FIXME: restore this example when third arg is allowed to be an array.
@c
@c
@c bitshift ([1, 10], 2, [3,4])
@c @result{} 4  8
@end group
@end example
@seealso{bitand, bitor, bitxor, bitset, bitget, bitcmp, intmax, flintmax}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  NDArray n = args(1).xarray_value ("bitshift: K must be a scalar or array of integers");

  int nbits = 64;

  if (nargin == 3)
    {
      // FIXME: for compatibility, we should accept an array or a scalar
      //        as the third argument.
      if (args(2).numel () > 1)
        error ("bitshift: N must be a scalar integer");

      nbits = args(2).xint_value ("bitshift: N must be an integer");

      if (nbits < 0)
        error ("bitshift: N must be positive");
    }

  octave_value retval;

  octave_value m_arg = args(0);
  std::string cname = m_arg.class_name ();

  if (cname == "double")
    {
      static const int bits_in_mantissa
        = std::numeric_limits<double>::digits;

      nbits = (nbits < bits_in_mantissa ? nbits : bits_in_mantissa);
      int64_t mask = max_mantissa_value<double> ();
      if (nbits < bits_in_mantissa)
        mask = mask >> (bits_in_mantissa - nbits);
      int bits_in_type = sizeof (double)
                         * std::numeric_limits<unsigned char>::digits;
      NDArray m = m_arg.array_value ();
      DO_BITSHIFT ();
    }
  else if (cname == "uint8")
    DO_UBITSHIFT (uint8, nbits < 8 ? nbits : 8);
  else if (cname == "uint16")
    DO_UBITSHIFT (uint16, nbits < 16 ? nbits : 16);
  else if (cname == "uint32")
    DO_UBITSHIFT (uint32, nbits < 32 ? nbits : 32);
  else if (cname == "uint64")
    DO_UBITSHIFT (uint64, nbits < 64 ? nbits : 64);
  else if (cname == "int8")
    DO_SBITSHIFT (int8, nbits < 8 ? nbits : 8);
  else if (cname == "int16")
    DO_SBITSHIFT (int16, nbits < 16 ? nbits : 16);
  else if (cname == "int32")
    DO_SBITSHIFT (int32, nbits < 32 ? nbits : 32);
  else if (cname == "int64")
    DO_SBITSHIFT (int64, nbits < 64 ? nbits : 64);
  else if (cname == "single")
    {
      static const int bits_in_mantissa
        = std::numeric_limits<float>::digits;
      nbits = (nbits < bits_in_mantissa ? nbits : bits_in_mantissa);
      int64_t mask = max_mantissa_value<float> ();
      if (nbits < bits_in_mantissa)
        mask = mask >> (bits_in_mantissa - nbits);
      int bits_in_type = sizeof (float)
                         * std::numeric_limits<unsigned char>::digits;
      FloatNDArray m = m_arg.float_array_value ();
      DO_BITSHIFT (Float);
    }
  else
    error ("bitshift: not defined for %s objects", cname.c_str ());

  return retval;
}

/*
%!assert (bitshift (uint8  (16), 1),  uint8 ( 32))
%!assert (bitshift (uint16 (16), 2), uint16 ( 64))
%!assert (bitshift (uint32 (16), 3), uint32 (128))
%!assert (bitshift (uint64 (16), 4), uint64 (256))
%!assert (bitshift (uint8 (255), 1), uint8 (254))

%!error <K must be a scalar or array of integers> bitshift (16, 1.5)
%!error bitshift (16, {1})
%!error <N must be a scalar integer> bitshift (10, [-2 -1 0 1 2], [1 1 1 1 1])
%!error <N must be positive> bitshift (10, [-2 -1 0 1 2], -1)
*/

DEFUN (flintmax, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Imax} =} flintmax ()
@deftypefnx {} {@var{Imax} =} flintmax ("double")
@deftypefnx {} {@var{Imax} =} flintmax ("single")
@deftypefnx {} {@var{Imax} =} flintmax (@var{var})
Return the largest integer that can be represented consecutively in a
floating point value.

The input is either a string specifying a floating point type, or it is an
existing floating point variable @var{var}.

The default type is @qcode{"double"}, but @qcode{"single"} is a valid option.
On IEEE 754 compatible systems, @code{flintmax} is @w{@math{2^{53}}} for
@qcode{"double"} and @w{@math{2^{24}}} for @qcode{"single"}.

Example Code - query an existing variable

@example
@group
x = single (1);
flintmax (x)
  @result{} 16777216
@end group
@end example

@seealso{intmax, realmax, realmin}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  std::string cname = "double";
  if (nargin == 1)
    {
      if (args(0).is_string ())
        cname = args(0).string_value ();
      else if (args(0).isfloat ())
        cname = args(0).class_name ();
      else
        error ("intmin: argument must be a string or floating point variable");
    }

  if (cname == "double")
    return ovl (static_cast<double> (max_mantissa_value<double> () + 1));
  else if (cname == "single")
    return ovl (static_cast<float> (max_mantissa_value<float> () + 1));
  else
    error ("flintmax: not defined for class '%s'", cname.c_str ());
}

/*
%!assert (flintmax (), 2^53)
%!assert (flintmax ("double"), 2^53)
%!assert (flintmax ("single"), single (2^24))

%!test
%! x = single (1);
%! assert (flintmax (x), single (16777216));

%!error flintmax ("double", 0)
%!error <must be a string or floating point variable> flintmax (int8 (1))
%!error <not defined for class 'int8'> flintmax ("int8")
%!error <not defined for class 'char'> flintmax ("char")
*/

DEFUN (intmax, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Imax} =} intmax ()
@deftypefnx {} {@var{Imax} =} intmax ("@var{type}")
@deftypefnx {} {@var{Imax} =} intmax (@var{var})
Return the largest integer that can be represented by a specific integer type.

The input is either a string @qcode{"@var{type}"} specifying an integer type,
or it is an existing integer variable @var{var}.

Possible values for @var{type} are

@table @asis
@item @qcode{"int8"}
signed 8-bit integer.

@item @qcode{"int16"}
signed 16-bit integer.

@item @qcode{"int32"}
signed 32-bit integer.

@item @qcode{"int64"}
signed 64-bit integer.

@item @qcode{"uint8"}
unsigned 8-bit integer.

@item @qcode{"uint16"}
unsigned 16-bit integer.

@item @qcode{"uint32"}
unsigned 32-bit integer.

@item @qcode{"uint64"}
unsigned 64-bit integer.
@end table

The default for @var{type} is @qcode{"int32"}.

Example Code - query an existing variable

@example
@group
x = int8 (1);
intmax (x)
  @result{} 127
@end group
@end example

@seealso{intmin, flintmax}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  std::string cname = "int32";
  if (nargin == 1)
    {
      if (args(0).is_string ())
        cname = args(0).string_value ();
      else if (args(0).isinteger ())
        cname = args(0).class_name ();
      else
        error ("intmax: argument must be a string or integer variable");
    }

  octave_value retval;

  if (cname == "uint8")
    retval = octave_uint8 (std::numeric_limits<uint8_t>::max ());
  else if (cname == "uint16")
    retval = octave_uint16 (std::numeric_limits<uint16_t>::max ());
  else if (cname == "uint32")
    retval = octave_uint32 (std::numeric_limits<uint32_t>::max ());
  else if (cname == "uint64")
    retval = octave_uint64 (std::numeric_limits<uint64_t>::max ());
  else if (cname == "int8")
    retval = octave_int8 (std::numeric_limits<int8_t>::max ());
  else if (cname == "int16")
    retval = octave_int16 (std::numeric_limits<int16_t>::max ());
  else if (cname == "int32")
    retval = octave_int32 (std::numeric_limits<int32_t>::max ());
  else if (cname == "int64")
    retval = octave_int64 (std::numeric_limits<int64_t>::max ());
  else
    error ("intmax: not defined for '%s' objects", cname.c_str ());

  return retval;
}

/*
%!assert (intmax (),          int32 (2^31 - 1))
%!assert (intmax ("int8"),     int8 (2^7 - 1))
%!assert (intmax ("uint8"),   uint8 (2^8 - 1))
%!assert (intmax ("int16"),   int16 (2^15 - 1))
%!assert (intmax ("uint16"), uint16 (2^16 - 1))
%!assert (intmax ("int32"),   int32 (2^31 - 1))
%!assert (intmax ("uint32"), uint32 (2^32 - 1))
%!assert (intmax ("int64"),   int64 (2^63 - 1))
%!assert (intmax ("uint64"), uint64 (2^64 - 1))

%!test
%! x = int8 (1);
%! assert (intmax (x), int8 (127));

%!error intmax ("int32", 0)
%!error <must be a string or integer variable> intmax (1.0)
%!error <not defined for 'double' objects> intmax ("double")
%!error <not defined for 'char' objects> intmax ("char")
*/

DEFUN (intmin, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Imin} =} intmin ()
@deftypefnx {} {@var{Imin} =} intmin ("@var{type}")
@deftypefnx {} {@var{Imin} =} intmin (@var{var})
Return the smallest integer that can be represented by a specific integer type.

The input is either a string @qcode{"@var{type}"} specifying an integer type,
or it is an existing integer variable @var{var}.

Possible values for @var{type} are

@table @asis
@item @qcode{"int8"}
signed 8-bit integer.

@item @qcode{"int16"}
signed 16-bit integer.

@item @qcode{"int32"}
signed 32-bit integer.

@item @qcode{"int64"}
signed 64-bit integer.

@item @qcode{"uint8"}
unsigned 8-bit integer.

@item @qcode{"uint16"}
unsigned 16-bit integer.

@item @qcode{"uint32"}
unsigned 32-bit integer.

@item @qcode{"uint64"}
unsigned 64-bit integer.
@end table

The default for @var{type} is @qcode{"int32"}.

Example Code - query an existing variable

@example
@group
x = int8 (1);
intmin (x)
  @result{} -128
@end group
@end example

@seealso{intmax, flintmax}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  std::string cname = "int32";
  if (nargin == 1)
    {
      if (args(0).is_string ())
        cname = args(0).string_value ();
      else if (args(0).isinteger ())
        cname = args(0).class_name ();
      else
        error ("intmin: argument must be a string or integer variable");
    }

  octave_value retval;

  if (cname == "uint8")
    retval = octave_uint8 (std::numeric_limits<uint8_t>::min ());
  else if (cname == "uint16")
    retval = octave_uint16 (std::numeric_limits<uint16_t>::min ());
  else if (cname == "uint32")
    retval = octave_uint32 (std::numeric_limits<uint32_t>::min ());
  else if (cname == "uint64")
    retval = octave_uint64 (std::numeric_limits<uint64_t>::min ());
  else if (cname == "int8")
    retval = octave_int8 (std::numeric_limits<int8_t>::min ());
  else if (cname == "int16")
    retval = octave_int16 (std::numeric_limits<int16_t>::min ());
  else if (cname == "int32")
    retval = octave_int32 (std::numeric_limits<int32_t>::min ());
  else if (cname == "int64")
    retval = octave_int64 (std::numeric_limits<int64_t>::min ());
  else
    error ("intmin: not defined for '%s' objects", cname.c_str ());

  return retval;
}

/*
%!assert (intmin (),          int32 (-2^31))
%!assert (intmin ("int8"),     int8 (-2^7))
%!assert (intmin ("uint8"),   uint8 (-2^8))
%!assert (intmin ("int16"),   int16 (-2^15))
%!assert (intmin ("uint16"), uint16 (-2^16))
%!assert (intmin ("int32"),   int32 (-2^31))
%!assert (intmin ("uint32"), uint32 (-2^32))
%!assert (intmin ("int64"),   int64 (-2^63))
%!assert (intmin ("uint64"), uint64 (-2^64))

%!test
%! x = int8 (1);
%! assert (intmin (x), int8 (-128));

%!error intmin ("int32", 0)
%!error <must be a string or integer variable> intmin (1.0)
%!error <not defined for 'double' objects> intmin ("double")
%!error <not defined for 'char' objects> intmin ("char")
*/

DEFUN (sizemax, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{max_numel} =} sizemax ()
Return the largest value allowed for the size of an array.

If Octave is compiled with 64-bit indexing, the result is of class int64,
otherwise it is of class int32.  The maximum array size is slightly smaller
than the maximum value allowable for the relevant class as reported by
@code{intmax}.
@seealso{intmax}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return octave_value (octave_int<octave_idx_type> (dim_vector::dim_max ()));
}

/*
%!assert (sizemax () >= (intmax ("int32") - 1))

%!error sizemax (0)
*/

OCTAVE_END_NAMESPACE(octave)
