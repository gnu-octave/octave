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

#include <algorithm>
#include <limits>

#include "mx-base.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "unwind-prot.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static dim_vector
get_vec_dims (const dim_vector& old_dims, octave_idx_type n)
{
  if (old_dims.ndims () == 2 && old_dims(0) == 1)
    return dim_vector (1, n);
  else if (old_dims.ndims () == 2 && old_dims(0) == 0 && old_dims(1) == 0)
    return dim_vector ();
  else
    return dim_vector (n, 1);
}

template <typename ArrayType>
static void
get_data_and_bytesize (const ArrayType& array,
                       const void *&data,
                       octave_idx_type& byte_size,
                       dim_vector& old_dims,
                       unwind_protect& frame)
{
  // The array given may be a temporary, constructed from a scalar or sparse
  // array.  This will ensure the data will be deallocated after we exit.
  frame.add_delete (new ArrayType (array));

  data = reinterpret_cast<const void *> (array.data ());
  byte_size = array.byte_size ();

  old_dims = array.dims ();
}

template <typename ArrayType>
static ArrayType
reinterpret_copy (const void *data, octave_idx_type byte_size,
                  const dim_vector& old_dims)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n = byte_size / sizeof (T);

  if (n * static_cast<int> (sizeof (T)) != byte_size)
    error ("typecast: incorrect number of input values to make output value");

  ArrayType retval (get_vec_dims (old_dims, n));
  T *dest = retval.fortran_vec ();
  std::memcpy (dest, data, n * sizeof (T));

  return retval;
}

template <typename ArrayType>
static ArrayType
reinterpret_int_copy (const void *data, octave_idx_type byte_size,
                      const dim_vector& old_dims)
{
  typedef typename ArrayType::element_type T;
  typedef typename T::val_type VT;
  octave_idx_type n = byte_size / sizeof (T);

  if (n * static_cast<int> (sizeof (T)) != byte_size)
    error ("typecast: incorrect number of input values to make output value");

  ArrayType retval (get_vec_dims (old_dims, n));
  VT *dest = reinterpret_cast<VT *> (retval.fortran_vec ());
  std::memcpy (dest, data, n * sizeof (VT));

  return retval;
}

DEFUN (typecast, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} typecast (@var{x}, "@var{class}")
Return a new array @var{y} resulting from interpreting the data of @var{x}
in memory as data of the numeric class @var{class}.

Both the class of @var{x} and @var{class} must be one of the built-in
numeric classes:

@example
@group
"logical"
"char"
"int8"
"int16"
"int32"
"int64"
"uint8"
"uint16"
"uint32"
"uint64"
"double"
"single"
"double complex"
"single complex"
@end group
@end example

@noindent
the last two are only used with @var{class}; they indicate that a
complex-valued result is requested.  Complex arrays are stored in memory as
consecutive pairs of real numbers.  The sizes of integer types are given by
their bit counts.  Both logical and char are typically one byte wide;
however, this is not guaranteed by C++.  If your system is IEEE conformant,
single and double will be 4 bytes and 8 bytes wide, respectively.
@qcode{"logical"} is not allowed for @var{class}.

If the input is a row vector, the return value is a row vector, otherwise it
is a column vector.

If the bit length of @var{x} is not divisible by that of @var{class}, an
error occurs.

An example of the use of typecast on a little-endian machine is

@example
@group
@var{x} = uint16 ([1, 65535]);
typecast (@var{x}, "uint8")
@result{} [   1,   0, 255, 255]
@end group
@end example
@seealso{cast, bitpack, bitunpack, swapbytes}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  unwind_protect frame;

  const void *data = nullptr;
  octave_idx_type byte_size = 0;
  dim_vector old_dims;

  octave_value array = args(0);

  if (array.islogical ())
    get_data_and_bytesize (array.bool_array_value (), data, byte_size,
                           old_dims, frame);
  else if (array.is_string ())
    get_data_and_bytesize (array.char_array_value (), data, byte_size,
                           old_dims, frame);
  else if (array.isinteger ())
    {
      if (array.is_int8_type ())
        get_data_and_bytesize (array.int8_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_int16_type ())
        get_data_and_bytesize (array.int16_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_int32_type ())
        get_data_and_bytesize (array.int32_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_int64_type ())
        get_data_and_bytesize (array.int64_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_uint8_type ())
        get_data_and_bytesize (array.uint8_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_uint16_type ())
        get_data_and_bytesize (array.uint16_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_uint32_type ())
        get_data_and_bytesize (array.uint32_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_uint64_type ())
        get_data_and_bytesize (array.uint64_array_value (), data, byte_size,
                               old_dims, frame);
    }
  else if (array.iscomplex ())
    {
      if (array.is_single_type ())
        get_data_and_bytesize (array.float_complex_array_value (), data,
                               byte_size, old_dims, frame);
      else
        get_data_and_bytesize (array.complex_array_value (), data,
                               byte_size, old_dims, frame);
    }
  else if (array.isreal ())
    {
      if (array.is_single_type ())
        get_data_and_bytesize (array.float_array_value (), data, byte_size,
                               old_dims, frame);
      else
        get_data_and_bytesize (array.array_value (), data, byte_size,
                               old_dims, frame);
    }
  else
    error ("typecast: invalid input CLASS: %s",
           array.class_name ().c_str ());

  std::string numclass = args(1).string_value ();
  std::transform (numclass.begin (), numclass.end (), numclass.begin (),
                  tolower);

  if (numclass.size () == 0)
    ;
  else if (numclass == "char")
    retval = octave_value (reinterpret_copy<charNDArray>
                           (data, byte_size, old_dims),
                           array.is_dq_string () ? '"' : '\'');
  else if (numclass[0] == 'i')
    {
      if (numclass == "int8")
        retval = reinterpret_int_copy<int8NDArray> (data, byte_size, old_dims);
      else if (numclass == "int16")
        retval = reinterpret_int_copy<int16NDArray> (data, byte_size, old_dims);
      else if (numclass == "int32")
        retval = reinterpret_int_copy<int32NDArray> (data, byte_size, old_dims);
      else if (numclass == "int64")
        retval = reinterpret_int_copy<int64NDArray> (data, byte_size, old_dims);
    }
  else if (numclass[0] == 'u')
    {
      if (numclass == "uint8")
        retval = reinterpret_int_copy<uint8NDArray> (data, byte_size, old_dims);
      else if (numclass == "uint16")
        retval = reinterpret_int_copy<uint16NDArray> (data, byte_size,
                 old_dims);
      else if (numclass == "uint32")
        retval = reinterpret_int_copy<uint32NDArray> (data, byte_size,
                 old_dims);
      else if (numclass == "uint64")
        retval = reinterpret_int_copy<uint64NDArray> (data, byte_size,
                 old_dims);
    }
  else if (numclass == "single")
    retval = reinterpret_copy<FloatNDArray> (data, byte_size, old_dims);
  else if (numclass == "double")
    retval = reinterpret_copy<NDArray> (data, byte_size, old_dims);
  else if (numclass == "single complex")
    retval = reinterpret_copy<FloatComplexNDArray> (data, byte_size,
             old_dims);
  else if (numclass == "double complex")
    retval = reinterpret_copy<ComplexNDArray> (data, byte_size, old_dims);

  if (retval.is_undefined ())
    error ("typecast: cannot convert to %s class", numclass.c_str ());

  return retval;
}

/*
%!assert (typecast (int64 (0), "char"),   char (zeros (1, 8)))
%!assert (typecast (int64 (0), "int8"),   zeros (1, 8, "int8"))
%!assert (typecast (int64 (0), "uint8"),  zeros (1, 8, "uint8"))
%!assert (typecast (int64 (0), "int16"),  zeros (1, 4, "int16"))
%!assert (typecast (int64 (0), "uint16"), zeros (1, 4, "uint16"))
%!assert (typecast (int64 (0), "int32"),  zeros (1, 2, "int32"))
%!assert (typecast (int64 (0), "uint32"), zeros (1, 2, "uint32"))
%!assert (typecast (int64 (0), "int64"),  zeros (1, 1, "int64"))
%!assert (typecast (int64 (0), "uint64"), zeros (1, 1, "uint64"))
%!assert (typecast (int64 (0), "single"), zeros (1, 2, "single"))
%!assert (typecast (int64 (0), "double"), 0)
%!assert (typecast (int64 (0), "single complex"), single (0))
%!assert (typecast (int64 ([0 0]), "double complex"), 0)

%!assert (typecast ([],   "double"), [])
%!assert (typecast (0,    "double"), 0)
%!assert (typecast (inf,  "double"), inf)
%!assert (typecast (-inf, "double"), -inf)
%!assert (typecast (nan,  "double"), nan)

%!error typecast ()
%!error typecast (1)
%!error typecast (1, 2, 3)
%!error typecast (1, "invalid")
%!error typecast (int8 (0), "double")
*/

template <typename ArrayType>
ArrayType
do_bitpack (const boolNDArray& bitp)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n
    = bitp.numel () / (sizeof (T) * std::numeric_limits<unsigned char>::digits);

  if (n * static_cast<int> (sizeof (T)) *
      std::numeric_limits<unsigned char>::digits != bitp.numel ())
    error ("bitpack: incorrect number of bits to make up output value");

  ArrayType retval (get_vec_dims (bitp.dims (), n));

  const bool *bits = bitp.data ();
  char *packed = reinterpret_cast<char *> (retval.fortran_vec ());

  octave_idx_type m = n * sizeof (T);

  for (octave_idx_type i = 0; i < m; i++)
    {
      char c = bits[0];
      for (int j = 1; j < std::numeric_limits<unsigned char>::digits; j++)
        c |= bits[j] << j;

      packed[i] = c;
      bits += std::numeric_limits<unsigned char>::digits;
    }

  return retval;
}

DEFUN (bitpack, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} bitpack (@var{x}, @var{class})
Return a new array @var{y} resulting from interpreting the logical array
@var{x} as raw bit patterns for data of the numeric class @var{class}.

@var{class} must be one of the built-in numeric classes:

@example
@group
"double"
"single"
"double complex"
"single complex"
"char"
"int8"
"int16"
"int32"
"int64"
"uint8"
"uint16"
"uint32"
"uint64"
@end group
@end example

The number of elements of @var{x} should be divisible by the bit length of
@var{class}.  If it is not, excess bits are discarded.  Bits come in
increasing order of significance, i.e., @code{x(1)} is bit 0, @code{x(2)} is
bit 1, etc.

The result is a row vector if @var{x} is a row vector, otherwise it is a
column vector.
@seealso{bitunpack, typecast}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  if (! args(0).islogical ())
    error ("bitpack: X must be a logical array");

  octave_value retval;

  boolNDArray bitp = args(0).bool_array_value ();

  std::string numclass = args(1).string_value ();

  if (numclass.size () == 0)
    ;
  else if (numclass == "char")
    retval = octave_value (do_bitpack<charNDArray> (bitp), '\'');
  else if (numclass[0] == 'i')
    {
      if (numclass == "int8")
        retval = do_bitpack<int8NDArray> (bitp);
      else if (numclass == "int16")
        retval = do_bitpack<int16NDArray> (bitp);
      else if (numclass == "int32")
        retval = do_bitpack<int32NDArray> (bitp);
      else if (numclass == "int64")
        retval = do_bitpack<int64NDArray> (bitp);
    }
  else if (numclass[0] == 'u')
    {
      if (numclass == "uint8")
        retval = do_bitpack<uint8NDArray> (bitp);
      else if (numclass == "uint16")
        retval = do_bitpack<uint16NDArray> (bitp);
      else if (numclass == "uint32")
        retval = do_bitpack<uint32NDArray> (bitp);
      else if (numclass == "uint64")
        retval = do_bitpack<uint64NDArray> (bitp);
    }
  else if (numclass == "single")
    retval = do_bitpack<FloatNDArray> (bitp);
  else if (numclass == "double")
    retval = do_bitpack<NDArray> (bitp);
  else if (numclass == "single complex")
    retval = do_bitpack<FloatComplexNDArray> (bitp);
  else if (numclass == "double complex")
    retval = do_bitpack<ComplexNDArray> (bitp);

  if (retval.is_undefined ())
    error ("bitpack: cannot pack to %s class", numclass.c_str ());

  return retval;
}

/*
%!assert (bitpack (zeros (1, 8,   "logical"), "char"),   "\0")
%!assert (bitpack (zeros (1, 8,   "logical"), "int8"),   int8 (0))
%!assert (bitpack (zeros (1, 8,   "logical"), "uint8"),  uint8 (0))
%!assert (bitpack (zeros (1, 16,  "logical"), "int16"),  int16 (0))
%!assert (bitpack (zeros (1, 16,  "logical"), "uint16"), uint16 (0))
%!assert (bitpack (zeros (1, 32,  "logical"), "int32"),  int32 (0))
%!assert (bitpack (zeros (1, 32,  "logical"), "uint32"), uint32 (0))
%!assert (bitpack (zeros (1, 64,  "logical"), "int64"),  int64 (0))
%!assert (bitpack (zeros (1, 64,  "logical"), "uint64"), uint64 (0))
%!assert (bitpack (zeros (1, 32,  "logical"), "single"), single (0))
%!assert (bitpack (zeros (1, 64,  "logical"), "double"), double (0))
%!assert (bitpack (zeros (1, 64,  "logical"), "single complex"), single (0))
%!assert (bitpack (zeros (1, 128, "logical"), "double complex"), double (0))

%!test <54931>
%! x = false (1, 32);
%! x(1) = true;
%! assert (bitpack (x, "uint32"), uint32 (1));
%! x([1, 9]) = true;
%! assert (bitpack (x, "uint32"), uint32 (257));

%!error bitpack ()
%!error bitpack (1)
%!error bitpack (1, 2, 3)
%!error bitpack (1, "invalid")
%!error bitpack (1, "double")
%!error bitpack (false, "invalid")
%!error bitpack (false, "double")
*/

template <typename ArrayType>
boolNDArray
do_bitunpack (const ArrayType& array)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n = array.numel () * sizeof (T)
                      * std::numeric_limits<unsigned char>::digits;

  boolNDArray retval (get_vec_dims (array.dims (), n));

  const char *packed = reinterpret_cast<const char *> (array.data ());
  bool *bits = retval.fortran_vec ();

  octave_idx_type m = n / std::numeric_limits<unsigned char>::digits;

  for (octave_idx_type i = 0; i < m; i++)
    {
      char c = packed[i];
      bits[0] = c & 1;
      for (int j = 1; j < std::numeric_limits<unsigned char>::digits; j++)
        bits[j] = (c >>= 1) & 1;
      bits += std::numeric_limits<unsigned char>::digits;
    }

  return retval;
}

DEFUN (bitunpack, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} bitunpack (@var{x})
Return a logical array @var{y} corresponding to the raw bit patterns of
@var{x}.

@var{x} must belong to one of the built-in numeric classes:

@example
@group
"double"
"single"
"char"
"int8"
"int16"
"int32"
"int64"
"uint8"
"uint16"
"uint32"
"uint64"
@end group
@end example

The result is a row vector if @var{x} is a row vector; otherwise, it is a
column vector.
@seealso{bitpack, typecast}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  if (! (args(0).isnumeric () || args(0).is_string ()))
    error ("bitunpack: argument must be a number or a string");

  octave_value retval;

  octave_value array = args(0);

  if (array.is_string ())
    retval = do_bitunpack (array.char_array_value ());
  else if (array.isinteger ())
    {
      if (array.is_int8_type ())
        retval = do_bitunpack (array.int8_array_value ());
      else if (array.is_int16_type ())
        retval = do_bitunpack (array.int16_array_value ());
      else if (array.is_int32_type ())
        retval = do_bitunpack (array.int32_array_value ());
      else if (array.is_int64_type ())
        retval = do_bitunpack (array.int64_array_value ());
      else if (array.is_uint8_type ())
        retval = do_bitunpack (array.uint8_array_value ());
      else if (array.is_uint16_type ())
        retval = do_bitunpack (array.uint16_array_value ());
      else if (array.is_uint32_type ())
        retval = do_bitunpack (array.uint32_array_value ());
      else if (array.is_uint64_type ())
        retval = do_bitunpack (array.uint64_array_value ());
    }
  else if (array.iscomplex ())
    {
      if (array.is_single_type ())
        retval = do_bitunpack (array.float_complex_array_value ());
      else
        retval = do_bitunpack (array.complex_array_value ());
    }
  else if (array.isreal ())
    {
      if (array.is_single_type ())
        retval = do_bitunpack (array.float_array_value ());
      else
        retval = do_bitunpack (array.array_value ());
    }
  else
    error ("bitunpack: invalid input class: %s",
           array.class_name ().c_str ());

  return retval;
}

/*
%!assert (bitunpack ("\0"),       zeros (1, 8,  "logical"))
%!assert (bitunpack (int8 (0)),   zeros (1, 8,  "logical"))
%!assert (bitunpack (uint8 (0)),  zeros (1, 8,  "logical"))
%!assert (bitunpack (int16 (0)),  zeros (1, 16, "logical"))
%!assert (bitunpack (uint16 (0)), zeros (1, 16, "logical"))
%!assert (bitunpack (int32 (0)),  zeros (1, 32, "logical"))
%!assert (bitunpack (uint32 (0)), zeros (1, 32, "logical"))
%!assert (bitunpack (int64 (0)),  zeros (1, 64, "logical"))
%!assert (bitunpack (uint64 (0)), zeros (1, 64, "logical"))
%!assert (bitunpack (single (0)), zeros (1, 32, "logical"))
%!assert (bitunpack (double (0)), zeros (1, 64, "logical"))
%!assert (bitunpack (complex (single (0))), zeros (1, 64, "logical"))
%!assert (bitunpack (complex (double (0))), zeros (1, 128, "logical"))

%!error bitunpack ()
%!error bitunpack (1, 2)
%!error bitunpack ({})
*/

OCTAVE_END_NAMESPACE(octave)
