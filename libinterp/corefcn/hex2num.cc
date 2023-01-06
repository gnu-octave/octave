////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "mach-info.h"
#include "ov.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static inline bool
is_little_endian (bool is_float)
{
  return ((is_float && (mach_info::native_float_format ()
                        == mach_info::flt_fmt_ieee_little_endian))
          || mach_info::words_little_endian ());
}

static uint8_t
hex2nibble (unsigned char ch)
{
  unsigned char val = 0;

  if (! isxdigit (ch))
    error ("hex2num: invalid character '%c' found in string S", ch);

  if (ch >= 'a')
    val = static_cast<unsigned char> (ch - 'a' + 10);
  else if (ch >= 'A')
    val = static_cast<unsigned char> (ch - 'A' + 10);
  else
    val = static_cast<unsigned char> (ch - '0');

  return val;
}

static void
hex2num (const std::string& hex, void *num, std::size_t nbytes, bool swap_bytes)
{
  unsigned char *cp = reinterpret_cast<unsigned char *> (num);

  const std::size_t nc = hex.length ();
  const std::size_t nchars = 2 * nbytes;

  if (nc > nchars)
    error ("hex2num: S must be no more than %zd characters", nchars);

  std::size_t j = 0;

  for (std::size_t i = 0; i < nbytes; i++)
    {
      std::size_t k = (swap_bytes ? nbytes - i - 1 : i);

      unsigned char ch1 = (j < nc) ? hex[j++] : '0';
      unsigned char ch2 = (j < nc) ? hex[j++] : '0';

      cp[k] = (hex2nibble (ch1) << 4) + hex2nibble (ch2);
    }
}

template <typename T>
Array<T>
hex2num (const Array<std::string>& val, bool swap_bytes)
{
  octave_idx_type nel = val.numel ();

  Array<T> m (val.dims ());

  std::size_t nbytes = sizeof (T);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      T num;

      hex2num (val.xelem (i), &num, nbytes, swap_bytes);

      m(i) = num;
    }

  return m;
}

DEFUN (hex2num, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{n} =} hex2num (@var{s})
@deftypefnx {} {@var{n} =} hex2num (@var{s}, @var{class})
Typecast a hexadecimal character array or cell array of strings to an
array of numbers.

By default, the input array is interpreted as a hexadecimal number
representing a double precision value.  If fewer than 16 characters are
given the strings are right padded with @qcode{'0'} characters.

Given a string matrix, @code{hex2num} treats each row as a separate number.

@example
@group
hex2num (["4005bf0a8b145769"; "4024000000000000"])
   @result{} [2.7183; 10.000]
@end group
@end example

The optional second argument @var{class} may be used to cause the input
array to be interpreted as a different value type.  Possible values are

@multitable {Option} {Characters}
@headitem Option @tab Characters
@item @qcode{"int8"} @tab 2
@item @qcode{"uint8"} @tab 2
@item @qcode{"int16"} @tab 4
@item @qcode{"uint16"} @tab 4
@item @qcode{"int32"} @tab 8
@item @qcode{"uint32"} @tab 8
@item @qcode{"int64"} @tab 16
@item @qcode{"uint64"} @tab 16
@item @qcode{"char"} @tab 2
@item @qcode{"single"} @tab 8
@item @qcode{"double"} @tab 16
@end multitable

For example:

@example
@group
hex2num (["402df854"; "41200000"], "single")
   @result{} [2.7183; 10.000]
@end group
@end example
@seealso{num2hex, hex2dec, dec2hex}
@end deftypefn */)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string type = "double";
  if (nargin == 2)
    type = args(1).xstring_value ("hex2num: CLASS must be a string");

  Array<std::string> val = args(0).cellstr_value ();

  // We always use big-endian order for hex digits.
  bool is_float = type == "single" || type == "double";
  bool swap_bytes = is_little_endian (is_float);

  if (type == "int8")
    retval = octave_value (hex2num<octave_int8> (val, swap_bytes));
  else if (type == "uint8")
    retval = octave_value (hex2num<octave_uint8> (val, swap_bytes));
  else if (type == "int16")
    retval = octave_value (hex2num<octave_int16> (val, swap_bytes));
  else if (type == "uint16")
    retval = octave_value (hex2num<octave_uint16> (val, swap_bytes));
  else if (type == "int32")
    retval = octave_value (hex2num<octave_int32> (val, swap_bytes));
  else if (type == "uint32")
    retval = octave_value (hex2num<octave_uint32> (val, swap_bytes));
  else if (type == "int64")
    retval = octave_value (hex2num<octave_int64> (val, swap_bytes));
  else if (type == "uint64")
    retval = octave_value (hex2num<octave_uint64> (val, swap_bytes));
  else if (type == "char")
    retval = octave_value (hex2num<char> (val, swap_bytes));
  else if (type == "single")
    retval = octave_value (hex2num<float> (val, swap_bytes));
  else if (type == "double")
    retval = octave_value (hex2num<double> (val, swap_bytes));
  else
    error ("hex2num: unrecognized CLASS '%s'", type.c_str ());

  return retval;
}

/*
%!assert (hex2num (["c00";"bff";"000";"3ff";"400"]), [-2:2]')
%!assert (hex2num (["c00";"bf8";"000";"3f8";"400"], "single"), single([-2:2])')
%!assert (hex2num ("ff", "uint8"), intmax ("uint8"))
%!assert (hex2num ("ffff", "uint16"), intmax ("uint16"))
%!assert (hex2num ("ffffffff", "uint32"), intmax ("uint32"))
%!assert (hex2num ("ffffffff", "uint32"), intmax ("uint32"))
%!assert (hex2num ("ffffffffffffffff", "uint64"), intmax ("uint64"))
*/

static inline unsigned char
nibble2hex (unsigned char ch)
{
  if (ch >= 10)
    ch += 'a' - 10;
  else
    ch += '0';

  return ch;
}

static inline void
num2hex (const void *p, std::size_t n, char *hex, bool swap_bytes)
{
  const unsigned char *cp = reinterpret_cast<const unsigned char *> (p);

  std::size_t k = 0;

  for (std::size_t i = 0; i < n; i++)
    {
      std::size_t j = (swap_bytes ? n - i - 1 : i);

      unsigned char ch = cp[j];

      hex[k++] = nibble2hex ((ch >> 4) & 0xF);
      hex[k++] = nibble2hex (ch & 0xF);
    }
}

template <typename T>
Cell
num2hex (const Array<T>& v, bool swap_bytes)
{
  const std::size_t nbytes = sizeof (T);
  const std::size_t nchars = 2 * nbytes;

  octave_idx_type nel = v.numel ();

  string_vector sv (nel);

  const T *pv = v.data ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      char hex[nchars];

      num2hex (pv++, nbytes, hex, swap_bytes);

      sv[i] = std::string (hex, nchars);
    }

  return Cell (v.dims (), sv);
}

DEFUN (num2hex, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} num2hex (@var{n})
@deftypefnx {} {@var{s} =} num2hex (@var{n}, "cell")
Convert a numeric array to an array of hexadecimal strings.

For example:

@example
@group
num2hex ([-1, 1, e, Inf])
@result{} "bff0000000000000
    3ff0000000000000
    4005bf0a8b145769
    7ff0000000000000"
@end group
@end example

If the argument @var{n} is a single precision number or vector, the returned
string has a length of 8.  For example:

@example
@group
num2hex (single ([-1, 1, e, Inf]))
@result{} "bf800000
    3f800000
    402df854
    7f800000"
@end group
@end example

With the optional second argument @qcode{"cell"}, return a cell array of
strings instead of a character array.
@seealso{hex2num, hex2dec, dec2hex}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  bool as_cell = false;

  if (nargin == 2)
    {
      std::string opt = args(1).xstring_value ("num2hex: second argument must be a string");
      if (opt == "cell")
        as_cell = true;
      else
        error ("num2hex: unrecognized option '%s'", opt.c_str ());
    }

  octave_value val = args(0);

  if (val.iscomplex ())
    error ("num2hex: N must be real");

  Cell result;

  // We always use big-endian order for hex digits.
  bool is_float = val.is_single_type () || val.is_double_type ();
  bool swap_bytes = is_little_endian (is_float);

  if (val.is_int8_type ())
    result = num2hex (val.int8_array_value (), swap_bytes);
  else if (val.is_int16_type ())
    result = num2hex<octave_int16> (val.int16_array_value (), swap_bytes);
  else if (val.is_int32_type ())
    result = num2hex<octave_int32> (val.int32_array_value (), swap_bytes);
  else if (val.is_int64_type ())
    result = num2hex<octave_int64> (val.int64_array_value (), swap_bytes);
  else if (val.is_uint8_type ())
    result = num2hex<octave_uint8> (val.uint8_array_value (), swap_bytes);
  else if (val.is_uint16_type ())
    result = num2hex<octave_uint16> (val.uint16_array_value (), swap_bytes);
  else if (val.is_uint32_type ())
    result = num2hex<octave_uint32> (val.uint32_array_value (), swap_bytes);
  else if (val.is_uint64_type ())
    result = num2hex<octave_uint64> (val.uint64_array_value (), swap_bytes);
  else if (val.is_char_matrix ())
    result = num2hex<char> (val.char_array_value (), swap_bytes);
  else if (val.is_single_type ())
    result = num2hex<float> (val.float_vector_value (), swap_bytes);
  else if (val.is_double_type ())
    result = num2hex<double> (val.vector_value (), swap_bytes);
  else
    err_wrong_type_arg ("num2hex", val);

  return (as_cell
          ? octave_value (result)
          : octave_value (result.string_vector_value ()));
}

/*
%!assert (num2hex (-2:2),
%!        ["c000000000000000";"bff0000000000000";"0000000000000000";"3ff0000000000000";"4000000000000000"])
%!assert (num2hex (single (-2:2)),
%!        ["c0000000";"bf800000";"00000000";"3f800000";"40000000"])
%!assert (num2hex (intmax ("uint8")), "ff")
%!assert (num2hex (intmax ("uint16")), "ffff")
%!assert (num2hex (intmax ("uint32")), "ffffffff")
%!assert (num2hex (intmax ("uint32")), "ffffffff")
%!assert (num2hex (intmax ("uint64")), "ffffffffffffffff")

%!assert (hex2num (num2hex (pi)), pi)
%!assert (hex2num (num2hex (single (pi)), "single"), single (pi))

%!error num2hex ()
%!error num2hex (1,2)
%!error num2hex (1,"foo")
%!error num2hex (1,2,3)
%!error num2hex (1j)
*/

OCTAVE_END_NAMESPACE(octave)
