/*

Copyright (C) 1994-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>

#include <queue>
#include <sstream>

#include "dMatrix.h"
#include "localcharset-wrapper.h"
#include "uniconv-wrappers.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"
#include "unwind-prot.h"
#include "utils.h"

#include "oct-string.h"

DEFUN (char, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} char (@var{x})
@deftypefnx {} {} char (@var{x}, @dots{})
@deftypefnx {} {} char (@var{s1}, @var{s2}, @dots{})
@deftypefnx {} {} char (@var{cell_array})
Create a string array from one or more numeric matrices, character
matrices, or cell arrays.

Arguments are concatenated vertically.  The returned values are padded with
blanks as needed to make each row of the string array have the same length.
Empty input strings are significant and will concatenated in the output.

For numerical input, each element is converted to the corresponding ASCII
character.  A range error results if an input is outside the ASCII range
(0-255).

For cell arrays, each element is concatenated separately.  Cell arrays
converted through @code{char} can mostly be converted back with
@code{cellstr}.  For example:

@example
@group
char ([97, 98, 99], "", @{"98", "99", 100@}, "str1", ["ha", "lf"])
   @result{} ["abc    "
       "       "
       "98     "
       "99     "
       "d      "
       "str1   "
       "half   "]
@end group
@end example
@seealso{strvcat, cellstr}
@end deftypefn */)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = "";
  else if (nargin == 1)
    retval = args(0).convert_to_str (true, true,
                                     args(0).is_dq_string () ? '"' : '\'');
  else
    {
      int n_elts = 0;

      int max_len = 0;

      std::queue<string_vector> args_as_strings;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args(i).xstring_vector_value ("char: unable to convert some args to strings");

          if (s.numel () > 0)
            n_elts += s.numel ();
          else
            n_elts += 1;

          int s_max_len = s.max_length ();

          if (s_max_len > max_len)
            max_len = s_max_len;

          args_as_strings.push (s);
        }

      string_vector result (n_elts);

      int k = 0;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args_as_strings.front ();
          args_as_strings.pop ();

          int n = s.numel ();

          if (n > 0)
            {
              for (int j = 0; j < n; j++)
                {
                  std::string t = s[j];
                  int t_len = t.length ();

                  if (max_len > t_len)
                    t += std::string (max_len - t_len, ' ');

                  result[k++] = t;
                }
            }
          else
            result[k++] = std::string (max_len, ' ');
        }

      retval = octave_value (result, '\'');
    }

  return retval;
}

/*
%!assert (char (), '')
%!assert (char (100), "d")
%!assert (char (100,100), ["d";"d"])
%!assert (char ({100,100}), ["d";"d"])
%!assert (char ([100,100]), ["dd"])
%!assert (char ({100,{100}}), ["d";"d"])
%!assert (char (100, [], 100), ["d";" ";"d"])
%!assert (char ({100, [], 100}), ["d";" ";"d"])
%!assert (char ({100,{100, {""}}}), ["d";"d";" "])
%!assert (char (["a";"be"], {"c", 100}), ["a";"be";"c";"d"])
%!assert (char ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"])
%!assert (char ([65, 83, 67, 73, 73]), "ASCII")

%!test
%! x = char ("foo", "bar", "foobar");
%! assert (x(1,:), "foo   ");
%! assert (x(2,:), "bar   ");
%! assert (x(3,:), "foobar");
*/

DEFUN (strvcat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} strvcat (@var{x})
@deftypefnx {} {} strvcat (@var{x}, @dots{})
@deftypefnx {} {} strvcat (@var{s1}, @var{s2}, @dots{})
@deftypefnx {} {} strvcat (@var{cell_array})
Create a character array from one or more numeric matrices, character
matrices, or cell arrays.

Arguments are concatenated vertically.  The returned values are padded with
blanks as needed to make each row of the string array have the same length.
Unlike @code{char}, empty strings are removed and will not appear in the
output.

For numerical input, each element is converted to the corresponding ASCII
character.  A range error results if an input is outside the ASCII range
(0-255).

For cell arrays, each element is concatenated separately.  Cell arrays
converted through @code{strvcat} can mostly be converted back with
@code{cellstr}.  For example:

@example
@group
strvcat ([97, 98, 99], "", @{"98", "99", 100@}, "str1", ["ha", "lf"])
      @result{} ["abc    "
          "98     "
          "99     "
          "d      "
          "str1   "
          "half   "]
@end group
@end example
@seealso{char, strcat, cstrcat}
@end deftypefn */)
{
  int nargin = args.length ();
  int n_elts = 0;
  size_t max_len = 0;
  std::queue<string_vector> args_as_strings;

  for (int i = 0; i < nargin; i++)
    {
      string_vector s = args(i).xstring_vector_value ("strvcat: unable to convert some args to strings");

      size_t n = s.numel ();

      // do not count empty strings in calculation of number of elements
      if (n > 0)
        {
          for (size_t j = 0; j < n; j++)
            {
              if (s[j].length () > 0)
                n_elts++;
            }
        }

      size_t s_max_len = s.max_length ();

      if (s_max_len > max_len)
        max_len = s_max_len;

      args_as_strings.push (s);
    }

  string_vector result (n_elts);

  octave_idx_type k = 0;

  for (int i = 0; i < nargin; i++)
    {
      string_vector s = args_as_strings.front ();
      args_as_strings.pop ();

      size_t n = s.numel ();

      if (n > 0)
        {
          for (size_t j = 0; j < n; j++)
            {
              std::string t = s[j];
              if (t.length () > 0)
                {
                  size_t t_len = t.length ();

                  if (max_len > t_len)
                    t += std::string (max_len - t_len, ' ');

                  result[k++] = t;
                }
            }
        }
    }

  // Cannot use ovl.  Relies on overloaded octave_value call.
  return octave_value (result, '\'');
}

/*
%!assert (strvcat (""), "")
%!assert (strvcat (100) == "d")
%!assert (strvcat (100,100), ["d";"d"])
%!assert (strvcat ({100,100}), ["d";"d"])
%!assert (strvcat ([100,100]), ["dd"])
%!assert (strvcat ({100,{100}}), ["d";"d"])
%!assert (strvcat (100, [], 100), ["d";"d"])
%!assert (strvcat ({100, [], 100}), ["d";"d"])
%!assert (strvcat ({100,{100, {""}}}), ["d";"d"])
%!assert (strvcat (["a";"be"], {"c", 100}), ["a";"be";"c";"d"])
%!assert (strvcat ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"])
%!assert (strvcat (), "")
*/

DEFUN (ischar, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} ischar (@var{x})
Return true if @var{x} is a character array.
@seealso{isfloat, isinteger, islogical, isnumeric, iscellstr, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_string ());
}

/*
%!assert (ischar ("a"), true)
%!assert (ischar (["ab";"cd"]), true)
%!assert (ischar ({"ab"}), false)
%!assert (ischar (1), false)
%!assert (ischar ([1, 2]), false)
%!assert (ischar ([]), false)
%!assert (ischar ([1, 2; 3, 4]), false)
%!assert (ischar (""), true)
%!assert (ischar ("test"), true)
%!assert (ischar (["test"; "ing"]), true)
%!assert (ischar (struct ("foo", "bar")), false)

%!error ischar ()
%!error ischar ("test", 1)
*/

static octave_value
do_strcmp_fun (const octave_value& arg0, const octave_value& arg1,
               octave_idx_type n, const char *fcn_name,
               bool (*array_op) (const Array<char>&, const Array<char>&,
                                 octave_idx_type),
               bool (*str_op) (const std::string&, const std::string&,
                               std::string::size_type))

{
  octave_value retval;

  bool s1_string = arg0.is_string ();
  bool s1_cell = arg0.is_cell ();
  bool s2_string = arg1.is_string ();
  bool s2_cell = arg1.is_cell ();

  if (s1_string && s2_string)
    retval = array_op (arg0.char_array_value (), arg1.char_array_value (), n);
  else if ((s1_string && s2_cell) || (s1_cell && s2_string))
    {
      octave_value str_val, cell_val;

      if (s1_string)
        {
          str_val = arg0;
          cell_val = arg1;
        }
      else
        {
          str_val = arg1;
          cell_val = arg0;
        }

      const Cell cell = cell_val.cell_value ();
      const string_vector str = str_val.string_vector_value ();
      octave_idx_type r = str.numel ();

      if (r == 0 || r == 1)
        {
          // Broadcast the string.

          boolNDArray output (cell_val.dims (), false);

          std::string s = (r == 0 ? "" : str[0]);

          if (cell_val.is_cellstr ())
            {
              const Array<std::string> cellstr = cell_val.cellstr_value ();
              for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                output(i) = str_op (cellstr(i), s, n);
            }
          else
            {
              // FIXME: should we warn here?
              for (octave_idx_type i = 0; i < cell.numel (); i++)
                {
                  if (cell(i).is_string ())
                    output(i) = str_op (cell(i).string_value (), s, n);
                }
            }

          retval = output;
        }
      else if (r > 1)
        {
          if (cell.numel () == 1)
            {
              // Broadcast the cell.

              const dim_vector dv (r, 1);
              boolNDArray output (dv, false);

              if (cell(0).is_string ())
                {
                  const std::string str2 = cell(0).string_value ();

                  for (octave_idx_type i = 0; i < r; i++)
                    output(i) = str_op (str[i], str2, n);
                }

              retval = output;
            }
          else
            {
              // Must match in all dimensions.

              boolNDArray output (cell.dims (), false);

              if (cell.numel () == r)
                {
                  if (cell_val.is_cellstr ())
                    {
                      const Array<std::string> cellstr
                        = cell_val.cellstr_value ();
                      for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                        output(i) = str_op (str[i], cellstr(i), n);
                    }
                  else
                    {
                      // FIXME: should we warn here?
                      for (octave_idx_type i = 0; i < r; i++)
                        {
                          if (cell(i).is_string ())
                            output(i) = str_op (str[i],
                                                cell(i).string_value (), n);
                        }
                    }

                  retval = output;
                }
              else
                retval = false;
            }
        }
    }
  else if (s1_cell && s2_cell)
    {
      octave_value cell1_val, cell2_val;
      octave_idx_type r1 = arg0.numel (), r2;

      if (r1 == 1)
        {
          // Make the singleton cell2.

          cell1_val = arg1;
          cell2_val = arg0;
        }
      else
        {
          cell1_val = arg0;
          cell2_val = arg1;
        }

      const Cell cell1 = cell1_val.cell_value ();
      const Cell cell2 = cell2_val.cell_value ();
      r1 = cell1.numel ();
      r2 = cell2.numel ();

      const dim_vector size1 = cell1.dims ();
      const dim_vector size2 = cell2.dims ();

      boolNDArray output (size1, false);

      if (r2 == 1)
        {
          // Broadcast cell2.

          if (cell2(0).is_string ())
            {
              const std::string str2 = cell2(0).string_value ();

              if (cell1_val.is_cellstr ())
                {
                  const Array<std::string> cellstr = cell1_val.cellstr_value ();
                  for (octave_idx_type i = 0; i < cellstr.numel (); i++)
                    output(i) = str_op (cellstr(i), str2, n);
                }
              else
                {
                  // FIXME: should we warn here?
                  for (octave_idx_type i = 0; i < r1; i++)
                    {
                      if (cell1(i).is_string ())
                        {
                          const std::string str1 = cell1(i).string_value ();
                          output(i) = str_op (str1, str2, n);
                        }
                    }
                }
            }
        }
      else
        {
          if (size1 != size2)
            error ("%s: nonconformant cell arrays", fcn_name);

          if (cell1.is_cellstr () && cell2.is_cellstr ())
            {
              const Array<std::string> cellstr1 = cell1_val.cellstr_value ();
              const Array<std::string> cellstr2 = cell2_val.cellstr_value ();
              for (octave_idx_type i = 0; i < r1; i++)
                output (i) = str_op (cellstr1(i), cellstr2(i), n);
            }
          else
            {
              // FIXME: should we warn here?
              for (octave_idx_type i = 0; i < r1; i++)
                {
                  if (cell1(i).is_string () && cell2(i).is_string ())
                    {
                      const std::string str1 = cell1(i).string_value ();
                      const std::string str2 = cell2(i).string_value ();
                      output(i) = str_op (str1, str2, n);
                    }
                }
            }
        }

      retval = output;
    }
  else
    retval = false;

  return retval;
}


// These are required so that they match the same signature as strncmp
// and strncmpi and can therefore be used in do_strcmp_fun.

template <typename T, typename T_size_type>
static bool
strcmp_ignore_n (const T& s1, const T& s2, T_size_type)
{ return octave::string::strcmp (s1, s2); }

template <typename T, typename T_size_type>
static bool
strcmpi_ignore_n (const T& s1, const T& s2, T_size_type)
{ return octave::string::strcmpi (s1, s2); }


DEFUN (strcmp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} strcmp (@var{s1}, @var{s2})
Return 1 if the character strings @var{s1} and @var{s2} are the same,
and 0 otherwise.

If either @var{s1} or @var{s2} is a cell array of strings, then an array
of the same size is returned, containing the values described above for
every member of the cell array.  The other argument may also be a cell
array of strings (of the same size or with only one element), char matrix
or character string.

@strong{Caution:} For compatibility with @sc{matlab}, Octave's strcmp
function returns 1 if the character strings are equal, and 0 otherwise.
This is just the opposite of the corresponding C library function.
@seealso{strcmpi, strncmp, strncmpi}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  return ovl (do_strcmp_fun (args(0), args(1), 0, "strcmp",
                             strcmp_ignore_n, strcmp_ignore_n));
}

/*
%!shared x
%! x = char (zeros (0, 2));
%!assert (strcmp ("", x), false)
%!assert (strcmp (x, ""), false)
%!assert (strcmp (x, x), true)
## %!assert (strcmp ({""}, x), true)
## %!assert (strcmp ({x}, ""), false)
## %!assert (strcmp ({x}, x), true)
## %!assert (strcmp ("", {x}), false)
## %!assert (strcmp (x, {""}), false)
## %!assert (strcmp (x, {x}), true)
## %!assert (strcmp ({x; x}, ""), [false; false])
## %!assert (strcmp ({x; x}, {""}), [false; false])
## %!assert (strcmp ("", {x; x}), [false; false])
## %!assert (strcmp ({""}, {x; x}), [false; false])
%!assert (strcmp ({"foo"}, x), false)
%!assert (strcmp ({"foo"}, "foo"), true)
%!assert (strcmp ({"foo"}, x), false)
%!assert (strcmp (x, {"foo"}), false)
%!assert (strcmp ("foo", {"foo"}), true)
%!assert (strcmp (x, {"foo"}), false)
%!shared y
%! y = char (zeros (2, 0));
%!assert (strcmp ("", y), false)
%!assert (strcmp (y, ""), false)
%!assert (strcmp (y, y), true)
%!assert (strcmp ({""}, y), [true; true])
%!assert (strcmp ({y}, ""), true)
%!assert (strcmp ({y}, y), [true; true])
%!assert (strcmp ("", {y}), true)
%!assert (strcmp (y, {""}), [true; true])
%!assert (strcmp (y, {y}), [true; true])
%!assert (strcmp ({y; y}, ""), [true; true])
%!assert (strcmp ({y; y}, {""}), [true; true])
%!assert (strcmp ("", {y; y}), [true; true])
%!assert (strcmp ({""}, {y; y}), [true; true])
%!assert (strcmp ({"foo"}, y), [false; false])
%!assert (strcmp ({"foo"}, y), [false; false])
%!assert (strcmp (y, {"foo"}), [false; false])
%!assert (strcmp (y, {"foo"}), [false; false])
%!assert (strcmp ("foobar", "foobar"), true)
%!assert (strcmp ("fooba", "foobar"), false)

%!error strcmp ()
%!error strcmp ("foo", "bar", 3)
*/

DEFUN (strncmp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} strncmp (@var{s1}, @var{s2}, @var{n})
Return 1 if the first @var{n} characters of strings @var{s1} and @var{s2}
are the same, and 0 otherwise.

@example
@group
strncmp ("abce", "abcd", 3)
      @result{} 1
@end group
@end example

If either @var{s1} or @var{s2} is a cell array of strings, then an array
of the same size is returned, containing the values described above for
every member of the cell array.  The other argument may also be a cell
array of strings (of the same size or with only one element), char matrix
or character string.

@example
@group
strncmp ("abce", @{"abcd", "bca", "abc"@}, 3)
     @result{} [1, 0, 1]
@end group
@end example

@strong{Caution:} For compatibility with @sc{matlab}, Octave's strncmp
function returns 1 if the character strings are equal, and 0 otherwise.
This is just the opposite of the corresponding C library function.
@seealso{strncmpi, strcmp, strcmpi}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_idx_type n = args(2).idx_type_value ();

  if (n > 0)
    return ovl (do_strcmp_fun (args(0), args(1), n, "strncmp",
                               octave::string::strncmp,
                               octave::string::strncmp));
  else
    error ("strncmp: N must be greater than 0");
}

/*
%!assert (strncmp ("abce", "abc", 3), true)
%!assert (strncmp (100, 100, 1), false)
%!assert (strncmp ("abce", {"abcd", "bca", "abc"}, 3), logical ([1, 0, 1]))
%!assert (strncmp ("abc",  {"abcd", "bca", "abc"}, 4), logical ([0, 0, 0]))
%!assert (strncmp ({"abcd", "bca", "abc"},"abce", 3), logical ([1, 0, 1]))
%!assert (strncmp ({"abcd", "bca", "abc"},{"abcd", "bca", "abe"}, 3), logical ([1, 1, 0]))
%!assert (strncmp ("abc", {"abcd", 10}, 2), logical ([1, 0]))

%!error strncmp ()
%!error strncmp ("abc", "def")
*/

DEFUNX ("strcmpi", Fstrcmpi, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {} strcmpi (@var{s1}, @var{s2})
Return 1 if the character strings @var{s1} and @var{s2} are the same,
disregarding case of alphabetic characters, and 0 otherwise.

If either @var{s1} or @var{s2} is a cell array of strings, then an array
of the same size is returned, containing the values described above for
every member of the cell array.  The other argument may also be a cell
array of strings (of the same size or with only one element), char matrix
or character string.

@strong{Caution:} For compatibility with @sc{matlab}, Octave's strcmp
function returns 1 if the character strings are equal, and 0 otherwise.
This is just the opposite of the corresponding C library function.

@strong{Caution:} National alphabets are not supported.
@seealso{strcmp, strncmp, strncmpi}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  return ovl (do_strcmp_fun (args(0), args(1), 0, "strcmpi",
                             strcmpi_ignore_n, strcmpi_ignore_n));
}

/*
%!assert (strcmpi ("abc123", "ABC123"), true)
*/

DEFUNX ("strncmpi", Fstrncmpi, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {} strncmpi (@var{s1}, @var{s2}, @var{n})
Return 1 if the first @var{n} character of @var{s1} and @var{s2} are the
same, disregarding case of alphabetic characters, and 0 otherwise.

If either @var{s1} or @var{s2} is a cell array of strings, then an array
of the same size is returned, containing the values described above for
every member of the cell array.  The other argument may also be a cell
array of strings (of the same size or with only one element), char matrix
or character string.

@strong{Caution:} For compatibility with @sc{matlab}, Octave's strncmpi
function returns 1 if the character strings are equal, and 0 otherwise.
This is just the opposite of the corresponding C library function.

@strong{Caution:} National alphabets are not supported.
@seealso{strncmp, strcmp, strcmpi}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_idx_type n = args(2).idx_type_value ();

  if (n > 0)
    return ovl (do_strcmp_fun (args(0), args(1), n, "strncmpi",
                               octave::string::strncmpi,
                               octave::string::strncmpi));
  else
    error ("strncmpi: N must be greater than 0");
}

/*
%!assert (strncmpi ("abc123", "ABC456", 3), true)
*/

DEFUN (__native2unicode__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{utf8_str} =} __native2unicode__ (@var{native_bytes}, @var{codepage})
Convert byte stream @var{native_bytes} to UTF-8 using @var{codepage}.

@seealso{native2unicode, __unicode2native__}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 2)
    print_usage ();

  if (args(0).is_string ())
    return ovl (args(0));

  std::string tmp = args(1).xstring_value ("CODEPAGE must be a string");
  const char *codepage
    = (tmp.empty () ? octave_locale_charset_wrapper () : tmp.c_str ());

  charNDArray native_bytes = args(0).char_array_value ();

  const char *src = native_bytes.data ();
  size_t srclen = native_bytes.numel ();

  size_t length;
  uint8_t *utf8_str = 0;

  octave::unwind_protect frame;

  frame.add_fcn (::free, static_cast<void *> (utf8_str));

  utf8_str = octave_u8_conv_from_encoding (codepage, src, srclen, &length);

  if (! utf8_str)
    error ("native2unicode: converting from codepage '%s' to UTF-8: %s",
           codepage, std::strerror (errno));

  octave_idx_type len = length;

  charNDArray retval (dim_vector (1, len));

  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem(i) = utf8_str[i];

  return ovl (retval);
}

DEFUN (__unicode2native__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{native_bytes} =} __unicode2native__ (@var{utf8_str}, @var{codepage})
Convert UTF-8 string @var{utf8_str} to byte stream @var{native_bytes} using
@var{codepage}.

@seealso{unicode2native, __native2unicode__}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 2)
    print_usage ();

  std::string tmp = args(1).xstring_value ("CODEPAGE must be a string");
  const char *codepage
    = (tmp.empty () ? octave_locale_charset_wrapper () : tmp.c_str ());

  charNDArray utf8_str = args(0).xchar_array_value ("UTF8_STR must be a string");

  const uint8_t *src = reinterpret_cast<const uint8_t *> (utf8_str.data ());
  size_t srclen = utf8_str.numel ();

  size_t length;
  char *native_bytes = 0;

  octave::unwind_protect frame;

  frame.add_fcn (::free, static_cast<void *> (native_bytes));

  native_bytes = octave_u8_conv_to_encoding (codepage, src, srclen, &length);

  if (! native_bytes)
    error ("native2unicode: converting from UTF-8 to codepage '%s': %s",
           codepage, std::strerror (errno));

  octave_idx_type len = length;

  uint8NDArray retval (dim_vector (1, len));

  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem(i) = native_bytes[i];

  return ovl (retval);
}

DEFUN (list_in_columns, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} list_in_columns (@var{arg}, @var{width}, @var{prefix})
Return a string containing the elements of @var{arg} listed in columns with
an overall maximum width of @var{width} and optional prefix @var{prefix}.

The argument @var{arg} must be a cell array of character strings or a
character array.

If @var{width} is not specified or is an empty matrix, or less than or equal
to zero, the width of the terminal screen is used.  Newline characters are
used to break the lines in the output string.  For example:
@c Set example in small font to prevent overfull line

@smallexample
@group
list_in_columns (@{"abc", "def", "ghijkl", "mnop", "qrs", "tuv"@}, 20)
     @result{} abc     mnop
        def     qrs
        ghijkl  tuv

whos ans
     @result{}
     Variables in the current scope:

       Attr Name        Size                     Bytes  Class
       ==== ====        ====                     =====  =====
            ans         1x37                        37  char

     Total is 37 elements using 37 bytes
@end group
@end smallexample

@seealso{terminal_size}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  string_vector s = args(0).xstring_vector_value ("list_in_columns: ARG must be a cellstr or char array");

  int width = -1;

  if (nargin > 1 && ! args(1).is_empty ())
    width = args(1).xint_value ("list_in_columns: WIDTH must be an integer");

  std::string prefix;

  if (nargin > 2)
    prefix = args(2).xstring_value ("list_in_columns: PREFIX must be a string");

  std::ostringstream buf;

  s.list_in_columns (buf, width, prefix);

  return ovl (buf.str ());
}

/*
%!test
%! input  = {"abc", "def", "ghijkl", "mnop", "qrs", "tuv"};
%! result = "abc     mnop\ndef     qrs\nghijkl  tuv\n";
%! assert (list_in_columns (input, 20), result);
%!test
%! input  = ["abc"; "def"; "ghijkl"; "mnop"; "qrs"; "tuv"];
%! result = "abc     mnop  \ndef     qrs   \nghijkl  tuv   \n";
%! assert (list_in_columns (input, 20), result);
%!test
%! input  = ["abc"; "def"; "ghijkl"; "mnop"; "qrs"; "tuv"];
%! result = "  abc     mnop  \n  def     qrs   \n  ghijkl  tuv   \n";
%! assert (list_in_columns (input, 20, "  "), result);

%!error list_in_columns ()
%!error list_in_columns (["abc", "def"], 20, 2)
%!error list_in_columns (["abc", "def"], 20, "  ", 3)
%!error <list_in_columns: WIDTH must be an integer> list_in_columns (["abc", "def"], "a")
*/
