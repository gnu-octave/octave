////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include <cctype>

#include <queue>
#include <sstream>

#include "dMatrix.h"
#include "localcharset-wrapper.h"
#include "uniconv-wrappers.h"
#include "unistr-wrappers.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"
#include "unwind-prot.h"
#include "utils.h"

#include "oct-string.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (char, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} char (@var{A})
@deftypefnx {} {@var{C} =} char (@var{A}, @dots{})
@deftypefnx {} {@var{C} =} char (@var{str1}, @var{str2}, @dots{})
@deftypefnx {} {@var{C} =} char (@var{cell_array})
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
   @result{} ["abc "
       "    "
       "98  "
       "99  "
       "d   "
       "str1"
       "half"]
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
%!assert (char (["a ";"be"], {"c", 100}), ["a ";"be";"c ";"d "])
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
@deftypefn  {} {@var{C} =} strvcat (@var{A})
@deftypefnx {} {@var{C} =} strvcat (@var{A}, @dots{})
@deftypefnx {} {@var{C} =} strvcat (@var{str1}, @var{str2}, @dots{})
@deftypefnx {} {@var{C} =} strvcat (@var{cell_array})
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
      @result{} ["abc "
          "98  "
          "99  "
          "d   "
          "str1"
          "half"]
@end group
@end example
@seealso{char, strcat, cstrcat}
@end deftypefn */)
{
  int nargin = args.length ();
  int n_elts = 0;
  std::size_t max_len = 0;
  std::queue<string_vector> args_as_strings;

  for (int i = 0; i < nargin; i++)
    {
      string_vector s = args(i).xstring_vector_value ("strvcat: unable to convert some args to strings");

      std::size_t n = s.numel ();

      // do not count empty strings in calculation of number of elements
      if (n > 0)
        {
          for (std::size_t j = 0; j < n; j++)
            {
              if (! s[j].empty ())
                n_elts++;
            }
        }

      std::size_t s_max_len = s.max_length ();

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

      std::size_t n = s.numel ();

      if (n > 0)
        {
          for (std::size_t j = 0; j < n; j++)
            {
              std::string t = s[j];
              if (t.length () > 0)
                {
                  std::size_t t_len = t.length ();

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
%!assert (strvcat (["a ";"be"], {"c", 100}), ["a ";"be";"c ";"d "])
%!assert (strvcat ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"])
%!assert (strvcat (), "")
*/

DEFUN (ischar, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} ischar (@var{x})
Return true if @var{x} is a character array.
@seealso{isfloat, isinteger, islogical, isnumeric, isstring, iscellstr, isa}
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
do_strcmp_fcn (const octave_value& arg0, const octave_value& arg1,
               octave_idx_type n, const char *fcn_name,
               bool (*array_op) (const Array<char>&, const Array<char>&,
                                 octave_idx_type),
               bool (*str_op) (const std::string&, const std::string&,
                               std::string::size_type))

{
  octave_value retval;

  bool s1_string = arg0.is_string ();
  bool s1_cell = arg0.iscell ();
  bool s2_string = arg1.is_string ();
  bool s2_cell = arg1.iscell ();

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

          if (cell_val.iscellstr ())
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
                  if (cell_val.iscellstr ())
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

              if (cell1_val.iscellstr ())
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

          if (cell1.iscellstr () && cell2.iscellstr ())
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
// and strncmpi and can therefore be used in do_strcmp_fcn.

template <typename T, typename T_size_type>
static bool
strcmp_ignore_n (const T& s1, const T& s2, T_size_type)
{ return string::strcmp (s1, s2); }

template <typename T, typename T_size_type>
static bool
strcmpi_ignore_n (const T& s1, const T& s2, T_size_type)
{ return string::strcmpi (s1, s2); }


DEFUN (strcmp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} strcmp (@var{str1}, @var{str2})
Return 1 if the character strings @var{str1} and @var{str2} are the same,
and 0 otherwise.

If either @var{str1} or @var{str2} is a cell array of strings, then an array
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

  return ovl (do_strcmp_fcn (args(0), args(1), 0, "strcmp",
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
%!assert (strcmp ("foobar", "fooBar"), false)
%!assert (strcmp ("fooba", "foobar"), false)

%!error strcmp ()
%!error strcmp ("foo", "bar", 3)
*/

DEFUN (strncmp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} strncmp (@var{str1}, @var{str2}, @var{n})
Return 1 if the first @var{n} characters of strings @var{str1} and @var{str2}
are the same, and 0 otherwise.

@example
@group
strncmp ("abce", "abcd", 3)
      @result{} 1
@end group
@end example

If either @var{str1} or @var{str2} is a cell array of strings, then an array
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
    return ovl (do_strcmp_fcn (args(0), args(1), n, "strncmp",
                               string::strncmp,
                               string::strncmp));
  else
    error ("strncmp: N must be greater than 0");
}

/*
%!assert (strncmp ("abce", "abc", 3), true)
%!assert (strncmp ("abce", "aBc", 3), false)
%!assert (strncmp (100, 100, 1), false)
%!assert (strncmp ("abce", {"abcd", "bca", "abc"}, 3), logical ([1, 0, 1]))
%!assert (strncmp ("abc",  {"abcd", "bca", "abc"}, 4), logical ([0, 0, 1]))
%!assert (strncmp ({"abcd", "bca", "abc"},"abce", 3), logical ([1, 0, 1]))
%!assert (strncmp ({"abcd", "bca", "abc"},{"abcd", "bca", "abe"}, 3),
%!        logical ([1, 1, 0]))
%!assert (strncmp ("abc", {"abcd", 10}, 2), logical ([1, 0]))

%!assert <*54373> (strncmp ("abc", "abc", 100))

%!error strncmp ()
%!error strncmp ("abc", "def")
*/

DEFUNX ("strcmpi", Fstrcmpi, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} strcmpi (@var{str1}, @var{str2})
Return 1 if the character strings @var{str1} and @var{str2} are the same,
disregarding case of alphabetic characters, and 0 otherwise.

If either @var{str1} or @var{str2} is a cell array of strings, then an array
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

  return ovl (do_strcmp_fcn (args(0), args(1), 0, "strcmpi",
                             strcmpi_ignore_n, strcmpi_ignore_n));
}

/*
%!assert (strcmpi ("abc123", "ABC123"), true)
*/

DEFUNX ("strncmpi", Fstrncmpi, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} strncmpi (@var{str1}, @var{str2}, @var{n})
Return 1 if the first @var{n} character of @var{s1} and @var{s2} are the
same, disregarding case of alphabetic characters, and 0 otherwise.

If either @var{str1} or @var{str2} is a cell array of strings, then an array
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
    return ovl (do_strcmp_fcn (args(0), args(1), n, "strncmpi",
                               string::strncmpi,
                               string::strncmpi));
  else
    error ("strncmpi: N must be greater than 0");
}

/*
%!assert (strncmpi ("abc123", "ABC456", 3), true)

%!assert <*54373> (strncmpi ("abc", "abC", 100))
*/

DEFUN (str2double, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{d} =} str2double (@var{str})
Convert a string to a real or complex number.

The string must be in one of the following formats where a and b are real
numbers and the complex unit is @qcode{'i'} or @qcode{'j'}:

@itemize
@item a + bi

@item a + b*i

@item a + i*b

@item bi + a

@item b*i + a

@item i*b + a
@end itemize

If present, a and/or b are of the form @nospell{[+-]d[,.]d[[eE][+-]d]} where
the brackets indicate optional arguments and @qcode{'d'} indicates zero or
more digits.  The special input values @code{Inf}, @code{NaN}, and @code{NA}
are also accepted.

@var{str} may be a character string, character matrix, or cell array.  For
character arrays the conversion is repeated for every row, and a double or
complex array is returned.  Empty rows in @var{s} are deleted and not
returned in the numeric array.  For cell arrays each character string
element is processed and a double or complex array of the same dimensions as
@var{str} is returned.

For unconvertible scalar or character string input @code{str2double} returns
a NaN@.  Similarly, for character array input @code{str2double} returns a
NaN for any row of @var{s} that could not be converted.  For a cell array,
@code{str2double} returns a NaN for any element of @var{s} for which
conversion fails.  Note that numeric elements in a mixed string/numeric
cell array are not strings and the conversion will fail for these elements
and return NaN.

Programming Note: @code{str2double} can replace @code{str2num}, is more
efficient, and avoids the security risk of using @code{eval} on unknown data.
@seealso{str2num}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval;

  if (args(0).is_string ())
    {
      if (args(0).rows () == 0 || args(0).columns () == 0)
        retval = Matrix (1, 1, numeric_limits<double>::NaN ());
      else if (args(0).rows () == 1 && args(0).ndims () == 2)
        retval = string::str2double (args(0).string_value ());
      else
        {
          const string_vector sv = args(0).string_vector_value ();

          retval = sv.map<Complex> (string::str2double);
        }
    }
  else if (args(0).iscell ())
    {
      const Cell cell = args(0).cell_value ();

      ComplexNDArray output (cell.dims (), numeric_limits<double>::NaN ());

      for (octave_idx_type i = 0; i < cell.numel (); i++)
        {
          if (cell(i).is_string ())
            output(i) = string::str2double (cell(i).string_value ());
        }
      retval = output;
    }
  else
    retval = Matrix (1, 1, numeric_limits<double>::NaN ());

  return retval;
}

/*
%!assert (str2double ("1"), 1)
%!assert (str2double ("-.1e-5"), -1e-6)
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")
%! assert (str2double (char ("1", "2 3", "4i")), [1; NaN; 4i]);
%!testif HAVE_LLVM_LIBCXX  <47413>
%! ## Same test code as above, intended only for test statistics with libc++.
%! assert (str2double (char ("1", "2 3", "4i")), [1; NaN; 4i]);
%!assert (str2double ("1,222.5"), 1222.5)
%!assert (str2double ("i"), i)
%!assert (str2double ("2j"), 2i)
%!assert (str2double ("2 + j"), 2+j)
%!assert (str2double ("i*2 + 3"), 3+2i)
%!assert (str2double (".5*i + 3.5"), 3.5+0.5i)
%!assert (str2double ("1e-3 + i*.25"), 1e-3 + 0.25i)
%!assert (str2double (char ("2 + j","1.25e-3","-05")), [2+i; 1.25e-3; -5])
%!assert (str2double ({"2 + j","1.25e-3","-05"}), [2+i, 1.25e-3, -5])
%!assert (str2double (1), NaN)
%!assert (str2double ("1 2 3 4"), NaN)
%!assert (str2double ("Hello World"), NaN)
%!assert (str2double ("NaN"), NaN)
%!assert (str2double ("NA"), NA)
%!assert (str2double ("Inf"), Inf)
%!assert (str2double ("iNF"), Inf)
%!assert (str2double ("-Inf"), -Inf)
%!assert (str2double ("Inf*i"), complex (0, Inf))
%!assert (str2double ("iNF*i"), complex (0, Inf))
%!assert (str2double ("NaN + Inf*i"), complex (NaN, Inf))
%!assert (str2double ("Inf - Inf*i"), complex (Inf, -Inf))
%!assert (str2double ("-i*NaN - Inf"), complex (-Inf, -NaN))
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")
%! assert (str2double ({"abc", "4i"}), [NaN + 0i, 4i]);
%!testif HAVE_LLVM_LIBCXX  <47413>
%! assert (str2double ({"abc", "4i"}), [NaN + 0i, 4i]);
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")
%! assert (str2double ({2, "4i"}), [NaN + 0i, 4i])
%!testif HAVE_LLVM_LIBCXX  <47413>
%! assert (str2double ({2, "4i"}), [NaN + 0i, 4i])
%!assert (str2double (zeros (3,1,2)), NaN)
%!assert (str2double (''), NaN)
%!assert (str2double ([]), NaN)
%!assert (str2double (char (zeros (3,0))), NaN)
*/

DEFUN (__native2unicode__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{utf8_str} =} __native2unicode__ (@var{native_bytes}, @var{codepage})
Convert byte stream @var{native_bytes} to UTF-8 using @var{codepage}.

@seealso{native2unicode, __unicode2native__}
@end deftypefn */)
{
  if (args(0).is_string ())
    return ovl (args(0));

  std::string tmp = args(1).string_value ();
  const char *codepage
    = (tmp.empty () ? octave_locale_charset_wrapper () : tmp.c_str ());

  charNDArray native_bytes = args(0).char_array_value ();

  const char *src = native_bytes.data ();
  std::size_t srclen = native_bytes.numel ();

  std::size_t length;
  uint8_t *utf8_str = nullptr;

  utf8_str = octave_u8_conv_from_encoding (codepage, src, srclen, &length);

  if (! utf8_str)
    {
      if (errno == ENOSYS)
        error ("native2unicode: iconv() is not supported.  Installing GNU "
               "libiconv and then re-compiling Octave could fix this.");
      else
        error ("native2unicode: converting from codepage '%s' to UTF-8: %s",
               codepage, std::strerror (errno));
    }

  unwind_action free_utf8_str ([=] () { ::free (utf8_str); });

  octave_idx_type len = length;

  charNDArray retval (dim_vector (1, len));

  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem (i) = utf8_str[i];

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
  std::string tmp = args(1).string_value ();
  const char *codepage
    = (tmp.empty () ? octave_locale_charset_wrapper () : tmp.c_str ());

  charNDArray utf8_str = args(0).char_array_value ();

  const uint8_t *src = reinterpret_cast<const uint8_t *> (utf8_str.data ());
  std::size_t srclen = utf8_str.numel ();

  std::size_t length;
  char *native_bytes = nullptr;

  native_bytes = octave_u8_conv_to_encoding (codepage, src, srclen, &length);

  if (! native_bytes)
    {
      if (errno == ENOSYS)
        error ("unicode2native: iconv() is not supported.  Installing GNU "
               "libiconv and then re-compiling Octave could fix this.");
      else
        error ("unicode2native: converting from UTF-8 to codepage '%s': %s",
               codepage, std::strerror (errno));
    }

  unwind_action free_native_bytes ([=] () { ::free (native_bytes); });

  octave_idx_type len = length;

  uint8NDArray retval (dim_vector (1, len));

  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem (i) = native_bytes[i];

  return ovl (retval);
}

DEFUN (__locale_charset__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{charset} =} __locale_charset__ ()
Return the identifier for the charset used if the encoding is set to
@qcode{"locale"}.
@end deftypefn */)
{
  const char *charset = octave_locale_charset_wrapper ();
  std::string charset_str (charset);
  return ovl (charset_str);
}

DEFUN (unicode_idx, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{idx} =} unicode_idx (@var{str})
Return an array with the indices for each UTF-8 encoded character in @var{str}.

@example
@group
unicode_idx ("aäbc")
     @result{} [1, 2, 2, 3, 4]
@end group
@end example

@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  charNDArray str = args(0).xchar_array_value ("STR must be a string");
  Array<octave_idx_type> p (dim_vector (str.ndims (), 1));
  charNDArray str_p;
  if (str.ndims () > 1)
    {
      for (octave_idx_type i=0; i < str.ndims (); i++)
        p(i) = i;
      p(0) = 1;
      p(1) = 0;
      str_p = str.permute (p);
    }

  const uint8_t *src = reinterpret_cast<const uint8_t *> (str_p.data ());
  octave_idx_type srclen = str.numel ();

  NDArray idx (str_p.dims ());

  octave_idx_type u8_char_num = 1;
  for (octave_idx_type i = 0; i < srclen; u8_char_num++)
    {
      int mblen = octave_u8_strmblen_wrapper (src + i);
      if (mblen < 1)
        mblen = 1;
      for (octave_idx_type j = 0; j < mblen; j++)
        idx(i+j) = u8_char_num;
      i += mblen;
    }

  return ovl (str.ndims () > 1 ? idx.permute (p, true) : idx);
}

/*
%!assert (unicode_idx (["aäou"; "Ä∞"]), [1 2 2 3 4; 5 5 6 6 6])
*/

DEFUN (__unicode_length__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{len} =} __unicode_length__ (@var{str})
Return number of Unicode code points in @var{str}.

The input @var{str} must be a UTF-8 encoded character vector or cell string.

@example
@group
length ("aäbc")
     @result{} 5
__unicode_length__ ("aäbc")
     @result{} 4
@end group
@end example

@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  bool arg_char = args(0).is_char_matrix ();

  if (! arg_char && ! args(0).iscellstr ())
    error ("STR must be a character array or cell string.");

  octave_value_list retval;

  if (arg_char)
    {
      charNDArray str = args(0).char_array_value ();
      Array<octave_idx_type> p (dim_vector (str.ndims (), 1));
      if (str.ndims () > 1)
        {
          for (octave_idx_type i=0; i < str.ndims (); i++)
            p(i) = i;
          p(0) = 1;
          p(1) = 0;
          str = str.permute (p);
        }

      const uint8_t *src = reinterpret_cast<const uint8_t *> (str.data ());
      octave_idx_type mbsnlen = octave_u8_mbsnlen_wrapper (src, str.numel ());

      retval = ovl (mbsnlen);
    }
  else
    {
      const Array<std::string> cellstr = args(0).cellstr_value ();
      NDArray output (args(0).dims (), false);
      for (octave_idx_type i = 0; i < cellstr.numel (); i++)
        {
          const uint8_t *src
            = reinterpret_cast<const uint8_t *> (cellstr(i).c_str ());
          output(i) = octave_u8_mbsnlen_wrapper (src, cellstr(i).size ());
        }

      retval = ovl (output);
    }

  return retval;
}

/*
%!assert (__unicode_length__ (""), 0)
%!assert (__unicode_length__ ("aäbc"), 4)
%!assert (__unicode_length__ (["aä"; "öo"]), 4)
%!assert (__unicode_length__ ({"aäbc", "abc"}), [4, 3])
*/

DEFUN (__u8_validate__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{out_str} =} __u8_validate__ (in_str, mode)
Return string with valid UTF-8.

On encountering invalid UTF-8 in @var{in_str}, the bytes are either replaced by
the replacement character @qcode{"�"} (if @var{mode} is omitted or is the
string @qcode{"replace"}) or interpreted as the Unicode code points
U+0080–U+00FF with the same value as the byte (if @var{mode} is the string
@qcode{"unicode"}), thus interpreting the bytes according to ISO-8859-1.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  // Input check
  std::string in_str =
    args(0).xstring_value ("__u8_validate__: IN_STR must be a string");

  std::string mode = "replace";
  if (nargin == 2)
    mode = args(1).xstring_value ("__u8_validate__: MODE must be a string");

  string::u8_fallback_type fb_type;
  if (mode == "replace")
    fb_type = string::U8_REPLACEMENT_CHAR;
  else if (mode == "unicode")
    fb_type = string::U8_ISO_8859_1;
  else
    error (R"(__u8_validate__: MODE must be either "replace" or "unicode")");

  string::u8_validate ("__u8_validate__", in_str, fb_type);

  return ovl (in_str);
}

DEFUN (newline, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{c} =} newline
Return the character corresponding to a newline.

This is equivalent to @qcode{"@backslashchar{}n"}.

Example Code

@example
@group
joined_string = [newline "line1" newline "line2"]
@result{}
line1
line2
@end group
@end example

@seealso{strcat, strjoin, strsplit}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  static octave_value_list retval = ovl ("\n");

  return retval;
}

/*
%!assert (newline (), "\n")

%!error newline (1)
## FIXME: The next error() test requires a semicolon at EOL until
##        bug #59265 is resolved.
%!error [a, b] = newline ();
*/

DEFUN (list_in_columns, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{str} =} list_in_columns (@var{arg}, @var{width}, @var{prefix})
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

  string_vector s = args(
                      0).xstring_vector_value ("list_in_columns: ARG must be a cellstr or char array");

  int width = -1;

  if (nargin > 1 && ! args(1).isempty ())
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
%! input  = char ("abc", "def", "ghijkl", "mnop", "qrs", "tuv");
%! result = "abc     mnop  \ndef     qrs   \nghijkl  tuv   \n";
%! assert (list_in_columns (input, 20), result);
%!test
%! input  = char ("abc", "def", "ghijkl", "mnop", "qrs", "tuv");
%! result = "  abc     mnop  \n  def     qrs   \n  ghijkl  tuv   \n";
%! assert (list_in_columns (input, 20, "  "), result);

%!error list_in_columns ()
%!error list_in_columns (["abc", "def"], 20, 2)
%!error list_in_columns (["abc", "def"], 20, "  ", 3)
%!error <list_in_columns: WIDTH must be an integer> list_in_columns (["abc", "def"], "a")
*/

OCTAVE_END_NAMESPACE(octave)
