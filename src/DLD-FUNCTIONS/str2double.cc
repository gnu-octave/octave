/*

Copyright (C) 2010 Jaroslav Hajek
Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <cctype>
#include <sstream>
#include <algorithm>

#include "lo-ieee.h"

#include "Cell.h"
#include "ov.h"
#include "defun-dld.h"
#include "gripes.h"
#include "utils.h"

static inline bool 
is_imag_unit (int c)
{ return c == 'i' || c == 'j'; }

static std::istringstream&
single_num (std::istringstream& is, double& num)
{
  char c = is.peek ();
  if (c == 'I')
    {
      // It's infinity.
      is.get ();
      char c1 = is.get (), c2 = is.get ();
      if (c1 == 'n' && c2 == 'f')
        {
          num = octave_Inf;
          is.peek (); // Sets eof bit.
        }
      else
        is.setstate (std::ios::failbit); // indicate that read has failed.
    }
  else if (c == 'N')
    {
      // It's NaN.
      is.get ();
      char c1 = is.get (), c2 = is.get ();
      if (c1 == 'a' && c2 == 'N')
        {
          num = octave_NaN;
          is.peek (); // Sets eof bit.
        }
      else
        is.setstate (std::ios::failbit); // indicate that read has failed.
    }
  else
    is >> num;

  return is;
}

static std::istringstream&
extract_num (std::istringstream& is, double& num, bool& imag, bool& have_sign)
{
  have_sign = imag = false;

  char c = is.peek ();
  bool negative = false;

  // Accept leading sign.
  if (c == '+' || c == '-')
    {
      negative = c == '-';
      is.get ();
      c = is.peek ();
      have_sign = true;
    }

  if (is_imag_unit (c))
    {
      c = is.get ();
      // It's i*num or just i.
      imag = true;
      char cn = is.peek ();
      if (cn == '*')
        {
          // Multiplier follows, we extract it as a number.
          is.get ();
          single_num (is, num);
        }
      else
        num = 1.0;
    }
  else
    {
      single_num (is, num);
      if (is.good ())
        {
          c = is.peek ();
          if (c == '*')
            {
              is.get ();
              c = is.get ();
              if (is_imag_unit (c))
                imag = true;
              else
                is.setstate (std::ios::failbit); // indicate that read has failed.
            }
          else if (is_imag_unit (c))
            imag = true;
        }
    }

  if (negative)
    num = -num;

  return is;
}

static inline void
set_component (Complex& c, double num, bool imag)
{
#if defined (HAVE_CXX_COMPLEX_SETTERS)
  if (imag)
    c.imag (num);
  else
    c.real (num);
#elif defined (HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS)
  if (imag)
    c.imag () = num;
  else
    c.real () = num;
#else
  if (imag)
    c = Complex (c.real (), num);
  else
    c = Complex (num, c.imag ());
#endif
}

static Complex
str2double1 (std::string str)
{
  Complex val (0.0, 0.0);
  std::string::iterator se = str.end ();
  // Remove commas (thousand separators) and spaces.
  se = std::remove (str.begin (), se, ',');
  se = std::remove (str.begin (), se, ' ');
  str.erase (se, str.end ());

  std::istringstream is (str);
  double num;
  bool i1, i2, s1, s2;

  if (is.eof ())
    val = octave_NaN;
  else if (! extract_num (is, num, i1, s1))
    val = octave_NaN;
  else
    {
      set_component (val, num, i1);

      if (! is.eof ())
        {
          if (! extract_num (is, num, i2, s2) || i1 == i2 || ! s2)
            val = octave_NaN;
          else
            set_component (val, num, i2);
        }
    }

  return val;
}

DEFUN_DLD (str2double, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} str2double (@var{s})\n\
Converts a string to real or complex number.\n\
A complex number should be in one of the formats:\n\
\n\
@itemize\n\
@item  a + bi\n\
@item  a + b*i\n\
@item  a + i*b\n\
@item  bi + a\n\
@item  b*i + a\n\
@item  i*b + a\n\
@end itemize\n\
\n\
It is also possible to use @code{j} instead of @code{i}, or write just\n\
@code{i} instead of @code{1*i}.\n\
@code{a} and @code{b} should be real numbers\n\
in a standard format.\n\
@var{s} can also be a character matrix, in which case the conversion is repeated\n\
for each row, or a cell array of strings, in which case each element is converted\n\
and an array of the same dimensions is returned.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      if (args(0).is_string ())
        {
          if (args(0).rows () == 1 && args(0).ndims () == 2)
            {
              retval = str2double1 (args(0).string_value ());
            }
          else
            {
              const string_vector sv = args(0).all_strings ();
              if (! error_state)
                retval = sv.map<Complex> (str2double1);
            }
        }
      else if (args(0).is_cellstr ())
        {
          Array<std::string> sa = args(0).cellstr_value ();
          retval = sa.map<Complex> (str2double1);
        }
      else
        gripe_wrong_type_arg ("str2double", args(0));
    }
  else
    print_usage ();

  return retval;
}

/*

%!assert (str2double ("1"), 1)
%!assert (str2double ("-.1e-5"), -1e-6)
%!assert (str2double ("1,222.5"), 1222.5)
%!assert (str2double ("i"), i)
%!assert (str2double ("2 + j"), 2+j)
%!assert (str2double ("i*2 + 3"), 3+2i)
%!assert (str2double (".5*i + 3.5"), 3.5+0.5i)
%!assert (str2double ("1e-3 + i*.25"), 1e-3 + i*.25)
%!assert (str2double (["2 + j";"1.25e-3";"-05"]), [2+i; 1.25e-3; -05])
%!assert (str2double ({"2 + j","1.25e-3","-05"}), [2+i, 1.25e-3, -05])
%!assert (str2double ("NaN"), NaN)
%!assert (str2double ("Inf"), Inf)
%!assert (str2double ("-Inf"), -Inf)
%!assert (str2double ("i*Inf"), complex (0, Inf))
%!assert (str2double ("NaN + Inf*i"), complex (NaN, Inf))
%!assert (str2double ("Inf - Inf*i"), complex (Inf, -Inf))
%!assert (str2double ("-i*NaN - Inf"), complex (-Inf, -NaN))

*/
