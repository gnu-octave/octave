/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>

#include "dMatrix.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"

DEFUN (char, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} char (@var{x})\n\
@deftypefnx {Built-in Function} {} char (@var{cell_array})\n\
@deftypefnx {Built-in Function} {} char (@var{s1}, @var{s2}, @dots{})\n\
Create a string array from a numeric matrix, cell array, or list of\n\
\n\
If the argument is a numeric matrix, each element of the matrix is\n\
converted to the corresponding ASCII character.  For example,\n\
\n\
@example\n\
@group\n\
char ([97, 98, 99])\n\
     @result{} \"abc\"\n\
@end group\n\
@end example\n\
\n\
If the argument is a cell array of strings, the result is a string array\n\
with each element corresponding to one element of the cell array.\n\
\n\
For multiple string arguments, the result is a string array with each\n\
element corresponding to the arguments.\n\
\n\
The returned values are padded with blanks as needed to make each row\n\
of the string array have the same length.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    retval = args(0).convert_to_str (true, true,
				     args(0).is_dq_string () ? '"' : '\'');
  else if (nargin > 1)
    {
      int n_elts = 0;

      int max_len = 0;

      for (int i = 0; i < nargin; i++)
	{
	  string_vector s = args(i).all_strings (false, true);

	  if (error_state)
	    {
	      error ("char: unable to convert some args to strings");
	      return retval;
	    }

	  n_elts += s.length ();

	  int s_max_len = s.max_length ();

	  if (s_max_len > max_len)
	    max_len = s_max_len;
	}

      string_vector result (n_elts);

      int k = 0;

      for (int i = 0; i < nargin; i++)
	{
	  string_vector s = args(i).all_strings (false, true);

	  int n = s.length ();

	  for (int j = 0; j < n; j++)
	    {
	      std::string t = s[j];
	      int t_len = t.length ();

	      if (max_len > t_len)
		t += std::string (max_len - t_len, ' ');

	      result[k++] = t;
	    }
	}

      retval = octave_value (result, '\'');
    }
  else
    print_usage ("char");

  return retval;
}

DEFUN (ischar, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ischar (@var{a})\n\
Return 1 if @var{a} is a string.  Otherwise, return 0.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).is_string ();
  else
    print_usage ("ischar");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
