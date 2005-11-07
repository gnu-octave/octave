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

#include "Cell.h"
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

DEFUN (strcmp, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} {} strcmp (@var{s1}, @var{s2})\n\
Return 1 if the character strings @var{s1} and @var{s2} are the same,\n\
and 0 otherwise.\n\
@strong{Caution:}  For compatibility with @sc{Matlab}, Octave's strcmp\n\
function returns 1 if the strings are equal, and 0 otherwise.  This is\n\
just the opposite of the corresponding C library function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      bool s1_string = args(0).is_string ();
      bool s1_cell = args(0).is_cell ();
      bool s2_string = args(1).is_string ();
      bool s2_cell = args(1).is_cell ();

      if (s1_string && s2_string)
	{
	  // Must match exactly in all dimensions.

	  const dim_vector dv1 = args(0).dims ();
	  const dim_vector dv2 = args(1).dims ();

	  if (dv1.length () == dv2.length ())
	    {
	      for (int i = 0; i < dv1.length (); i++)
		{
		  if (dv1(i) != dv2(i))
		    {
		      retval = false;
		      return retval;
		    }
		}

	      if (dv1(0) == 0)
		retval = true;
	      else
		{
		  charNDArray s1 = args(0).char_array_value ();
		  charNDArray s2 = args(1).char_array_value ();

		  for (int i = 0; i < dv1.numel (); i++)
		    {
		      if (s1(i) != s2(i))
			{
			  retval = false;
			  return retval;
			}
		    }

		  retval = true;
		}
	    }
	}
      else if ((s1_string && s2_cell) || (s1_cell && s2_string))
	{
	  string_vector str;
	  Cell cell;
	  int r;

	  if (s1_string)
	    {
	      str = args(0).all_strings ();
	      r = args(0).rows ();
	      cell = args(1).cell_value ();
	    }
	  else
	    {
	      str = args(1).all_strings ();
	      r = args(1).rows ();
	      cell = args(0).cell_value ();
	    }

	  if (r == 1)
	    {
	      // Broadcast the string.

	      boolNDArray output (cell.dimensions);

	      for (int i = 0; i < cell.length (); i++)
		if (cell(i).is_string ())
		  output(i) = (cell(i).string_value () == str[0]);
		else
		  output(i) = false;

	      retval = output;
	    }
	  else if (r > 1)
	    {
	      if (cell.length () == 1)
		{
		  // Broadcast the cell.

		  const dim_vector dv (r, 1);
		  boolNDArray output (dv);

		  if (cell(0).is_string ())
		    {
		      const std::string str2 = cell(0).string_value ();

		      for (int i = 0; i < r; i++)
			output(i) = (str[i] == str2);
		    }
		  else
		    {
		      for (int i = 0; i < r; i++)
			output(i) = false;
		    }

		  retval = output;
		}
	      else
		{
		  // Must match in all dimensions.

		  boolNDArray output (cell.dimensions);

		  if (cell.length () == r)
		    {
		      for (int i = 0; i < r; i++)
			if (cell(i).is_string ())
			  output(i) = (str[i] == cell(i).string_value ());
			else
			  output(i) = false;

		      retval = output;
		    }
		  else
		    retval = false;
		}
	    }
	}
      else if (s1_cell && s2_cell)
	{
	  Cell cell1;
	  Cell cell2;

	  int r1 = args(0).numel ();
	  int r2;

	  if (r1 == 1)
	    {
	      // Make the singleton cell2.

	      cell1 = args(1).cell_value ();
	      cell2 = args(0).cell_value ();
	      r1 = cell1.length ();
	      r2 = 1;
	    }
	  else
	    {
	      cell1 = args(0).cell_value ();
	      cell2 = args(1).cell_value ();
	      r2 = cell2.length ();
	    }

	  const dim_vector size1 = cell1.dimensions;
	  const dim_vector size2 = cell2.dimensions;

	  boolNDArray output (size1);

	  if (r2 == 1)
	    {
	      // Broadcast cell2.

	      if (! cell2(0).is_string ())
		{
		  for (int i = 0; i < r1; i++)
		    output(i) = false;
		}
	      else
		{
		  const std::string str2 = cell2(0).string_value ();

		  for (int i = 0; i < r1; i++)
		    {
		      if (cell1(i).is_string ())
			{
			  const std::string str1 = cell1(i).string_value ();
			  output(i) = (str1 == str2);
			}
		      else
			output(i) = false;
		    }
		}
	    }
	  else
	    {
	      if (size1 != size2)
		{
		  error ("strcmp: nonconformant cell arrays");
		  return retval;
		}

	      for (int i = 0; i < r1; i++)
		{
		  if (cell1(i).is_string () && cell2(i).is_string ())
		    {
		      const std::string str1 = cell1(i).string_value ();
		      const std::string str2 = cell2(i).string_value ();
		      output(i) = (str1 == str2);
		    }
		  else
		    output(i) = false;
		}
	    }

	  retval = output;
	}
      else
	retval = false;
    }
  else
    print_usage ("strcmp");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
