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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iostream.h>

#include "oct-term.h"
#include "str-vec.h"

// Create a string vector from a NULL terminated list of C strings.

string_vector::string_vector (const char * const *s)
  : Array<string> ()
{
  int n = 0;

  while (*s++)
    n++;

  resize (n);

  for (int i = 0; i < n; i++)
    elem (i) = s[i];
}

// Create a string vector from up to N C strings.  Assumes that N is
// nonnegative.

string_vector::string_vector (const char * const *s, int n)
  : Array<string> (n)
{
  for (int i = 0; i < n; i++)
    elem (i) = s[i];
}

// Format a list in neat columns.  Mostly stolen from GNU ls.

ostream&
string_vector::list_in_columns (ostream& os) const
{
  // Compute the maximum name length.

  int max_name_length = 0;
  int total_names = length ();

  for (int i = 0; i < total_names; i++)
    {
      int name_length = elem (i).length ();
      if (name_length > max_name_length)
	max_name_length = name_length;
    }

  // Allow at least two spaces between names.

  max_name_length += 2;

  // Calculate the maximum number of columns that will fit.

  int line_length = terminal_columns ();
  int cols = line_length / max_name_length;
  if (cols == 0)
    cols = 1;

  // Calculate the number of rows that will be in each column except
  // possibly  for a short column on the right.

  int rows = total_names / cols + (total_names % cols != 0);

  // Recalculate columns based on rows.

  cols = total_names / rows + (total_names % rows != 0);

  int count;
  for (int row = 0; row < rows; row++)
    {
      count = row;
      int pos = 0;

      // Print the next row.

      while (1)
	{
	  string nm = elem (count);

	  os << nm;
	  int name_length = nm.length ();

	  count += rows;
	  if (count >= total_names)
	    break;

	  int spaces_to_pad = max_name_length - name_length;
	  for (int i = 0; i < spaces_to_pad; i++)
	    os << " ";
	  pos += max_name_length;
	}
      os << "\n";
    }

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
