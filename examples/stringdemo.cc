/*

Copyright (C) 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License 
as published by the Free Software Foundation; either
version 3  of the License, or (at your option) any later 
version.

Octave is distributed in the hope that it will be useful, 
but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public 
License along with Octave; see the file COPYING.  If not,
see <http://www.gnu.org/licenses/>.

*/

#include <octave/oct.h>

DEFUN_DLD (stringdemo, args, , "String Demo")
{
  int nargin = args.length();
  octave_value_list retval; 

  if (nargin != 1)
    print_usage ();
  else
    {
      charMatrix ch = args(0).char_matrix_value ();

      if (! error_state)
        {
          if (args(0).is_sq_string ())
            retval(1) = octave_value (ch, true);
          else
            retval(1) = octave_value (ch, true, '\'');

          octave_idx_type nr = ch.rows();
          for (octave_idx_type i = 0; i < nr / 2; i++)
            {
              std::string tmp = ch.row_as_string (i);
              ch.insert (ch.row_as_string(nr-i-1).c_str(), 
			 i, 0);
              ch.insert (tmp.c_str(), nr-i-1, 0);
            }
          retval(0) = octave_value (ch, true);
        }
    }
  return retval;
}
