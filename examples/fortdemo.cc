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
#include <octave/f77-fcn.h>

extern "C" 
{
  F77_RET_T 
  F77_FUNC (fortsub, FORTSUB) 
        (const int&, double*, F77_CHAR_ARG_DECL  
         F77_CHAR_ARG_LEN_DECL);
}

DEFUN_DLD (fortdemo , args , , "Fortran Demo.")
{
  octave_value_list retval;  
  int nargin = args.length();
  if (nargin != 1)
    print_usage ();
  else
    {
      NDArray a = args(0).array_value ();
      if (! error_state)
        {
          double *av = a.fortran_vec ();
          octave_idx_type na = a.nelem ();
          OCTAVE_LOCAL_BUFFER (char, ctmp, 128);

          F77_XFCN (fortsub, FORTSUB, (na, av, ctmp 
                    F77_CHAR_ARG_LEN (128)));

          if (f77_exception_encountered)
            error ("fortdemo: error in fortran");
          else
            {
              retval(1) = std::string (ctmp);
              retval(0) = a;
            }
        }
    }
  return retval;
}
