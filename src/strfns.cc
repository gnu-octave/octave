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

#include <cctype>

#include "dMatrix.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (isstr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isstr (@var{a})\n\
Return 1 if @var{a} is a string.  Otherwise, return 0.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = static_cast<double> (args(0).is_string ());
  else
    print_usage ("isstr");

  return retval;
}

DEFUN (setstr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} setstr (@var{x})\n\
Convert a matrix to a string.  Each element of the matrix is converted\n\
to the corresponding ASCII \n\
character.  For example,\n\
\n\
@example\n\
@group\n\
setstr ([97, 98, 99])\n\
     @result{} "abc"\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).convert_to_str ();
  else
    print_usage ("setstr");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
