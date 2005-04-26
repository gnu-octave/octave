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

#include "LPsolve.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

DEFUN_DLD (lpsolve, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} lpsolve ()\n\
Not implemented yet...\n\
@end deftypefn")
{
  octave_value_list retval;

  // Force a bad value of inform, and empty matrices for x and phi.

  Matrix m;
  retval(2) = -1.0;
  retval(1) = m;
  retval(0) = m;

  error ("lpsolve: not implemented yet");

  return retval;
}

DEFUN_DLD (lpsolve_options, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} lpsolve_options ()\n\
Not implemented yet...\n\
@end deftypefn")
{
  octave_value_list retval;

  error ("lpsolve_options: not implemented yet");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
