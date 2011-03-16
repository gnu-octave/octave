/*

Copyright (C) 2011 Iain Murray


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

#include "defun-dld.h"
#include "sys/sysinfo.h"

DEFUN_DLD (nprocs, args, nargout,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} nprocs ()\n\
Return the number of available processors.\n\
@seealso{nprocs_conf}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0 || (nargout != 0 && nargout != 1))
    {
      print_usage ();
      return retval;
    }

  retval = get_nprocs ();

  return retval;
}

DEFUN_DLD (nprocs_conf, args, nargout,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} nprocs_conf ()\n\
Return the number of number of processors the operating system has\n\
configured.  This number may be less than the total available as reported by\n\
@code{nprocs}.\n\
@seealso{nprocs}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0 || (nargout != 0 && nargout != 1))
    {
      print_usage ();
      return retval;
    }

  retval = get_nprocs_conf ();

  return retval;
}

