/*

Copyright (C) 2007 David Bateman


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

#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "defun-dld.h"
#include "oct-md5.h"

DEFUN_DLD (md5sum, args, nargout,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} md5sum (@var{str})\n\
Calculates the MD5 sum of the string @var{str}.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage();
  else
    {
      std::string str = args(0).string_value();

      if (!error_state)
	retval = oct_md5 (str);
    }

  return retval;
}
