/*

Copyright (C) 2007, 2009 David Bateman


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
#include <vector>

#include "defun-dld.h"
#include "file-stat.h"
#include "file-ops.h"
#include "gripes.h"
#include "load-path.h"
#include "oct-env.h"
#include "oct-md5.h"

DEFUN_DLD (md5sum, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} md5sum (@var{file})\n\
@deftypefnx {Loadable Function} {} md5sum (@var{str}, @var{opt})\n\
Calculates the MD5 sum of the file @var{file}.  If the second parameter\n\
@var{opt} exists and is true, then calculate the MD5 sum of the\n\
string @var{str}.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1 && nargin != 2)
    print_usage();
  else
    {
      bool have_str = false;
      std::string str = args(0).string_value();

      if (nargin == 2)
	have_str = args(1).bool_value();
	
      if (!error_state)
	{
	  if (have_str)
	    retval = oct_md5 (str);
	  else
	    {
	      file_stat fs (str);

	      if (! fs.exists ())
		{
		  std::string tmp = octave_env::make_absolute
		    (load_path::find_file (str), octave_env::getcwd ());

		  if (! tmp.empty ())
		    {
		      warning_with_id ("Octave:md5sum-file-in-path",
				       "md5sum: file found in load path");
		      str = tmp;
		    }
		}

	      retval = oct_md5_file (str);
	    }
	}
    }

  return retval;
}
