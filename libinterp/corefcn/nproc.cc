/*

Copyright (C) 2012-2018 Iain Murray

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "nproc-wrapper.h"

DEFUN (nproc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} nproc ()
@deftypefnx {} {} nproc (@var{query})
Return the current number of available processors.

If called with the optional argument @var{query}, modify how processors
are counted as follows:

@table @code
@item all
total number of processors.

@item current
processors available to the current process.

@item overridable
same as @code{current}, but overridable through the
@w{@env{OMP_NUM_THREADS}} environment variable.
@end table
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_nproc_query query = OCTAVE_NPROC_CURRENT;

  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      std::transform (arg.begin (), arg.end (), arg.begin (), tolower);

      if (arg == "all")
        query = OCTAVE_NPROC_ALL;
      else if (arg == "current")
        query = OCTAVE_NPROC_CURRENT;
      else if (arg == "overridable")
        query = OCTAVE_NPROC_CURRENT_OVERRIDABLE;
      else
        error ("nproc: invalid value for QUERY");
    }

  return ovl (octave_num_processors_wrapper (query));
}

/*
## Must always report at least 1 cpu available
%!assert (nproc () >= 1)
%!assert (nproc ("all") >= 1)
%!assert (nproc ("current") >= 1)

%!test
%! c = nproc ("current");
%! unwind_protect
%!   old_val = getenv ("OMP_NUM_THREADS");
%!   new_val = c + 1;
%!   setenv ("OMP_NUM_THREADS", num2str (new_val));
%!   assert (nproc ("overridable"), new_val);
%! unwind_protect_cleanup
%!   if (! isempty (old_val))
%!     setenv ("OMP_NUM_THREADS", old_val);
%!   else
%!     unsetenv ("OMP_NUM_THREADS");
%!   endif
%! end_unwind_protect

%!error nproc ("no_valid_option")
*/
