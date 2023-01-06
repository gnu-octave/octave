########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{n} =} maxNumCompThreads ()
## @deftypefnx {} {@var{n_old} =} maxNumCompThreads (@var{n})
## @deftypefnx {} {@var{n_old} =} maxNumCompThreads ("automatic")
## This function is provided for @sc{matlab} compatibility only.
##
## The output @var{n} is the number of available processors as determined by
## the @code{nproc} function.
##
## Programming Note: The function may be called with an argument to set the
## number of computational threads, but that setting has @strong{no effect}.
## @seealso{nproc}
## @end deftypefn

function retval = maxNumCompThreads (arg)

  persistent nthreads = nproc ();

  retval = nthreads;

  if (nargin == 1)
    if (isnumeric (arg) && isscalar (arg) && arg == fix (arg)
        && arg > 0 && isfinite (arg))
      ## FIXME: Should there be an upper limit?
      nthreads = arg;
      warning ("Octave:maxNumCompThreads:no-effect",
               "maxNumCompThreads: setting number of threads has no effect");
    elseif (ischar (arg) && strcmpi (arg, "automatic"))
      nthreads = nproc ();
    else
      error ("maxNumCompThreads: invalid input argument");
    endif
  endif

endfunction


%!test
%! maxNumCompThreads ("automatic");
%! assert (maxNumCompThreads (), nproc ());

%!test
%! warning ("off", "Octave:maxNumCompThreads:no-effect", "local");
%! maxNumCompThreads (4);
%! assert (maxNumCompThreads ("automatic"), 4);

%!error <invalid input argument> maxNumCompThreads ([1, 2])
%!error <invalid input argument> maxNumCompThreads ("foobar")
