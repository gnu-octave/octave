########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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
## @deftypefn {} {@var{mem_fcn_handle} =} memoize (@var{fcn_handle})
##
## Create a memoized version @var{mem_fcn_handle} of function @var{fcn_handle}.
##
## Each call to the memoized version @var{mem_fcn_handle} checks the inputs
## against an internally maintained table, and if the inputs have occurred
## previously, then the result of the function call is returned from the table
## itself instead of evaluating the full function again.  This speeds up the
## execution of functions that are called with the same inputs multiple times.
##
## For example, here we take a slow user-written function named @code{slow_fcn}
## and memoize it to a new handle @code{cyc}.  The first executions of both
## versions take the same time, but the subsequent executions of the memoized
## version returns the previously computed value, thus reducing 2.4 seconds of
## runtime to only 2.4 milliseconds.  The final check verifies that the same
## result was returned from both versions.
##
## @example
## @group
## >> tic; @var{p} = slow_fcn (5040); toc
## Elapsed time is 2.41244 seconds.
## >> tic; @var{p} = slow_fcn (5040); toc
## Elapsed time is 2.41542 seconds.
##
## >> cyc = memoize (@@slow_fcn);
## >> tic; @var{r} = cyc (5040); toc
## Elapsed time is 2.42609 seconds.
## >> tic; @var{r} = cyc (5040); toc
## Elapsed time is 0.00236511 seconds.
##
## >> all (@var{p} == @var{r})
## ans = 1
## @end group
## @end example
##
## @seealso{clearAllMemoizedCaches}
## @end deftypefn

function mem_fcn_handle = memoize (fcn_handle)

  if (nargin != 1)
    print_usage ();
  endif

  if (! is_function_handle (fcn_handle))
    error ("memoize: FCN_HANDLE must be a function handle");
  endif

  mem_fcn_handle = __memoize__ (fcn_handle);

endfunction


%!test
%! fcn1 = memoize (@sin);
%! assert (isa (fcn1, "matlab.lang.MemoizedFunction"));
%! fcn1 (pi);
%! fcn2 = memoize (@sin);
%! fcn2 (2*pi);
%! assert (isequal (fcn1, fcn2));

%!test
%! fcn = memoize (@rand);
%! a = fcn ();
%! b = fcn ();
%! assert (a, b);
%! fcn2 = memoize (@rand);
%! c = fcn2 ();
%! assert (a, c);

%!test
%! fcn = memoize (@plus);
%! fcn.stats;
%! stats (fcn);
%! clearCache (fcn);
%! fcn.clearCache;

## Test input validation
%!error <Invalid call> memoize ();
%!error <FCN_HANDLE must be a function handle> memoize (1);
