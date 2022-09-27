########################################################################
##
## Copyright (C) 2021 The Octave Project Developers
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
## @deftypefn  {} {@var{mem_fcn_handle} =} memoize (@var{fcn_handle})
##
## @seealso{clearAllMemoizedCaches}
## @end deftypefn

function mem_fcn_handle = memoize (fcn_handle)

  if (nargin != 1 || nargout > 1)
    print_usage ();
  endif
  if (! isa (fcn_handle, "function_handle"))
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
%! assert (isequal (fcn1, fcn2))

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

# Test input validation
%!error memoize ();
%!error memoize (1, 2);
%!error [a, b] = memoize (1);
%!error memoize (1);
