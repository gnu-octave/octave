########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{arg} =} nthargout (@var{n}, @var{fcn}, @dots{})
## @deftypefnx {} {@var{arg} =} nthargout (@var{n}, @var{ntot}, @var{fcn}, @dots{})
## Return the @var{n}th output argument of the function specified by the
## function handle or string @var{fcn}.
##
## Any additional arguments are passed directly to @var{fcn}.  The total
## number of arguments to call @var{fcn} with can be passed in @var{ntot}; by
## default @var{ntot} is @var{n}.  The input @var{n} can also be a vector of
## indices of the output, in which case the output will be a cell array of the
## requested output arguments.
##
## The intended use of @code{nthargout} is to avoid intermediate variables.
## For example, when finding the indices of the maximum entry of a matrix, the
## following two compositions of @code{nthargout}
##
## @example
## @group
## @var{m} = magic (5);
## cell2mat (nthargout ([1, 2], @@ind2sub, size (@var{m}),
##                      nthargout (2, @@max, @var{m}(:))))
## @result{} 5   3
## @end group
## @end example
##
## @noindent
## are completely equivalent to the following lines:
##
## @example
## @group
## @var{m} = magic (5);
## [~, idx] = max (@var{M}(:));
## [i, j] = ind2sub (size (@var{m}), idx);
## [i, j]
## @result{} 5   3
## @end group
## @end example
##
## It can also be helpful to have all output arguments collected in a single
## cell array as the following code demonstrates:
##
## @example
## @var{USV} = nthargout ([1:3], @@svd, hilb (5));
## @end example
##
## @seealso{nargin, nargout, varargin, varargout, isargout}
## @end deftypefn

function arg = nthargout (n, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (is_function_handle (varargin{1}) || ischar (varargin{1}))
    ntot = max (n(:));
    fcn = varargin{1};
    args = varargin(2:end);
  elseif (isnumeric (varargin{1})
          && (is_function_handle (varargin{2}) || ischar (varargin{2})))
    ntot = varargin{1};
    fcn = varargin{2};
    args = varargin(3:end);
  else
    print_usage ();
  endif

  if (any (n != fix (n))  || ntot != fix (ntot) || any (n <= 0) || ntot <= 0)
    error ("nthargout: N and NTOT must consist of positive integers");
  endif

  outargs = cell (1, ntot);

  try
    [outargs{:}] = feval (fcn, args{:});
    if (numel (n) > 1)
      arg = outargs(n);
    else
      arg = outargs{n};
    endif
  catch
    err = lasterr ();
    if (strfind ("some elements undefined in return list", err))
      error ("nthargout: Too many output arguments: %d", ntot);
    else
      error (err);
    endif
  end_try_catch

endfunction


%!shared m
%! m = magic (5);
%!assert (nthargout ([1,2], @ind2sub, size (m), nthargout (2, @max, m(:))),
%!        {5,3})
%!assert (nthargout (3, @find, m(m>20)), [23, 24, 25, 21, 22]')
