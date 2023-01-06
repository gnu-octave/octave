########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{err}, @var{yi}, @dots{}] =} common_size (@var{xi}, @dots{})
## Determine if all input arguments are either scalar or of common size.
##
## If true, @var{err} is zero, and @var{yi} is a matrix of the common size
## with all entries equal to @var{xi} if this is a scalar or @var{xi}
## otherwise.  If the inputs cannot be brought to a common size, @var{err} is
## 1, and @var{yi} is @var{xi}.  For example:
##
## @example
## @group
## [err, a, b] = common_size ([1 2; 3 4], 5)
##      @result{} err = 0
##      @result{} a = [ 1, 2; 3, 4 ]
##      @result{} b = [ 5, 5; 5, 5 ]
## @end group
## @end example
##
## @noindent
## This is useful for implementing functions where arguments can either be
## scalars or of common size.
## @seealso{size, size_equal, numel, ndims}
## @end deftypefn

function [err, varargout] = common_size (varargin)

  if (nargin < 2)
    error ("common_size: only makes sense if nargin >= 2");
  endif

  ## Find array args
  array = cellfun ("numel", varargin) != 1;
  aridx = find (array, 1);

  if (isempty (aridx))
    ## All inputs are scalars
    err = 0;
    varargout = varargin;
  else
    sz_eq = cellfun ("size_equal", varargin, varargin(aridx));
    if (any (! sz_eq & array))
      err = 1;
      varargout = varargin;
    else
      err = 0;
      if (nargout > 1)
        varargout = varargin;
        if (any (array))
          scalar = ! array;
          dims = size (varargin{aridx});
          subs = arrayfun (@ones, 1, dims, "uniformoutput", false);
          varargout(scalar) = cellindexmat (varargin(scalar), subs{:});
        endif
      endif
    endif
  endif

endfunction


%!test
%! m = [1,2;3,4];
%! [err, a, b, c] = common_size (m, 3, 5);
%! assert (err, 0);
%! assert (a, m);
%! assert (b, [3,3;3,3]);
%! assert (c, [5,5;5,5]);

%!test
%! m = [1,2;3,4];
%! [err, a, b, c] = common_size (m, [], 5);
%! assert (err, 1);
%! assert (a, m);
%! assert (b, []);
%! assert (c, 5);

## Test input validation
%!error <only makes sense if nargin .= 2> common_size ()
%!error <only makes sense if nargin .= 2> common_size (1)
