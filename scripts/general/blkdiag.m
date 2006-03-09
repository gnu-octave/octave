## Copyright (C) 2000 Daniel Calvelo
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} blkdiag (@var{a}, @var{b}, @var{c}, @dots{})
## Build a block diagonal matrix from @var{a}, @var{b}, @var{c}, @dots{}.
## All the arguments must be numeric and are two-dimensional matrices or
## scalars.
## @seealso{diag, horzcat, vertcat}
## @end deftypefn

## Author: Daniel Calvelo
## Modified by: William Poetra Yoga Hadisoeseno

function retval = blkdiag (varargin)

  if (nargin < 1)
    usage ("blkdiag (a, b, c, ...)");
  endif

  if (! all (cell2mat (cellfun (@isnumeric, varargin))))
    error ("blkdiag: all arguments must be numeric");
  endif

  ## Note: trailing singletons are automatically (correctly) ignored.
  if (! all (cellfun ("ndims", varargin) == 2))
    error ("blkdiag: all arguments must be two-dimensional matrices");
  endif

  ## size is an option for cellfun, but it's a bit different from
  ## calling size directly.
  csz = cumsum ([0 0; (cell2mat (cellfun (@size, varargin')))], 1);
  retval = zeros (csz(end,:));

  for p = 1:nargin
    retval((csz(p,1)+1):csz(p+1,1),(csz(p,2)+1):csz(p+1,2)) = varargin{p};
  endfor

endfunction

# regular tests
%!assert(blkdiag(1,ones(2),1),[1,0,0,0;0,1,1,0;0,1,1,0;0,0,0,1])
%!assert(blkdiag([1,2],[3,4],[5,6]),[1,2,0,0,0,0;0,0,3,4,0,0;0,0,0,0,5,6])
%!assert(blkdiag([1,2],[3;4],[5,6]),[1,2,0,0,0;0,0,3,0,0;0,0,4,0,0;0,0,0,5,6])
%!assert(blkdiag([1,2;3,4],[5,6,7]),[1,2,0,0,0;3,4,0,0,0;0,0,5,6,7])
# tests involving empty matrices
%!assert(blkdiag([],[],[]),[])
%!assert(blkdiag([],[1,2;3,4],[],5,[]),[1,2,0;3,4,0;0,0,5])
%!assert(blkdiag(zeros(1,0,1),[1,2,3],1,0,5,zeros(0,1,1)),[0,0,0,0,0,0,0;1,2,3,0,0,0,0;0,0,0,1,0,0,0;0,0,0,0,0,0,0;0,0,0,0,0,5,0]);
# sanity checks
%!test
%! A = rand (round (rand (1, 2) * 10));
%! assert (blkdiag (A), A);
