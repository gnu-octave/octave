## Copyright (C) 2018 Juan Pablo Carbajal
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

## Author: Juan Pablo Carbajal <ajuanpi+dev@gmail.com>
## Created: 2018-08-13

## -*- texinfo -*-
## @deftypefn {} {@var{args} =} __parse_movargs__ (@var{varargin})
##
## Parse arguments for movXXX functions before passing to @code{movfun}.
##
## @seealso{movfun}
## @end deftypefn

function args = __parse_movargs__ (varargin)

  if (isscalar (varargin))
    ## Either DIM or NANCOND
    if (ischar (varargin{1}))
      args = {"nancond", varargin{1}};
    else
      args = {"dim", varargin{1}};
    endif
  else
    ## Name,Value pairs
    args = varargin;
  endif

endfunction


## No tests needed for internal function
%!assert (1)
