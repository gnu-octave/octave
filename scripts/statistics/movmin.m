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
## @deftypefn  {} {@var{y} =} movmin (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movmin (@var{x}, [@var{na}, @var{nb}])
## @deftypefnx {} {@var{y} =} movmin (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movmin (@dots{}, @var{nanstr})
## @deftypefnx {} {@var{y} =} movmin (@dots{}, @var{property}, @var{value})
## Minimum of @var{x} over a sliding window of length @var{wlen}.
##
## FIXME: Need explanation of all options.  Write once and then replicate.
##
## @seealso{movfun}
## @end deftypefn

function y = movmin (x, wlen, varargin)
  y = movfun (@min, x, wlen, __parse_movargs__ ("movmin", varargin{:}){:});
endfunction


## FIXME: Need BIST tests
