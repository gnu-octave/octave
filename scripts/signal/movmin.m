## Copyright (C) 2018 Juan Pablo Carbajal
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program. If not, see <http://www.gnu.org/licenses/>.

## Author: Juan Pablo Carbajal <ajuanpi+dev@gmail.com>
## Created: 2018-08-13

## -*- texinfo -*-
## @defun {@var{y} =} movmin (@var{x}, @var{wlen})
## @defunx {@var{y} =} movmin (@dots{}, @var{dim})
## @defunx {@var{y} =} movmin (@dots{}, @var{nanstr})
## @defunx {@var{y} =} movmin (@dots{}, @var{property}, @var{value})
## Minimum of @var{x} over sliding window of length @var{wlen}.
##
## This is equivalent to
## @code{movfun (@@min, @var{x}, @var{wlen}, args@{:@}}
## where @code{args = @{'dim', @var{dim}, 'nancond', @var{nanstr}, @dots{}@}}
##
## Property @asis{'SamplePoints'} are not implemented yet
##
## @seealso{movfun}
## @end defun

function y = movmin (x, wlen, varargin)
  y = movfun (@min, x, wlen, parse_movargs(varargin{:}){:});
endfunction
