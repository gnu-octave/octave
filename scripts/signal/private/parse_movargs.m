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
## @defun {@var{y} =} parse_movargs (@var{x})
## Parse arguments in compatibility with other software.
##
## Undocumented private function
## @end defun

function args = parse_movargs (varargin)
  if (numel (varargin) == 1) # scalar was given --> dim or nancond
    if (ischar (varargin{1})) # --> nancond
      args = {'nancond', varargin{1}};
    else # --> dim
      args = {'dim', varargin{1}};
    endif
  else # Name,Value pairs
    args = varargin;
  endif
endfunction
