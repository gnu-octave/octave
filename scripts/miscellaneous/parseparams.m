## Copyright (C) 2006 Alexander Barth
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} [@var{reg}, @var{prop}] = parseparams (@var{params})
## Return in @var{reg} the cell elements of @var{param} up to the first
## string element and in @var{prop} all remaining elements beginning
## with the first string element.  For example
##
## @example
## [reg, prop] = parseparams (@{1, 2, "linewidth", 10@})
## reg =
## {
##   [1,1] = 1
##   [1,2] = 2
## }
## prop =
## {
##   [1,1] = linewidth
##   [1,2] = 10
## }
## @end example
##
## The parseparams function may be used to separate "regular"
## arguments and additional arguments given as property/value pairs of
## the @var{varargin} cell array.
## @seealso{varargin}
## @end deftypefn

## Author: Alexander Barth <abarth93@users.sourceforge.net>
## Author: Aida Alvera Azcarate <aida@netecho.info>

function [reg, prop] = parseparams (params)

  i = 1;

  while (i <= numel (params))
    if (ischar (params{i}))
      break;
    endif
    i++;
  endwhile

  reg = params(1:i-1);
  prop = params(i:end);

endfunction
