## Copyright (C) 2004 David Bateman & Andy Adler
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
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301  USA

## -*- texinfo -*-
## @deftypefn {Function File} {@var{y} =} sphcat (@var{a1}, @var{a2}, @dots{}, @var{aN})
## Return the horizontal concatenation of sparse matrices. This function
## is obselete and @code{horzcat} should be used
## @end deftypefn
## @seealso {spvcat, vertcat, horzcat, cat}

function y = sphcat (varargin)

  persistent sphcat_warned = false;

  if (!sphcat_warned)
    sphcat_warned = true;
    warning ("sphcat: This function is depreciated. Use horzcat instead");
  endif

  y = horzcat (varargin{:});
endfunction
