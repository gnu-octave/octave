## Copyright (C) 1995, 1996  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} xor (@var{x}, @var{y})
## Return the `exclusive or' of the entries of @var{x} and @var{y}.
## For boolean expressions @var{x} and @var{y},
## @code{xor (@var{x}, @var{y})} is true if and only if @var{x} or @var{y}
## is true, but not if both @var{x} and @var{y} are true.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function z = xor (x, y)

  if (nargin == 2)
    if (is_scalar (x) || is_scalar (y) || size (x) == size (y))
      z = logical ((x | y) - (x & y));
    else
      error ("xor: x and y must be of common size or scalars");
    endif
  else
    usage ("xor (x, y)");
  endif

endfunction
