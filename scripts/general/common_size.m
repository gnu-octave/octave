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
## @deftypefn {Function File} {[@var{err}, @var{y1}, ...] =} common_size (@var{x1}, ...)
## Determine if all input arguments are either scalar or of common
## size.  If so, @var{err} is zero, and @var{yi} is a matrix of the
## common size with all entries equal to @var{xi} if this is a scalar or
## @var{xi} otherwise.  If the inputs cannot be brought to a common size,
## errorcode is 1, and @var{yi} is @var{xi}.  For example,
##
## @example
## @group
## [errorcode, a, b] = common_size ([1 2; 3 4], 5)
##      @result{} errorcode = 0
##      @result{} a = [ 1, 2; 3, 4 ]
##      @result{} b = [ 5, 5; 5, 5 ]
## @end group
## @end example
##
## @noindent
## This is useful for implementing functions where arguments can either
## be scalars or of common size.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 15 October 1994
## Adapted-By: jwe

function [errorcode, ...] = common_size (...)

  if (nargin < 2)
    error ("common_size: only makes sense if nargin >= 2");
  endif

  va_start ();
  for k = 1 : nargin
    s(k, :) = size (va_arg ());
  endfor

  m = max (s);
  if (any (any ((s != 1)') & any ((s != ones (nargin, 1) * m)')))
    errorcode = 1;
    va_start ();
    for k = 1 : nargin
      vr_val (va_arg ());
    endfor
  else
    errorcode = 0;
    va_start ();
    for k = 1 : nargin
      if (prod (s(k, :)) == 1)
        vr_val (va_arg () * ones (m));
      else
        vr_val (va_arg ());
      endif
    endfor
  endif

endfunction
