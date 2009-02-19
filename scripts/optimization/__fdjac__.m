## Copyright (C) 2008 Jaroslav Hajek <highegg@gmail.com>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn{Function File} {[@var{fval}, @var{fjac}]} =  __fdjac__ (@var{fcn}, @var{x}, @var{err})
## Undocumented internal function.
## @end deftypefn

function [fvec, fjac] = __fdjac__ (fcn, x, err = 0)
  err = sqrt (max (eps, err));
  fvec = fcn (x);
  fv = fvec(:);
  h = abs (x(:))*err;
  h(h == 0) = err;
  fjac = zeros (length (fv), numel (x));
  for i = 1:numel (x)
    x1 = x;
    x1(i) += h(i);
    fjac(:,i) = (fcn (x1)(:) - fv) / h(i);
  endfor
endfunction



