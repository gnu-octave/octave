## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{num}, @var{den}] =} zp2tf (@var{zer}, @var{pol}, @var{k})
## Converts zeros / poles to a transfer function.
##
## @strong{Inputs}
## @table @var
## @item zer
## @itemx pol
## Vectors of (possibly complex) poles and zeros of a transfer
## function.  Complex values must appear in conjugate pairs.
## @item k
## Real scalar (leading coefficient).
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## (With help from students Ingram, McGowan.)

function [num, den] = zp2tf (zer, pol, k)

  ## Find out whether data was entered as a row or a column vector and
  ## convert to a column vector if necessary.

  [rp,cp] = size(pol);
  [rz,cz] = size(zer);

  if(!(isvector(zer) | isempty(zer)) )
    error(sprintf("zer(%dx%d) must be a vector",rz,cz));
  elseif(!(isvector(pol) | isempty(pol)) )
    error(sprintf("pol(%dx%d) must be a vector",rp,cp));
  elseif(length(zer) > length(pol))
    error(sprintf("zer(%dx%d) longer than pol(%dx%d)",rz,cz,rp,cp));
  endif

  ## initialize converted polynomials

  num = k;  den = 1;

  ## call __zp2ssg2__ if there are complex conjugate pairs left, otherwise
  ## construct real zeros one by one.  Repeat for poles.

  while(!isempty(zer))
    if( max(abs(imag(zer))) )     [poly, zer] = __zp2ssg2__ (zer);
    else                          poly = [1, -zer(1)];
                                  zer = zer(2:length(zer));      endif
    num = conv(num,poly);
  endwhile

  while(!isempty(pol))
    if( max(abs(imag(pol))) )     [poly, pol] = __zp2ssg2__ (pol);
    else                          poly = [1, -pol(1)];
                                  pol = pol(2:length(pol));      endif
    den = conv(den,poly);
  endwhile

endfunction
