## Copyright (C) 1996, 1998, 2000, 2001, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{poly}, @var{rvals}] =} __zp2ssg2__ (@var{rvals})
## Used internally in @code{zp2ss}
## Extract 2 values from @var{rvals} (if possible) and construct
## a polynomial with those roots.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1996

function [poly, rvals] = __zp2ssg2__ (rvals)

  ## locate imaginary roots (if any)
  cidx = find(imag(rvals));

  if(!isempty(cidx))
    ## select first complex root, omit from cidx
    r1i = cidx(1);      r1 = rvals(r1i);     cidx = complement(r1i,cidx);

    ## locate conjugate root (must be in cidx list, just in case there's
    ## roundoff)
    err = abs(rvals(cidx) - r1');
    minerr = min(err);
    c2i = find(err == minerr, 1);
    r2i = cidx(c2i);
    r2 = rvals(r2i);
    cidx = complement(r2i,cidx);

    ## don't check for divide by zero, since 0 is not complex.
    if(abs(r2 - r1')/abs(r1) > 1e-12)
      error(sprintf("r1=(%f,%f); r2=(%f,%f), not conjugates.", ...
        real(r1),imag(r1),real(r2),imag(r2)));
    endif

    ## complex conjugate pair
    poly = [1, -2*real(r1), real(r1)^2+imag(r1)^2];
  else
    ## select two roots (they're all real)
    r1 = rvals(1);
    r2 = rvals(2);
    poly = [1, -(r1+r2), (r1*r2)];
    r1i = 1;  r2i = 2;
  endif

  ## remove roots used
  idx = complement([r1i, r2i],1:length(rvals));
  rvals = rvals(idx);

endfunction

