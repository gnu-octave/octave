## Copyright (C) 1996, 1998, 2000, 2004, 2005, 2007
##               Auburn University. All rights reserved.
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
## @deftypefn {Function File} {@var{zz} =} zginit (@var{a}, @var{b}, @var{c}, @var{d})
## Construct right hand side vector @var{zz}
## for the zero-computation generalized eigenvalue problem
## balancing procedure.  Called by @command{zgepbal}.
## @end deftypefn

## References:
## ZGEP: Hodel, "Computation of Zeros with Balancing," 1992, submitted to  LAA
## Generalized CG: Golub and Van Loan, "Matrix Computations, 2nd ed" 1989

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 24, 1992
## Conversion to Octave by R. Bruce Tenison, July 3, 1994

function zz = zginit (a, b, c, d)

  [nn,mm] = size(b);
  [pp,mm] = size(d);

  nmp = nn+mm+pp;

  ## set up log vector zz
  zz = zeros(nmp,1);

  ## zz part 1:
  for i=1:nn
    ## nonzero off diagonal entries of a
    if(nn > 1)
      nidx = complement(i,1:nn);
      a_row_i = a(i,nidx);                 a_col_i = a(nidx,i);
      arnz = a_row_i(find(a_row_i != 0));  acnz = a_col_i(find(a_col_i != 0));
    else
      arnz = acnz = [];
    endif

    ## row of b
    bidx = find(b(i,:) != 0);
    b_row_i = b(i,bidx);

    ## column of c
    cidx = find(c(:,i) != 0);
    c_col_i = c(cidx,i);

    ## sum the entries
    zz(i) = sum(log(abs(acnz))) - sum(log(abs(arnz))) ...
            - sum(log(abs(b_row_i))) + sum(log(abs(c_col_i)));
  endfor

  ## zz part 2:
  bd = [b;d];
  for i=1:mm
    i1 = i+nn;

    ## column of [b;d]
    bdidx = find(bd(:,i) != 0);
    bd_col_i = bd(bdidx,i);
    zz(i1) = sum(log(abs(bd_col_i)));
  endfor

  ## zz part 3:
  cd = [c, d];
  for i=1:pp
    i1 = i+nn+mm;
    cdidx = find(cd(i,:) != 0);
    cd_row_i = cd(i,cdidx);
    zz(i1) = -sum(log(abs(cd_row_i)));
  endfor

  ## now set zz as log base 2
  zz = zz*(1/log(2));
endfunction
