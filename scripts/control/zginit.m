# Copyright (C) 1996,1998 A. Scottedward Hodel 
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation; either version 2, or (at your option) any 
# later version. 
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
# 
# You should have received a copy of the GNU General Public License 
# along with Octave; see the file COPYING.  If not, write to the Free 
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 
 
function zz = zginit(a,b,c,d)
  # zz = zginit(a,b,c,d)
  # construct right hand side vector zz
  # for the zero-computation generalized eigenvalue problem
  # balancing procedure
  # called by zgepbal
  # References:
  # ZGEP: Hodel, "Computation of Zeros with Balancing," Linear Algebra and
  #              its Applications, 1993
  # Generalized CG: Golub and Van Loan, "Matrix Computations, 2nd ed" 1989
  
  # A. S. Hodel July 24 1992
  # Conversion to Octave by R. Bruce Tenison, July 3, 1994
  # $Revision: 1.1 $
  # $Log: zginit.m,v $

  [nn,mm] = size(b);
  [pp,mm] = size(d);

  nmp = nn+mm+pp;

  # set up log vector zz
  zz = zeros(nmp,1);

  # zz part 1:
  for i=1:nn
    # nonzero off diagonal entries of a
    nidx = complement(i,1:nn);
    a_row_i = a(i,nidx);                 a_col_i = a(nidx,i);
    arnz = a_row_i(find(a_row_i != 0));  acnz = a_col_i(find(a_col_i != 0));

    # row of b
    bidx = find(b(i,:) != 0);
    b_row_i = b(i,bidx);

    # column of c
    cidx = find(c(:,i) != 0);
    c_col_i = c(cidx,i);
   
    # sum the entries
    zz(i) = sum(log(abs(acnz))) - sum(log(abs(arnz))) ...
            - sum(log(abs(b_row_i))) + sum(log(abs(c_col_i)));
  endfor

  # zz part 2:
  bd = [b;d];
  for i=1:mm
    i1 = i+nn;

    # column of [b;d]
    bdidx = find(bd(:,i) != 0);
    bd_col_i = bd(bdidx,i);
    zz(i1) = sum(log(abs(bd_col_i)));
  endfor

  # zz part 3:
  cd = [c d];
  for i=1:pp
    i1 = i+nn+mm;
    cdidx = find(cd(i,:) != 0);
    cd_row_i = cd(i,cdidx);
    zz(i1) = -sum(log(abs(cd_row_i)));
  endfor

  # now set zz as log base 2
  zz = zz*(1/log(2));
endfunction
