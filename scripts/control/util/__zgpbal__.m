## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __zgpbal__ (@var{sys})
##
## Used internally in @command{tzero}; minimal argument checking performed.
##
## Implementation of zero computation generalized eigenvalue problem
## balancing method (Hodel and Tiller, Allerton Conference, 1991)
## Based on Ward's balancing algorithm (@acronym{SIAM} J. Sci Stat. Comput., 1981).
##
## @command{__zgpbal__} computes a state/input/output weighting that attempts to
## reduced the range of the magnitudes of the nonzero elements of [@var{a}, @var{b},
## @var{c}, @var{d}].
## The weighting uses scalar multiplication by powers of 2, so no roundoff
## will occur.
##
## @command{__zgpbal__} should be followed by @command{zgpred}.
## @end deftypefn

## References:
## ZGEP: Hodel, "Computation of Zeros with Balancing," 1992, submitted to  LAA
## Generalized CG: Golub and Van Loan, "Matrix Computations, 2nd ed" 1989

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 24, 1992
## Conversion to Octave by R. Bruce Tenison July 3, 1994

function retsys = __zgpbal__ (Asys)

  if( (nargin != 1) | (!isstruct(Asys)))
    usage("retsys = __zgpbal__ (Asys)");
  endif

  Asys = sysupdate(Asys,"ss");
  [a,b,c,d] = sys2ss(Asys);

  [nn,mm,pp] = abcddim(a,b,c,d);

  np1 = nn+1;
  nmp = nn+mm+pp;

  ## set up log vector zz, incidence matrix ff
  zz = zginit(a,b,c,d);

  ## disp("__zgpbal__: zginit returns")
  ## zz
  ## disp("/__zgpbal__")

  if (norm(zz))
    ## generalized conjugate gradient approach
    xx = zgscal(a,b,c,d,zz,nn,mm,pp);

    for i=1:nmp
      xx(i) = floor(xx(i)+0.5);
      xx(i) = 2.0^xx(i);
    endfor

    ## now scale a
    ## block 1: a = sigma a inv(sigma)
    for i=1:nn
      a(i,1:nn) = a(i,1:nn)*xx(i);
      a(1:nn,i) = a(1:nn,i)/xx(i);
    endfor
    ## block 2: b= sigma a phi
    for j=1:mm
      j1 = j+nn;
      b(1:nn,j) = b(1:nn,j)*xx(j1);
    endfor
    for i=1:nn
      b(i,1:mm) = b(i,1:mm)*xx(i);
    endfor
    for i=1:pp
      i1 = i+nn+mm;
      ## block 3: c = psi C inv(sigma)
      c(i,1:nn) = c(i,1:nn)*xx(i1);
    endfor
    for j=1:nn
      c(1:pp,j) = c(1:pp,j)/xx(j);
    endfor
    ## block 4: d = psi D phi
    for j=1:mm
      j1 = j+nn;
      d(1:pp,j) = d(1:pp,j)*xx(j1);
    endfor
    for i=1:pp
      i1 = i + nn + mm;
      d(i,1:mm) = d(i,1:mm)*xx(i1);
    endfor
  endif

  retsys = ss(a,b,c,d);
endfunction

