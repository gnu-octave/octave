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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{K} =} hinf_ctr (@var{dgs}, @var{f}, @var{h}, @var{z}, @var{g})
## Called by @code{hinfsyn} to compute the 
## @iftex
## @tex
## $ { \cal H }_\infty $
## @end tex
## @end iftex
## @ifinfo
## H-infinity
## @end ifinfo
## optimal controller.
##
## @strong{Inputs}
## @table @var
## @item dgs
## data structure returned by @code{is_dgkf}
## @item f
## @itemx h
## feedback and filter gain (not partitioned)
## @item g
## final gamma value
## @end table
## @strong{Outputs}
## @table @var
## @item K
## controller (system data structure)
## @end table
##
## Do not attempt to use this at home; no argument checking performed.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## Revised by Kai P. Mueller April 1998 to solve the general H_infinity
## problem using unitary transformations Q (on w and z)
## and non-singular transformations R (on u and y).

function K = hinf_ctr (dgs, F, H, Z, g)

  nw = dgs.nw;
  nu = dgs.nu;
  nz = dgs.nz;
  ny = dgs.ny;
  d22nz = dgs.Dyu_nz;

  B1  = dgs.Bw;
  B2  = dgs.Bu;
  C1  = dgs.Cz;
  C2  = dgs.Cy;
  C = [C1; C2];
  D11 = dgs.Dzw;
  D12 = dgs.Dzu;
  D21 = dgs.Dyw;
  D22 = dgs.Dyu;
  A = dgs.A;
  Ru = dgs.Ru;
  Ry = dgs.Ry;

  nout = nz + ny;
  nin = nw + nu;
  nstates = size(A, 1);

  F11 = F(1:(nw-ny),:);
  F12 = F((nw-ny+1):nw,:);
  F2  = F((nw+1):nin,:);
  H11 = H(:,1:(nz-nu));
  H12 = H(:,(nz-nu+1):nz);
  H2  = H(:,(nz+1):nout);

  ## D11 partitions
  D1111 = D11(1:(nz-nu),1:(nw-ny));
  D1112 = D11(1:(nz-nu),(nw-ny+1):nw);
  D1121 = D11((nz-nu+1):nz,1:(nw-ny));
  D1122 = D11((nz-nu+1):nz,(nw-ny+1):nw);

  ## D11ik may be the empty matrix, don't calculate with empty matrices
  [nd1111,md1111] = size(D1111);
  md1112 = length(D1112);
  md1121 = length(D1121);

  if ((nd1111 == 0) || (md1112 == 0))
    d11hat = -D1122;
  else
    xx = inv(g*g*eye(nz-nu) - D1111*D1111');
    d11hat = -D1121*D1111'*xx*D1112 - D1122;
  endif
  if (md1112 == 0)
    d21hat = eye(ny);
  elseif (nd1111 == 0)
    d21hat = chol(eye(ny) - D1112'*D1112/g/g);
  else
    xx = inv(g*g*eye(nz-nu) - D1111*D1111');
    xx = eye(ny) - D1112'*xx*D1112;
    d21hat = chol(xx);
  endif
  if (md1121 == 0)
    d12hat = eye(nu);
  elseif (md1111 == 0)
    d12hat = chol(eye(nu) - D1121*D1121'/g/g)';
  else
    xx = inv(g*g*eye(nw-ny) - D1111'*D1111);
    xx = eye(nu)-D1121*xx*D1121';
    d12hat = chol(xx)';
  endif

  b2hat = (B2+H12)*d12hat;
  c2hat = -d21hat*(C2+F12)*Z;
  b1hat = -H2 + (b2hat/d12hat)*d11hat;
  c1hat =  F2*Z + (d11hat/d21hat)*c2hat;
  ahat  =  A + H*C + (b2hat/d12hat)*c1hat;

  ## rescale controller by Ru and Ry
  b1hat = b1hat/Ry;
  c1hat = Ru\c1hat;
  bhat  = [b1hat, b2hat];
  chat  = [c1hat; c2hat];
  dhat  = [Ru\d11hat/Ry, Ru\d12hat; d21hat/Ry, 0*d11hat'];

  ## non-zero D22 is a special case
  if (d22nz)
    if (rank(eye(nu) + d11hat*D22) < nu)
      error(" *** cannot compute controller for D22 non-zero.");
    endif

    d22new = [D22, zeros(ny,ny); zeros(nu,nu), 0*D22'];
    xx = inv(eye(nu+ny) + d22new*dhat);
    mhat = inv(eye(nu+ny) + dhat*d22new);
    ahat = ahat - bhat*((eye(nu+ny)-xx)/dhat)*chat;
    bhat = bhat*xx;
    chat = mhat*chat;
    dhat = dhat*xx;

  endif

  K = ss(ahat,bhat(:,1:ny),chat(1:nu,:),dhat(1:nu,1:ny));

endfunction
