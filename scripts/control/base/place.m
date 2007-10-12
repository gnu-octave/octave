## Copyright (C) 1997 Jose Daniel Munoz Frias
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
## @deftypefn {Function File} {@var{K} =} place (@var{sys}, @var{p})
## Computes the matrix @var{K} such that if the state
## is feedback with gain @var{K}, then the eigenvalues  of the closed loop
## system (i.e. @math{A-BK}) are those specified in the vector @var{p}.
##
## Version: Beta (May-1997): If you have any comments, please let me know.
## (see the file place.m for my address)
## @end deftypefn

## Author: Jose Daniel Munoz Frias

## Universidad Pontificia Comillas
## ICAIdea
## Alberto Aguilera, 23
## 28015 Madrid, Spain
##
## E-Mail: daniel@dea.icai.upco.es
##
## Phone: 34-1-5422800   Fax: 34-1-5596569
##
## Algorithm taken from "The Control Handbook", IEEE press pp. 209-212
##
## code adaped by A.S.Hodel (a.s.hodel@eng.auburn.edu) for use in controls
## toolbox

function K = place (sys, P)

  ## check arguments

  if(!isstruct(sys))
    error("sys must be in system data structure format (see ss)");
  endif
  sys = sysupdate(sys,"ss");    # make sure it has state space form up to date
  if(!is_controllable(sys))
    error("sys is not controllable.");
  elseif( min(size(P)) != 1)
    error("P must be a vector")
  else
    P = reshape(P,length(P),1); # make P a column vector
  endif
  ## system must be purely continuous or discrete
  is_digital(sys);
  [n,nz,m,p] = sysdimensions(sys);
  nx = n+nz;    # already checked that it's not a mixed system.
  if(m != 1)
    error(["sys has ", num2str(m)," inputs; need only 1"]);
  endif

  ## takes the A and B matrix from the system representation
  [A,B]=sys2ss(sys);
  sp = length(P);
  if(nx == 0)
    error("place: A matrix is empty (0x0)");
  elseif(nx != length(P))
    error(["A=(",num2str(nx),"x",num2str(nx),", P has ", num2str(length(P)), ...
	"entries."])
  endif

  ## arguments appear to be compatible; let's give it a try!
  ## The second step is the calculation of the characteristic polynomial ofA
  PC=poly(A);

  ## Third step: Calculate the transformation matrix T that transforms the state
  ## equation in the controllable canonical form.

  ## first we must calculate the controllability matrix M:
  M=B;
  AA=A;
  for n = 2:nx
    M(:,n)=AA*B;
    AA=AA*A;
  endfor

  ## second, construct the matrix W
  PCO=PC(nx:-1:1);
  PC1=PCO;      # Matrix to shift and create W row by row

  for n = 1:nx
    W(n,:) = PC1;
    PC1=[PCO(n+1:nx),zeros(1,n)];
  endfor

  T=M*W;

  ## finaly the matrix K is calculated
  PD = poly(P); # The desired characteristic polynomial
  PD = PD(nx+1:-1:2);
  PC = PC(nx+1:-1:2);

  K = (PD-PC)/T;

  ## Check if the eigenvalues of (A-BK) are the same specified in P
  Pcalc = eig(A-B*K);

  Pcalc = sortcom(Pcalc);
  P = sortcom(P);

  if(max( (abs(Pcalc)-abs(P))./abs(P) ) > 0.1)
    disp("Place: Pole placed at more than 10% relative error from specified");
  endif

endfunction

