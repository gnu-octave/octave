## Copyright (C) 1998 Kai P. Mueller
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
## @deftypefn {Function File} {} buildssic (@var{clst}, @var{ulst}, @var{olst}, @var{ilst}, @var{s1}, @var{s2}, @var{s3}, @var{s4}, @var{s5}, @var{s6}, @var{s7}, @var{s8})
##
## Form an arbitrary complex (open or closed loop) system in
## state-space form from several systems. @command{buildssic} can
## easily (despite its cryptic syntax) integrate transfer functions
## from a complex block diagram into a single system with one call.
## This function is especially useful for building open loop
## interconnections for 
## @iftex
## @tex
## $ { \cal H }_\infty $ and $ { \cal H }_2 $
## @end tex
## @end iftex
## @ifinfo
## H-infinity and H-2
## @end ifinfo
## designs or for closing loops with these controllers.
##
## Although this function is general purpose, the use of @command{sysgroup}
## @command{sysmult}, @command{sysconnect} and the like is recommended for
## standard operations since they can handle mixed discrete and continuous
## systems and also the names of inputs, outputs, and states.
##
## The parameters consist of 4 lists that describe the connections
## outputs and inputs and up to 8 systems @var{s1}--@var{s8}.
## Format of the lists:
## @table @var
## @item      clst
## connection list, describes the input signal of
## each system. The maximum number of rows of Clst is
## equal to the sum of all inputs of s1-s8.
##
## Example:
## @code{[1 2 -1; 2 1 0]} means that:  new input 1 is old input 1
## + output 2 - output 1, and new input 2 is old input 2
## + output 1. The order of rows is arbitrary.
##
## @item ulst
## if not empty the old inputs in vector @var{ulst} will
## be appended to the outputs. You need this if you
## want to ``pull out'' the input of a system. Elements
## are input numbers of @var{s1}--@var{s8}.
##
## @item olst
## output list, specifiy the outputs of the resulting
## systems. Elements are output numbers of @var{s1}--@var{s8}.
## The numbers are allowed to be negative and may
## appear in any order. An empty matrix means
## all outputs.
##
## @item ilst
## input list, specifiy the inputs of the resulting
## systems. Elements are input numbers of @var{s1}--@var{s8}.
## The numbers are allowed to be negative and may
## appear in any order. An empty matrix means
## all inputs.
## @end table
##
## Example:  Very simple closed loop system.
## @example
## @group
## w        e  +-----+   u  +-----+
##  --->o--*-->|  K  |--*-->|  G  |--*---> y
##      ^  |   +-----+  |   +-----+  |
##    - |  |            |            |
##      |  |            +----------------> u
##      |  |                         |
##      |  +-------------------------|---> e
##      |                            |
##      +----------------------------+
## @end group
## @end example
##
## The closed loop system @var{GW} can be optained by
## @example
## GW = buildssic([1 2; 2 -1], 2, [1 2 3], 2, G, K);
## @end example
## @table @var
## @item clst
## 1st row: connect input 1 (@var{G}) with output 2 (@var{K}).
##
## 2nd row: connect input 2 (@var{K}) with negative output 1 (@var{G}).
## @item ulst
## Append input of 2 (@var{K}) to the number of outputs.
## @item olst
## Outputs are output of 1 (@var{G}), 2 (@var{K}) and 
## appended output 3 (from @var{ulst}).
## @item ilst
## The only input is 2 (@var{K}).
## @end table
##
## Here is a real example:
## @example
## @group
##                          +----+
##     -------------------->| W1 |---> v1
## z   |                    +----+
## ----|-------------+
##     |             |
##     |    +---+    v      +----+
##     *--->| G |--->O--*-->| W2 |---> v2
##     |    +---+       |   +----+
##     |                |
##     |                v
##    u                  y
## @end group
## @end example
## @iftex
## @tex
## $$ { \rm min } \Vert GW_{vz} \Vert _\infty $$  
## @end tex
## @end iftex
## @ifinfo
## @example
## min || GW   ||
##          vz   infty
## @end example
## @end ifinfo
##
## The closed loop system @var{GW} 
## @iftex
## @tex
## from $ [z, u]^T $ to $ [v_1, v_2, y]^T $
## @end tex
## @end iftex
## @ifinfo
## from [z, u]' to [v1, v2, y]' 
## @end ifinfo
## can be obtained by (all @acronym{SISO} systems):
## @example
## GW = buildssic([1, 4; 2, 4; 3, 1], 3, [2, 3, 5],
##                [3, 4], G, W1, W2, One);
## @end example
## where ``One'' is a unity gain (auxillary) function with order 0.
## (e.g. @code{One = ugain(1);})
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: April 1998

function sys = buildssic (Clst, Ulst, Olst, Ilst, s1, s2, s3, s4, s5, s6, s7, s8)

  if((nargin < 5) || (nargin > 12))
    usage("sys = buildssic(Clst,Ulst,Olst,Ilst,s1,s2,s3,s4,s5,s6,s7,s8)");
  endif
  if (nargin >= 5)
    if (!isstruct(s1))
      error("---> s1 must be a structed system.");
    endif
    s1 = sysupdate(s1, "ss");
    [n, nz, m, p] = sysdimensions(s1);
    if (!n && !nz)
      error("---> pure static system must not be the first in list.");
    endif
    if (n && nz)
      error("---> cannot handle mixed continuous and discrete systems.");
    endif
    D_SYS = (nz > 0);
    [A,B,C,D,tsam] = sys2ss(s1);
    nt = n + nz;
  endif
  for ii = 6:nargin
    eval(["mysys = s", num2str(ii-4), ";"]);
    if (!isstruct(mysys))
      error("---> Parameter must be a structed system.");
    endif
    mysys = sysupdate(mysys, "ss");
    [n1, nz1, m1, p1] = sysdimensions(mysys);
    if (n1 && nz1)
      error("---> cannot handle mixed continuous and discrete systems.");
    endif
    if (D_SYS)
      if (n1)
        error("---> cannot handle mixed cont. and discr. systems.");
      endif
      if (tsam != sysgettsam(mysys))
        error("---> sampling time of all systems must match.");
      endif
    endif
    [as,bs,cs,ds] = sys2ss(mysys);
    nt1 = n1 + nz1;
    if (!nt1)
      ## pure gain (pad B, C with zeros)
      B = [B, zeros(nt,m1)];
      C = [C; zeros(p1,nt)];
    else
      A = [A, zeros(nt,nt1); zeros(nt1,nt), as];
      B = [B, zeros(nt,m1);  zeros(nt1,m),  bs];
      C = [C, zeros(p,nt1);  zeros(p1,nt),  cs];
    endif
    D = [D, zeros(p,m1); zeros(p1,m), ds];
    n = n + n1;
    nz = nz + nz1;
    nt = nt + nt1;
    m = m + m1;
    p = p + p1;
  endfor

  ## check maximum dimensions
  [nx, mx] = size(Clst);
  if (nx > m)
    error("---> more rows in Clst than total number of inputs.");
  endif
  if (mx > p+1)
    error("---> more cols in Clst than total number of outputs.");
  endif
  ## empty vector Ulst is OK
  lul = length(Ulst);
  if (lul)
    if (!isvector(Ulst))
      error("---> Input u list Ulst must be a vector.");
    endif
    if (lul > m)
      error("---> more values in Ulst than number of inputs.");
    endif
  endif
  if (!length(Olst))  Olst = [1:(p+lul)];  endif
  if (!length(Ilst))  Ilst = [1:m];        endif
  if (!isvector(Olst))
    error("---> Output list Olst must be a vector.");
  endif
  if (!isvector(Ilst))
    error("---> Input list Ilst must be a vector.");
  endif

  ## build the feedback "K" from the interconnection data Clst
  K = zeros(m, p);
  inp_used = zeros(m,1);
  for ii = 1:nx
    xx = Clst(ii,:);
    iu = xx(1);
    if ((iu < 1) || (iu > m))
      error("---> invalid value in first col of Clst.");
    endif
    if (inp_used(iu))
      error("---> Input specified more than once.");
    endif
    inp_used(iu) = 1;
    for kk = 2:mx
      it = xx(kk);
      if (abs(it) > p)
        error("---> invalid row value in Clst.");
      elseif (it)
        K(iu,abs(it)) = sign(it);
      endif
    endfor
  endfor

  ## form the "closed loop", i.e replace u in
  ## .
  ## x = Ax + Bu
  ##                            ~
  ## y = Cx + Du   by   u = K*y+u
  ##
  ##            -1
  ## R = (I-D*K)   must exist.

  R = eye(p) - D*K;
  if (rank(R) < p)
    error("---> singularity in algebraic loop.");
  else
    R = inv(R);
  endif
  A = A + B*K*R*C;
  B = B + B*K*R*D;
  C = R*C;
  D = R*D;

  ## append old inputs u to the outputs (if lul > 0)
  kc = K*C;
  kdi = eye(m) + K*D;
  for ii = 1:lul
    it = Ulst(ii);
    if ((it < 1) || (it > m))
      error("---> invalid value in Ulst.");
    endif
    C = [C; kc(it,:)];
    D = [D; kdi(it,:)];
  endfor

  ## select and rearrange outputs
  nn = length(A);
  lol = length(Olst);
  Cnew = zeros(lol,nn);
  Dnew = zeros(lol,m);
  for ii = 1:lol
    iu = Olst(ii);
    if (!iu || (abs(iu) > p+lul))
      error("---> invalid value in Olst.");
    endif
    Cnew(ii,:) = sign(iu)*C(abs(iu),:);
    Dnew(ii,:) = sign(iu)*D(abs(iu),:);
  endfor
  C = Cnew;
  D = Dnew;
  lil = length(Ilst);
  Bnew = zeros(nn,lil);
  Dnew = zeros(lol,lil);
  for ii = 1:lil
    iu = Ilst(ii);
    if (!iu || (abs(iu) > m))
      error("---> invalid value in Ilst.");
    endif
    Bnew(:,ii) = sign(iu)*B(:,abs(iu));
    Dnew(:,ii) = sign(iu)*D(:,abs(iu));
  endfor

  sys = ss(A, Bnew, C, Dnew, tsam, n, nz);

endfunction
