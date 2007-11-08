## Copyright (C) 1996, 2000, 2005, 2007
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

## -*- texinfo -*-
## @deftypefn {Function File} {} zgreduce (@var{sys}, @var{meps})
## Implementation of procedure REDUCE in (Emami-Naeini and Van Dooren,
## Automatica, # 1982).
## @end deftypefn

function retsys = zgreduce (Asys, meps)

  if (nargin != 2)
    print_usage ();
  endif  

  ## SYS_INTERNAL accesses members of system data structure

  is_digital(Asys);             # make sure it's pure digital/continuous

  exit_1 = 0;                   # exit_1 = 1 or 2 on exit of loop

  if(Asys.n + Asys.nz == 0)
    exit_1 = 2;                 # there are no finite zeros
  endif

  while (! exit_1)
    [Q,R,Pi] = qr(Asys.d);              # compress rows of D
    Asys.d = Q'*Asys.d;
    Asys.c = Q'*Asys.c;

    ## check row norms of Asys.d
    [sig,tau] = zgrownorm(Asys.d,meps);

    ## disp("=======================================")
    ## disp(["zgreduce: meps=",num2str(meps), ", sig=",num2str(sig), ...
    ##   ", tau=",num2str(tau)])
    ## sysout(Asys)

    if(tau == 0)
      exit_1 = 1;               # exit_1 - reduction complete and correct
    else
      Cb = Db = [];
      if(sig)
        Cb = Asys.c(1:sig,:);
        Db = Asys.d(1:sig,:);
      endif
      Ct =Asys.c(sig+(1:tau),:);

      ## compress columns of Ct
      [pp,nn] = size(Ct);
      rvec = nn:-1:1;
      [V,Sj,Pi] = qr(Ct');
      V = V(:,rvec);
      [rho,gnu] = zgrownorm(Sj,meps);

      ## disp(["zgreduce: rho=",num2str(rho),", gnu=",num2str(gnu)])
      ## Cb
      ## Db
      ## Ct
      ## Sj'

      if(rho == 0)
        exit_1 = 1;     # exit_1 - reduction complete and correct
      elseif(gnu == 0)
        exit_1 = 2;     # there are no zeros at all
      else
        mu = rho + sig;

        ## update system with Q
        M = [Asys.a , Asys.b ];
        [nn,mm] = size(Asys.b);

        pp = rows(Asys.d);
        Vm =[V,zeros(nn,mm) ; zeros(mm,nn), eye(mm)];
        if(sig)
          M = [M; Cb, Db];
          Vs =[V',zeros(nn,sig) ; zeros(sig,nn), eye(sig)];
        else
          Vs = V';
        endif
        ## disp("zgreduce: before transform: M=");
        ## M
        ## Vs
        ## Vm

        M = Vs*M*Vm;

        ## disp("zgreduce: after transform: M=");
        ## M

        ## disp("debugging code:")
        ## Mtmp = [Asys.a Asys.b; Asys.c Asys.d]
        ## Vl = [V', zeros(nn,mm); zeros(mm,nn),Q]
        ## Vr =[V,zeros(nn,mm) ; zeros(mm,nn), eye(mm)];
        ## Mtmpf = Vl*Mtmp*Vr

        idx = 1:gnu;
        jdx = nn + (1:mm);
        sdx = gnu + (1:mu);

        Asys.a = M(idx,idx);
        Asys.b = M(idx,jdx);
        Asys.c = M(sdx,idx);
        Asys.d = M(sdx,jdx);

        ## disp(["zgreduce: resulting system: nn =",num2str(nn)," mu=",num2str(mu)])
        ## sysout(Asys)
        ## idx
        ## jdx
        ## sdx
      endif
    endif
  endwhile

  ## disp(["zgreduce: while loop done: exit_1=",num2str(exit_1)]);

  if(exit_1 == 2)
    ## there are no zeros at all!
    Asys.a = Asys.b = Asys.c = [];
  endif

  ## update dimensions
  if(is_digital(Asys))
    Asys.nz = rows(Asys.a);
  else
    Asys.n = rows(Asys.a);
  endif

  retsys = Asys;

endfunction
