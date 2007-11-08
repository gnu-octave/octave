## Copyright (C) 1998, 2000, 2002, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {[@var{dsys}, @var{fidx}] =} dmr2d (@var{sys}, @var{idx}, @var{sprefix}, @var{ts2}, @var{cuflg})
## convert a multirate digital system to a single rate digital system
## states specified by @var{idx}, @var{sprefix} are sampled at @var{ts2}, all
## others are assumed sampled at @var{ts1} = @code{sysgettsam (@var{sys})}.
##
## @strong{Inputs}
## @table @var
## @item   sys
## discrete time system;
## @code{dmr2d} exits with an error if @var{sys} is not discrete
## @item   idx
## indices or names of states with sampling time 
## @code{sysgettsam(@var{sys})} (may be empty); see @code{cellidx}
## @item   sprefix
## list of string prefixes of states with sampling time
## @code{sysgettsam(@var{sys})} (may be empty)
## @item   ts2
## sampling time of states not specified by @var{idx}, @var{sprefix}
## must be an integer multiple of @code{sysgettsam(@var{sys})}
## @item   cuflg
## "constant u flag" if @var{cuflg} is nonzero then the system inputs are
## assumed to be constant over the revised sampling interval @var{ts2}.
## Otherwise, since the inputs can change during the interval
## @var{t} in @math{[k ts2, (k+1) ts2]}, an additional set of inputs is
## included in the revised B matrix so that these intersample inputs
## may be included in the single-rate system.
## default @var{cuflg} = 1.
## @end table
##
## @strong{Outputs}
## @table @var
## @item   dsys
## equivalent discrete time system with sampling time @var{ts2}.
##
## The sampling time of sys is updated to @var{ts2}.
##
## if @var{cuflg}=0 then a set of additional inputs is added to
## the system with suffixes _d1, @dots{}, _dn to indicate their
## delay from the starting time k @var{ts2}, i.e.
## u = [u_1; u_1_d1; @dots{}, u_1_dn] where u_1_dk is the input
## k*ts1 units of time after u_1 is sampled. (@var{ts1} is
## the original sampling time of the discrete time system and
## @var{ts2} = (n+1)*ts1)
##
## @item   fidx
## indices of "formerly fast" states specified by @var{idx} and @var{sprefix};
## these states are updated to the new (slower) sampling interval @var{ts2}.
## @end table
##
## @strong{WARNING} Not thoroughly tested yet; especially when
## @var{cuflg} == 0.
## @end deftypefn

## Adapted from c2d by a.s.hodel@eng.auburn.edu

function [dsys, fidx] = dmr2d (sys, idx, sprefix, Ts2, cuflg)

  ## parse input arguments
  if (nargin != 4)
    print_usage ();

  elseif (!isstruct(sys))
    error("sys must be in system data structure form");

  elseif(!is_digital(sys))
    error("sys must be discrete-time; continuous time passed");
  
  endif

  if(is_signal_list(idx) | ischar(idx))
    idx = sysidx(sys,"st",idx);

  elseif (!(isvector(idx) | isempty(idx)))
    error(["idx(",num2str(rows(idx)),"x",num2str(columns(idx)), ...
      ") must be a vector"]);

  elseif (any(idx <= 0))
    idv = find(idx <= 0);
    ii = idv(1);
    error(["idx(",num2str(ii),")=",num2str(idx(ii)), ...
      "; entries of idx must be positive"]);

  elseif(!(is_signal_list(sprefix) | isempty(sprefix)))
    error("sprefix must be a signal list (see is_signal_list) or empty");

  elseif(!is_sample(Ts2))
    error(["Ts2=",num2str(Ts2),"; invalid sampling time"]);

  endif

  ## optional argument: cuflg
  if(nargin <= 4)
    cuflg = 1;          # default: constant inputs over Ts2 sampling interv.
  elseif( !isscalar(cuflg) )
    error("cuflg must be a scalar")
  elseif( cuflg != 0 | cuflg != 1)
    error(["cuflg = ",num2str(cuflg),", should be 0 or 1"]);
  endif

  ## extract  state space information
  [da,db,dc,dd,Ts1,nc,nz,stname,inname,outname,yd] = sys2ss(sys);

  ## compute number of steps
  if(Ts1 > Ts2)
    error(["Current sampling time=",num2str(Ts1)," > Ts2=",num2str(Ts2)]);
  endif
  nstp = floor(Ts2/Ts1+0.5);
  if(abs((Ts2 - Ts1*nstp)/Ts1) > 1e-12)
    warning(["dmr2d: Ts1=",num2str(Ts1),", Ts2=",num2str(Ts2), ...
      ", selecting nsteps=",num2str(nstp),"; mismatch"]);
  endif

  if(isempty(sprefix) & isempty(idx))
    warning("both sprefix and idx are empty; returning dsys=sys");
    fidx = [];
    dsys = sys;
    return
  elseif(isempty(sprefix))
    fidx = idx;
  else
    fidx = reshape(idx,1,length(idx));
    ## find states whose name begins with any strings in sprefix.
    ns = length(sprefix);
    for kk=1:ns
      spk = sprefix{kk};  # get next prefix and length
      spl = length(spk);

      ## check each state name
      for ii=1:nz
        sti = stname{ii};  # compare spk with this state name
        if(length(sti) >= spl)
          ## if the prefix matches and ii isn't already in the list, add ii
          if(strcmp(sti(1:spl),spk) & !any(fidx == ii) )
            fidx = sort([fidx,ii]);
          endif
        endif
      endfor
    endfor
  endif

  if(nstp == 0)
    warning("dmr2d: nstp = 0; setting tsam and returning");
    dsys = syschtsam(sys,Ts2);
    return
  elseif(nstp < 0)
    error(["nstp = ", num2str(nstp)," < 0; this shouldn't be!"]);
  endif

  ## permute system matrices
  pv = sysreorder(nz,fidx);
  pv = pv(nz:-1:1);          # reverse order to put fast modes in leading block

  ## construct inverse permutation
  Inz = eye(nz);
  pvi = (Inz(pv,:)'*[1:nz]')';

  ## permute A, B (stname permuted for debugging only)
  da = da(pv,pv);
  db = db(pv,:);
  stname = stname(pv);

  ## partition A, B:
  lfidx = length(fidx);
  bki = 1:lfidx;
  a11 = da(bki,bki);
  b1 = db(bki,:);

  if(lfidx < nz)
    lfidx1 = lfidx+1;
    bki2 = (lfidx1):nz;
    a12 = da(bki,bki2);
    b2 = db(bki2,:);
  else
    warning("dmr2d: converting entire A,B matrices to new sampling rate");
    lfidx1 = -1;
    bki2 = [];
  endif

  ## begin system conversion: nstp steps

  ## compute abar_{n-1}*a12 and appropriate b matrix stuff
  a12b = a12;      # running  total of abar_{n-1}*a12
  a12w = a12;      # current a11^n*a12  (start with n = 0)
  if(cuflg)
    b1b = b1;
    b1w = b1;
  else
    ## cuflg == 0, need to keep track of intersample inputs too
    nzdx = find(max(abs(b1)) != 0);  # FIXME: check tolerance relative to ||b1||
    b1w = b1(nzdx);
    innamenz = inname(nzdx);
    b1b = b1;                        # initial b1 must match columns in b2
  endif

  ## compute a11h = a11^nstp by squaring
  a11h = eye(size(a11));
  p2 = 1;
  a11p2 = a11;        #a11^p2

  nstpw = nstp;       # workspace for computing a11^nstp
  while(nstpw > 0.5)
    oddv = rem(nstpw,2);
    if(oddv)
      a11h = a11h*a11p2;
    endif
    nstpw = (nstpw-oddv)/2;
    if(nstpw > 0.5)
      a11p2 = a11p2*a11p2;    # a11^(next power of 2)
    endif
  endwhile

  ## FIXME: this part should probably also use squaring, but
  ## that would require exponentially growing memory.  What do do?
  for kk=2:nstp
    ## update a12 block to sum(a12 + ... + a11^(kk-1)*a12)
    a12w = a11*a12w;
    a12b = a12b + a12w;

    ## similar for b1 block (checking for cuflg first!)
    b1w = a11*b1w;
    if(cuflg)
      b1b = b1b + b1w;        # update b1 block just like we did a12
    else
      b1b = [b1b, b1w];       # append new inputs
      newin = strappend(innamenz,["_d",num2str(kk-1)]);
      inname = __sysconcat__(inname,newin);
    endif
  endfor

  ## reconstruct system and return
  da(bki,bki) = a11h;
  db(bki,1:columns(b1b)) = b1b;
  if(!isempty(bki2))
    da(bki,bki2) = a12b;
  endif

  da = da(pvi,pvi);
  db = db(pvi,:);
  stname = stname(pvi);

  ## construct new system and return
  dsys = ss(da,db,dc,dd,Ts2,0,nz,stname,inname,outname,find(yd == 1));

endfunction
