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
## @deftypefn {Function File} {[@var{rldata}, @var{k_break}, @var{rlpol}, @var{gvec}, @var{real_ax_pts}] =} rlocus (@var{sys}[, @var{increment}, @var{min_k}, @var{max_k}])
##
## Display root locus plot of the specified @acronym{SISO} system.
## @example
## @group
##        -----   ---     --------
##    --->| + |---|k|---->| SISO |----------->
##        -----   ---     --------        |
##        - ^                             |
##          |_____________________________|
## @end group
## @end example
##
## @strong{Inputs}
## @table @var
## @item sys
## system data structure
## @item min_k
## Minimum value of @var{k}
## @item max_k
## Maximum value of @var{k}
## @item increment
## The increment used in computing gain values
## @end table
##
## @strong{Outputs}
##
## Plots the root locus to the screen.
## @table @var 
## @item rldata
## Data points plotted: in column 1 real values, in column 2 the imaginary values.
## @item k_break
## Gains for real axis break points.
## @item rlpol
## Closed-loop roots for each gain value: 1 locus branch per row; 1 pole
## set per column
## @item gvec
## Gains vector
## @item real_ax_pts
## Real axis breakpoints
## @end table
## @end deftypefn

## Author: David Clem
## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Updated by Kristi McGowan July 1996 for intelligent gain selection
## Updated by John Ingram July 1996 for systems

function [rldata, k_break, rlpol, gvec, real_ax_pts] = rlocus (sys, increment, min_k, max_k)

  if (nargin < 1) | (nargin > 4)
    usage("rlocus(sys[,inc,mink,maxk])");
  endif

  ## Convert the input to a transfer function if necessary

  [num,den] = sys2tf(sys)               # extract numerator/denom polyomials
  lnum = length(num);      lden = length(den);
  if(lden < 2)
    error(sprintf("length of derivative=%d, doesn't make sense",lden));
  elseif(lnum == 1)
    num = [0, num];     # so that derivative is shortened by one
  endif

  ## root locus plot axis limits

  ## compute real axis locus breakpoints
  ## compute the derivative of the numerator and the denominator
  dern=polyderiv(num);        derd=polyderiv(den);

  ## compute real axis breakpoints
  real_ax_pol = conv(den,dern) - conv(num,derd);
  real_ax_pts = roots(real_ax_pol);
  if(isempty(real_ax_pts))
    k_break = [];
    maxk = 0;
  else
    ## compute gains that achieve the breakpoints
    c1 = polyval(num,real_ax_pts);
    c2 = polyval(den,real_ax_pts);
    k_break = -real(c2 ./ c1);
    maxk = max(max(k_break,0));
  endif

  ## compute gain ranges based on computed K values
  if(maxk == 0)     maxk = 1;
  else              maxk = 1.1*maxk;        endif
  mink = 0;
  ngain = 20;

  ## check for input arguments:
  if (nargin > 2)       mink = min_k;          endif
  if (nargin > 3)       maxk = max_k;          endif
  if (nargin > 1)
    if(increment <= 0)  error("increment must be positive");
    else
      ngain = (maxk-mink)/increment;
    endif
  endif

  ## vector of gains
  ngain = max(3,ngain);
  gvec = linspace(mink,maxk,ngain);

  ## Find the open loop zeros and the initial poles
  rlzer = roots(num);

  ## update num to be the same length as den
  lnum = length(num);  if(lnum < lden) num = [zeros(1,lden - lnum),num];  endif

  ## compute preliminary pole sets
  nroots = lden-1;
  for ii=1:ngain
   gain = gvec(ii);
   rlpol(1:nroots,ii)  = vec(sortcom(roots(den + gain*num)));
  endfor

  ## compute axis limits (isolate asymptotes)
  olpol = roots(den);
  real_axdat = union(real(rlzer), real(union(olpol,real_ax_pts)) );
  rmin = min(real_axdat);      rmax = max(real_axdat);

  rlpolv = [vec(rlpol); vec(real_axdat)];
  idx = find(real(rlpolv) >= rmin & real(rlpolv) <= rmax);
  axlim = axis2dlim([real(rlpolv(idx)),imag(rlpolv(idx))]);
  xmin = axlim(1);
  xmax = axlim(2);

  ## set smoothing tolerance per axis limits
  smtol = 0.01*max(abs(axlim));

  ## smooth poles if necessary, up to maximum of 1000 gain points
  ## only smooth points within the axis limit window
  ## smoothing done if max_k not specified as a command argument
  done=(nargin == 4);    # perform a smoothness check
  while((!done) & ngain < 1000)
    done = 1 ;      # assume done
    dp = abs(diff(rlpol'))';
    maxd = max(dp);
    ## search for poles in the real axis limits whose neighbors are distant
    idx = find(maxd > smtol);
    for ii=1:length(idx)
      i1 = idx(ii);      g1 = gvec(i1);       p1 = rlpol(:,i1);
      i2 = idx(ii)+1;    g2 = gvec(i2);       p2 = rlpol(:,i2);

      ## isolate poles in p1, p2 that are inside the real axis limits
      bidx = find( (real(p1) >= xmin & real(p1) <= xmax)  ...
          | (real(p2) >= xmin & real(p2) <= xmax) );
      if(!isempty(bidx))
        p1 = p1(bidx);
        p2 = p2(bidx);
        if( max(abs(p2-p1)) > smtol)
          newg = linspace(g1,g2,5);
          newg = newg(2:4);
          if(isempty(newg))
            printf("rlocus: empty newg")
            g1
            g2
            i1
            i2
            idx_i1 = idx(ii)
            gvec_i1 = gvec(i1:i2)
            delta_vec_i1 = diff(gvec(i1:i2))
            prompt
          endif
          gvec =  [gvec,newg];
          done = 0;             # need to process new gains
        endif
      endif
    endfor

    ## process new gain values
    ngain1 = length(gvec);
    for ii=(ngain+1):ngain1
      gain = gvec(ii);
      rlpol(1:nroots,ii)  = vec(sortcom(roots(den + gain*num)));
    endfor

    [gvec,idx] = sort(gvec);
    rlpol = rlpol(:,idx);
    ngain = length(gvec);
  endwhile

  ## Plot the data
  if(nargout  == 0)
    rlpolv = vec(rlpol);
    idx = find(real(rlpolv) >= xmin & real(rlpolv) <= xmax);
    axdata = [real(rlpolv(idx)),imag(rlpolv(idx))];
    axlim = axis2dlim(axdata);
    axlim(1:2) = [xmin, xmax];
    __gnuplot_set__ nologscale xy;
    grid("on");
    rldata = [real(rlpolv), imag(rlpolv) ];
    axis(axlim);
    [stn,inname,outname] = sysgetsignals(sys);
    xlabel(sprintf("Root locus from %s to %s, gain=[%f,%f]: Real axis", ...
        inname{1}, outname{1},gvec(1),gvec(ngain)));
    ylabel("Imag. axis");

    plot(real(rlpolv),imag(rlpolv),".1;locus points;", ...
        real(olpol),imag(olpol),"x2;open loop poles;", ...
        real(rlzer),imag(rlzer),"o3;zeros;");
    rldata = [];
  endif
endfunction
