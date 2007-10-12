## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{rldata}, @var{k}] =} rlocus (@var{sys}[, @var{increment}, @var{min_k}, @var{max_k}])
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
## @item k
## Gains for real axis break points.
## @end table
## @end deftypefn

## Author: David Clem
## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Updated by Kristi McGowan July 1996 for intelligent gain selection
## Updated by John Ingram July 1996 for systems

function [rldata, k_break, rlpol, gvec, real_ax_pts] = rlocus (sys, increment, min_k, max_k)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  ## Convert the input to a transfer function if necessary
  [num,den] = sys2tf(sys);               # extract numerator/denom polyomials
  lnum = length(num);      
  lden = length(den);
  # equalize length of num, den polynomials
  if(lden < 2)
    error("system has no poles");
  elseif(lnum < lden)
    num = [zeros(1,lden-lnum), num];  # so that derivative is shortened by one
  endif

  olpol = roots(den);
  olzer = roots(num);
  nas = lden -lnum; # number of asymptotes
  maxk = 0;
  if(nas > 0)
    cas = ( sum(olpol) - sum(olzer) )/nas;
    angles = (2*[1:nas]-1)*pi/nas;
    # printf("rlocus: there are %d asymptotes centered at %f\n", nas, cas);
  else
    cas = angles = [];
    maxk = 100*den(1)/num(1);
  endif


  # compute real axis break points and corresponding gains
  dnum=polyderiv(num);
  dden=polyderiv(den);
  brkp = conv(den, dnum) - conv(num, dden);
  real_ax_pts = roots(brkp);
  real_ax_pts = real_ax_pts(find(imag(real_ax_pts) == 0));
  k_break = -polyval(den,real_ax_pts) ./ polyval(num, real_ax_pts);
  idx = find(k_break >= 0);
  k_break = k_break(idx);
  real_ax_pts = real_ax_pts(idx);
  if(!isempty(k_break))
    maxk = max(max(k_break),maxk);
  endif
  
  if(nas == 0)
    maxk = max(1, 2*maxk);  % get at least some root locus
  else
    # get distance from breakpoints, poles, and zeros to center of asymptotes
    dmax = 3*max(abs( [vec(olzer); vec(olpol); vec(real_ax_pts)] - cas ));
    if(dmax == 0)
      dmax = 1;
    endif
 
    # get gain for dmax along each asymptote, adjust maxk if necessary
    svals = cas + dmax*exp(j*angles);
    kvals = -polyval(den,svals) ./ polyval(num, svals);
    maxk = max(maxk, max(real(kvals)));
  end
  
  ## check for input arguments:
  if (nargin > 2)
    mink = min_k;
  else
    mink = 0;
  endif
  if (nargin > 3)
    maxk = max_k;
  endif
  if (nargin > 1)
    if(increment <= 0)
      error("increment must be positive");
    else
      ngain = (maxk-mink)/increment;
    endif
  else
    ngain = 30;
  endif

  ## vector of gains
  ngain = max(30,ngain);
  gvec = linspace(mink,maxk,ngain);
  if(length(k_break))
    gvec = sort([gvec, vec(k_break)']);
  endif

  ## Find the open loop zeros and the initial poles
  rlzer = roots(num);

  ## update num to be the same length as den
  lnum = length(num);  
  if(lnum < lden)
    num = [zeros(1,lden - lnum),num];
  endif

  ## compute preliminary pole sets
  nroots = lden-1;
  for ii=1:ngain
   gain = gvec(ii);
   rlpol(1:nroots,ii)  = vec(sortcom(roots(den + gain*num)));
  endfor

  ## set smoothing tolerance 
  smtolx = 0.01*( max(max(real(rlpol))) - min(min(real(rlpol))));
  smtoly = 0.01*( max(max(imag(rlpol))) - min(min(imag(rlpol))));
  smtol = max(smtolx, smtoly);
  rlpol = sort_roots(rlpol,smtolx, smtoly);   % sort according to nearest-neighbor

  done=(nargin == 4);    # perform a smoothness check
  while((!done) & ngain < 1000)
    done = 1 ;      # assume done
    dp = abs(diff(rlpol'))';
    maxdp = max(dp);
    
    ## search for poles whose neighbors are distant
    if(lden == 2)
      idx = find(dp > smtol);
    else
      idx = find(maxdp > smtol);
    endif

    for ii=1:length(idx)
      i1 = idx(ii);
      g1 = gvec(i1);
      p1 = rlpol(:,i1);

      i2 = idx(ii)+1;
      g2 = gvec(i2);
      p2 = rlpol(:,i2);

      ## isolate poles in p1, p2 
      if( max(abs(p2-p1)) > smtol)
        newg = linspace(g1,g2,5);
        newg = newg(2:4);
        gvec =  [gvec,newg];
        done = 0;             # need to process new gains
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
    rlpol = sort_roots(rlpol,smtolx, smtoly);   % sort according to nearest-neighbor
  endwhile
  rldata = rlpol;

  ## Plot the data
  if(nargout  == 0)
    rlpolv = vec(rlpol);
    axdata = [real(rlpolv),imag(rlpolv); real(olzer), imag(olzer)];
    axlim = axis2dlim(axdata);
    rldata = [real(rlpolv), imag(rlpolv) ];
    [stn,inname,outname] = sysgetsignals(sys);

    ## build plot command args pole by pole

    n_rlpol = rows(rlpol);
    nelts = n_rlpol+1;
    if (! isempty (rlzer))
      nelts++;
    endif
    # add asymptotes
    n_A = length (olpol) - length (olzer);
    if (n_A > 0)
      nelts += n_A;
    endif
    args = cell (3, nelts);
    kk = 0;
    # asymptotes first
    if (n_A > 0)
      len_A = 2*max(abs(axlim));
      sigma_A = (sum(olpol) - sum(olzer))/n_A;
      for i_A=0:n_A-1
        phi_A = pi*(2*i_A + 1)/n_A;
        args{1,++kk} = [sigma_A sigma_A+len_A*cos(phi_A)];
        args{2,kk} = [0 len_A*sin(phi_A)];
        if (i_A == 1)
          args{3,kk} = "k--;asymptotes;";
        else
          args{3,kk} = "k--";
        endif
      endfor
    endif
    # locus next
    for ii=1:rows(rlpol)
      args{1,++kk} = real (rlpol (ii,:));
      args{2,kk} = imag (rlpol (ii,:));
      if (ii == 1)
        args{3,kk} = "b-;locus;";
      else
        args{3,kk} = "b-";
      endif
    endfor
    # poles and zeros last
    args{1,++kk} = real(olpol);
    args{2,kk} = imag(olpol);
    args{3,kk} = "rx;open loop poles;";
    if (! isempty(rlzer))
      args{1,++kk} = real(rlzer);
      args{2,kk} = imag(rlzer);
      args{3,kk} = "go;zeros;";
    endif

    set (gcf,"visible","off");
    hplt = plot (args{:});
    set (hplt(kk--), "markersize", 2);
    if (! isempty(rlzer))
      set(hplt(kk--), "markersize", 2);
    endif
    for ii=1:rows(rlpol)
      set (hplt(kk--), "linewidth", 2);
    endfor
    legend ("boxon", 2);
    grid ("on");
    axis (axlim);
    xlabel (sprintf ("Root locus from %s to %s, gain=[%f,%f]: Real axis",
		     inname{1}, outname{1}, gvec(1), gvec(ngain)));
    ylabel ("Imag. axis");
    set (gcf,"visible","on");
    rldata = [];
  endif
endfunction

function rlpol = sort_roots (rlpol,tolx, toly)
  # no point sorting of you've only got one pole!
  if(rows(rlpol) == 1)
    return
  endif

  # reorder entries in each column of rlpol to be by their nearest-neighbors
  dp = diff(rlpol')';
  drp = max(real(dp));
  dip = max(imag(dp));
  idx = find( drp > tolx | dip > toly );
  if(isempty(idx) )
    return
  endif

  [np,ng] = size(rlpol);  # num poles, num gains
  for jj = idx
    vals = rlpol(:,[jj,jj+1]);
    jdx = (jj+1):ng;
    for ii=1:rows(rlpol-1)
      rdx = ii:np;
      dval = abs(rlpol(rdx,jj+1)-rlpol(ii,jj));
      mindist = min(dval);
      sidx = min( find ( dval == mindist)) + ii - 1;
      if( sidx != ii)
        c1 = norm(diff(vals'));
        [vals(ii,2), vals(sidx,2)] = swap( vals(ii,2), vals(sidx,2));
        c2 = norm(diff(vals'));
        if(c1 > c2 )
          # perform the swap
          [rlpol(ii,jdx), rlpol(sidx,jdx)] = swap( rlpol(ii,jdx), rlpol(sidx,jdx));
          vals = rlpol(:,[jj,jj+1]);
        endif
      endif
    endfor
  endfor

endfunction
