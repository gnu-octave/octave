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
## @deftypefn {Function File} {[@var{wmin}, @var{wmax}] =} bode_bounds (@var{zer}, @var{pol}, @var{dflg}, @var{tsam})
## Get default range of frequencies based on cutoff frequencies of system
## poles and zeros.
## Frequency range is the interval
## @iftex
## @tex
## $ [ 10^{w_{min}}, 10^{w_{max}} ] $
## @end tex
## @end iftex
## @ifinfo
## [10^@var{wmin}, 10^@var{wmax}]
## @end ifinfo
##
## Used internally in @command{__freqresp__} (@command{bode}, @command{nyquist})
## @end deftypefn

function [wmin, wmax] = bode_bounds (zer, pol, DIGITAL, tsam)

  ## make sure zer,pol are row vectors
  if(!isempty(pol)) pol = reshape(pol,1,length(pol)); endif
  if(!isempty(zer)) zer = reshape(zer,1,length(zer)); endif

  ## check for natural frequencies away from omega = 0
  if (DIGITAL)
    ## The 2nd conditions prevents log(0) in the next log command
    iiz = find(abs(zer - 1) > norm(zer) * eps && abs(zer) > norm(zer) * eps);
    iip = find(abs(pol - 1) > norm(pol) * eps && abs(pol) > norm(pol) * eps);

    ## avoid dividing empty matrices, it would work but looks nasty
    if (!isempty(iiz)) czer = log(zer(iiz))/tsam;
    else               czer = [];                 endif

    if (!isempty(iip)) cpol = log(pol(iip))/tsam;
    else               cpol = [];                 endif

  else
    ## continuous
    iip = find((abs(pol)) > (norm(pol) * eps));
    iiz = find((abs(zer)) > (norm(zer) * eps));

    if(!isempty(zer)) czer = zer(iiz);
    else              czer = [];                endif
    if(!isempty(pol)) cpol = pol(iip);
    else              cpol = [];                endif
  endif

  if(max(size(iip)) + max(size(iiz)) )
    wmin = floor(log10(min(abs([cpol,czer]))));
    wmax = ceil(log10(max(abs([cpol,czer]))));
  else
    ## no poles/zeros away from omega = 0; pick defaults
    wmin = -1;
    wmax = 3;
  endif

  ## expand to show the entirety of the "interesting" portion of the plot
  wmin--;
  wmax++;

  ## run digital frequency all the way to pi
  if (DIGITAL) wmax = log10(pi/tsam); endif

endfunction
