## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} __freqresp__ (@var{sys}, @var{USEW}, @var{w})
## Frequency response function - used internally by @command{bode}, @command{nyquist}.
## minimal argument checking; ``do not attempt to do this at home''.
##
## @strong{Inputs}
## @table @var
## @item sys
## system data structure
## @item USEW
## returned by @code{freqchkw}
## @item optional
## must be present if @var{USEW} is true (nonzero)
## @end table
## @strong{Outputs}
## @table @var
## @item @var{out}
## vector of finite @math{G(j*w)} entries (or @math{||G(j*w)||} for @acronym{MIMO})
## @item w
## vector of corresponding frequencies
## @end table
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 11, 1994

function [ff, w] = __freqresp__ (sys, USEW, w);

  ## SYS_INTERNAL accesses members of system data structure

  ## Check Args
  if ((nargin < 2) || (nargin > 4))
    print_usage ();
  elseif (USEW & (nargin < 3) )
    error ("USEW = 1 but w was not passed.");
  elseif (USEW & isempty(w))
    warning("USEW = 1 but w is empty; setting USEW=0");
    USEW = 0;
  endif

  DIGITAL = is_digital(sys);

  ## compute default w if needed
  if(!USEW)
    if(is_siso(sys))
	sys = sysupdate(sys,"zp");
	[zer,pol] = sys2zp(sys);
    else
	zer = tzero(sys);
	pol = eig(sys2ss(sys));
    endif

    ## get default frequency range
    [wmin,wmax] = bode_bounds(zer,pol,DIGITAL,sysgettsam(sys));
    w = logspace(wmin,wmax,50);
  else
    w = reshape(w,1,length(w));         # make sure it's a row vector
  endif

  ## now get complex values of s or z
  if(DIGITAL)
    jw = exp(i*w*sysgettsam(sys));
  else
    jw = i*w;
  endif

  [nn,nz,mm,pp] = sysdimensions(sys);

  ## now compute the frequency response - divide by zero yields a warning
  if (strcmp(sysgettype(sys),"zp"))
    ## zero-pole form (preferred)
    [zer,pol,sysk] = sys2zp(sys);
    ff = ones(size(jw));
    l1 = min(length(zer)*(1-isempty(zer)),length(pol)*(1-isempty(pol)));
    for ii=1:l1
	ff = ff .* (jw - zer(ii)) ./ (jw - pol(ii));
    endfor

    ## require proper  transfer function, so now just get poles.
    for ii=(l1+1):length(pol)
	ff = ff ./ (jw - pol(ii));
    endfor
    ff = ff*sysk;

  elseif (strcmp(sysgettype(sys),"tf"))
    ## transfer function form
    [num,den] = sys2tf(sys);
    ff = polyval(num,jw)./polyval(den,jw);
  elseif (mm==pp)
    ## The system is square; do state-space form bode plot
    [sysa,sysb,sysc,sysd,tsam,sysn,sysnz] = sys2ss(sys);
    n = sysn + sysnz;
    for ii=1:length(jw);
	ff(ii) = det(sysc*((jw(ii).*eye(n)-sysa)\sysb)+sysd);
    endfor;
  else
    ## Must be state space... bode
    [sysa,sysb,sysc,sysd,tsam,sysn,sysnz] = sys2ss(sys);
    n = sysn + sysnz;
    for ii=1:length(jw);
	ff(ii) = norm(sysc*((jw(ii)*eye(n)-sysa)\sysb)+sysd);
    endfor

  endif

  w = reshape(w,1,length(w));
  ff = reshape(ff,1,length(ff));

endfunction

