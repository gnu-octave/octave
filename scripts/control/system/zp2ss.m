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
## @deftypefn {Function File} {[@var{A}, @var{B}, @var{C}, @var{D}] =} zp2ss (@var{zer}, @var{pol}, @var{k})
## Conversion from zero / pole to state space.
## @strong{Inputs}
## @table @var
## @item zer
## @itemx pol
## vectors of (possibly) complex poles and zeros of a transfer
## function.  Complex values must come in conjugate pairs
## (i.e., x+jy in zer means that x-jy is also in zer)
## @item k
## real scalar (leading coefficient)
## @end table
## @strong{Outputs}
##  @var{A}, @var{B}, @var{C}, @var{D}
## The state space system
## @example
## .
## x = Ax + Bu
## y = Cx + Du
## @end example
## is obtained from a vector of zeros and a vector of poles via the
## function call @code{[a,b,c,d] = zp2ss(zer,pol,k)}.
## The vectors @samp{zer} and
## @samp{pol} may either be row or column vectors.  Each zero and pole that
## has an imaginary part must have a conjugate in the list.
## The number of zeros must not exceed the number of poles.
## @samp{k} is @code{zp}-form leading coefficient.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994

function [a, b, c, d] = zp2ss (zer, pol, k)

  sav_val = empty_list_elements_ok;
  empty_list_elements_ok = 1;

  if(nargin != 3)
    error("Incorrect number of input arguments");
  endif

  if(! (is_vector(zer) | isempty(zer)) )
    error(["zer(",num2str(rows(zer)),",",num2str(columns(zer)), ...
        ") should be a vector"]);
  elseif(! (is_vector(pol) | isempty(pol) ) )
    error(["pol(",num2str(rows(pol)),",",num2str(columns(pol)), ...
        ") should be a vector"]);
  elseif(! is_scalar(k))
    error(["k(",num2str(rows(k)),",",num2str(columns(k)), ...
        ") should be a scalar"]);
  elseif( k != real(k))
    warning("zp2ss: k is complex")
  endif

  zpsys = ss2sys([],[],[],k);

  ## Find the number of zeros and the number of poles
  nzer=length(zer);
  npol =length(pol);

  if(nzer > npol)
    error([num2str(nzer)," zeros, exceeds number of poles=",num2str(npol)]);
  endif

  ## Sort to place complex conjugate pairs together
  zer=sortcom(zer);
  pol=sortcom(pol);

  ## construct the system as a series connection of poles and zeros
  ## problem: poles and zeros may come in conjugate pairs, and not
  ## matched up!

  ## approach: remove poles/zeros from the list as they are included in
  ## the ss system

  while(length(pol))

    ## search for complex poles, zeros
    cpol=[];    czer = [];
    if(!isempty(pol))
      cpol = find(imag(pol) != 0);
    endif
    if(!isempty(zer))
      czer = find(imag(zer) != 0);
    endif

    if(isempty(cpol) & isempty(czer))
      pcnt = 1;
    else
      pcnt = 2;
    endif

    num=1;      # assume no zeros left.
    switch(pcnt)
    case(1)
      ## real pole/zero combination
      if(length(zer))
        num = [1, -zer(1)];
        zer = zer(2:length(zer));
      endif
      den = [1, -pol(1)];
      pol = pol(2:length(pol));
    case(2)
      ## got a complex pole or zero, need two roots (if available)
      if(length(zer) > 1)
        [num,zer] = zp2ssg2(zer);       # get two zeros
      elseif(length(zer) == 1)
        num = [1, -zer];                # use last zero (better be real!)
        zer = [];
      endif
      [den,pol] = zp2ssg2(pol);         # get two poles
    otherwise
      error(["pcnt = ",num2str(pcnt)])
    endswitch

    ## pack tf into system form and put in series with earlier realization
    zpsys1 = tf2sys(num,den,0,"u","yy");

    ## change names to avoid warning messages from sysgroup
    zpsys  = syssetsignals(zpsys,"in","u1",1);
    zpsys1 = sysupdate(zpsys1,"ss");
    nn     = sysdimensions(zpsys);        # working with continuous system
    zpsys  = syssetsignals(zpsys,"st", sysdefioname(nn,"x"));
    nn1    = sysdimensions(zpsys1);
    zpsys1 = syssetsignals(zpsys1,"st",sysdefioname(nn1,"xx"));

    zpsys = sysmult(zpsys,zpsys1);

  endwhile

  [a,b,c,d] = sys2ss(zpsys);

  empty_list_elements_ok = sav_val;
endfunction

