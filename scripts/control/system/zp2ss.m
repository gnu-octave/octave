## Copyright (C) 1996, 2000, 2002, 2003, 2004, 2005, 2007
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
## @deftypefn {Function File} {[@var{a}, @var{b}, @var{c}, @var{d}] =} zp2ss (@var{zer}, @var{pol}, @var{k})
## Conversion from zero / pole to state space.
##
## @strong{Inputs}
## @table @var
## @item zer
## @itemx pol
## Vectors of (possibly) complex poles and zeros of a transfer
## function. Complex values must come in conjugate pairs
## (i.e., @math{x+jy} in @var{zer} means that @math{x-jy} is also in @var{zer}).
## The number of zeros must not exceed the number of poles.
## @item k
## Real scalar (leading coefficient).
## @end table
##
## @strong{Outputs}
## @table @var
## @item @var{a}
## @itemx @var{b}
## @itemx @var{c}
## @itemx @var{d}
## The state space system, in the form:
## @iftex
## @tex
## $$ \dot x = Ax + Bu $$
## $$ y = Cx + Du $$
## @end tex
## @end iftex
## @ifinfo
## @example
##      .
##      x = Ax + Bu
##      y = Cx + Du
## @end example
## @end ifinfo
## @end table
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994

function [a, b, c, d] = zp2ss (zer, pol, k)

    if(nargin != 3)
      error("Incorrect number of input arguments");
    endif

    if(! (isvector(zer) | isempty(zer)) )
      error(["zer(",num2str(rows(zer)),",",num2str(columns(zer)), ...
	  ") should be a vector"]);
    elseif(! (isvector(pol) | isempty(pol) ) )
      error(["pol(",num2str(rows(pol)),",",num2str(columns(pol)), ...
	  ") should be a vector"]);
    elseif(! isscalar(k))
      error(["k(",num2str(rows(k)),",",num2str(columns(k)), ...
	  ") should be a scalar"]);
    elseif( k != real(k))
      warning("zp2ss: k is complex")
    endif

    zpsys = ss (zeros (0, 0), zeros (0, 1), zeros (1, 0), k);

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
	  [num, zer] = __zp2ssg2__ (zer);       # get two zeros
	elseif(length(zer) == 1)
	  num = [1, -zer];                # use last zero (better be real!)
	  zer = [];
	endif
	[den, pol] = __zp2ssg2__ (pol);         # get two poles
      otherwise
	error(["pcnt = ",num2str(pcnt)])
      endswitch

      ## pack tf into system form and put in series with earlier realization
      zpsys1 = tf(num,den,0,"u","yy");

      ## change names to avoid warning messages from sysgroup
      zpsys  = syssetsignals (zpsys, "in", "u1", 1);
      zpsys1 = sysupdate (zpsys1, "ss");
      nn     = sysdimensions (zpsys);        # working with continuous system
      zpsys  = syssetsignals (zpsys, "st", __sysdefioname__ (nn, "x"));
      nn1    = sysdimensions (zpsys1);
      zpsys1 = syssetsignals (zpsys1, "st", __sysdefioname__ (nn1, "xx"));

      zpsys = sysmult(zpsys,zpsys1);

    endwhile

    [a,b,c,d] = sys2ss(zpsys);

endfunction

