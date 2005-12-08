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
## @deftypefn {Function File} {[@var{zer}, @var{pol}] =} pzmap (@var{sys})
## Plots the zeros and poles of a system in the complex plane.
##
## @strong{Input}
## @table @var
## @item sys
## System data structure.
## @end table
##
## @strong{Outputs}
## @table @var
## @item pol
## @item zer
## if omitted, the poles and zeros are plotted on the screen.
## otherwise, @var{pol} and @var{zer} are returned as the 
## system poles and zeros (see @command{sys2zp} for a preferable function call).
## @end table
## @end deftypefn

function [zer, pol]=pzmap (sys)

  if(nargin != 1)
    usage("pzmap(sys) or [zer,pol] = pzmap(sys)");
  elseif (!isstruct(sys));
    error("sys must be in system format");
  endif

  [zer,pol] = sys2zp(sys);

  ## force to column vectors, split into real, imaginary parts
  zerdata = poldata = [];
  if(length(zer))
    zer = reshape(zer,length(zer),1);
    zerdata = [real(zer(:,1)), imag(zer(:,1))];
  endif
  if(length(pol))
    pol = reshape(pol,length(pol),1);
    poldata = [real(pol(:,1)), imag(pol(:,1))];
  endif

  ## determine continuous or discrete plane
  vars = "sz";
  varstr = vars(is_digital(sys) + 1);

  ## Plot the data
  __gnuplot_set__ nologscale xy;
  if(is_siso(sys))
    title(sprintf("Pole-zero map from %s to %s", ...
	 sysgetsignals(sys,"in",1,1), sysgetsignals(sys,"out",1,1) ));
  endif
  xlabel(["Re(",varstr,")"]);
  ylabel(["Im(",varstr,")"]);
  grid;

  ## compute axis limits
  axis(axis2dlim([zerdata;poldata]));
  grid
  ## finally, plot the data
  if(length(zer) == 0)
    plot(poldata(:,1), poldata(:,2),"@12 ;poles (no zeros);");
  elseif(length(pol) == 0)
    plot(zerdata(:,1), zerdata(:,2),"@31 ;zeros (no poles);");
  else
    plot(zerdata(:,1), zerdata(:,2),"@31 ;zeros;", ...
	poldata(:,1), poldata(:,2),"@12 ;poles;");
  endif
  replot

endfunction
