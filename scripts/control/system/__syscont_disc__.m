## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{n_tot}, @var{st_c}, @var{st_d}, @var{y_c}, @var{y_d}] =} __syscont_disc__ (@var{sys})
## Used internally in syscont and sysdisc.
##
## @strong{Inputs}
## @var{sys} is a system data structure.
##
## @strong{Outputs}
## @table @var
## @item n_tot
## total number of states
## @item st_c
## vector of continuous state indices (empty if none)
## @item st_d
## vector of discrete state indices (empty if none)
## @item y_c
## vector of continuous output indices
## @item y_d
## vector of discrete output indices
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: February 1997

function [n_tot, st_c, st_d, y_c, y_d] = __syscont_disc__ (sys)

  ## get ranges for discrete/continuous states and outputs
  [nn,nz,mm,pp,yd] = sysdimensions(sys);
  n_tot = nn + nz;
  st_c = 1:(nn);
  st_d = nn + (1:nz);
  y_c = find(yd == 0);          # y_c, y_d will be empty if there are none.
  y_d = find(yd == 1);

endfunction
