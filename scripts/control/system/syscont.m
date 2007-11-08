## Copyright (C) 1996, 2000, 2002, 2003, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {[@var{csys}, @var{acd}, @var{ccd}] =} syscont (@var{sys})
## Extract the purely continuous subsystem of an input system.
##
## @strong{Input}
## @table @var
## @item sys
## system data structure.
## @end table
##
## @strong{Outputs}
## @table @var
## @item csys
## is the purely continuous input/output connections of @var{sys}
## @item acd
## @itemx ccd
## connections from discrete states to continuous states,
## discrete states to continuous outputs, respectively.
##
## If no continuous path exists, @var{csys} will be empty.
## @end table
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: August 1996

function [csys, Acd, Ccd] = syscont (sys)

  if (nargin != 1)
    print_usage ();
  elseif (! isstruct (sys))
    error ("sys must be in system data structure form");
  endif

  sys = sysupdate (sys, "ss");
  [n_tot, st_c, st_d, y_c, y_d] = __syscont_disc__ (sys);        # get ranges

  ## assume there's nothing there; build partitions as appropriate
  Acc = Acd = Bcc = Ccc = Ccd = Dcc = [];

  if (isempty (st_c) && isempty (y_c))
    error ("syscont: expecting continuous states and/or continuous outputs");
  elseif (isempty (st_c))
    warning ("syscont: no continuous states");
  elseif (isempty (y_c))
    warning ("syscont: no continuous outputs");
  endif

  [sys_a, sys_b, sys_c, sys_d ] = sys2ss (sys);
  [sys_stname, sys_inname, sys_outname] = sysgetsignals (sys);
  [sys_n, sys_nz, sys_m, sys_p] = sysdimensions (sys);
  if (! isempty (st_c))
    Acc = sys_a(st_c,st_c);
    stname = sys_stname(st_c);
    Bcc = sys_b(st_c,:);
    Ccc = sys_c(y_c,st_c);
    Acd = sys_a(st_c,st_d);
  else
    stname = [];
  endif
  outname = sys_outname(y_c);
  Dcc = sys_d(y_c,:);
  Ccd = sys_c(y_c,st_d);
  inname = sys_inname;

  csys = ss (Acc, Bcc, Ccc, Dcc, 0, sys_n, 0, stname, inname, outname);

endfunction
