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
## @deftypefn {Function File} {[@var{systype}, @var{nout}, @var{nin}, @var{ncstates}, @var{ndstates}] =} minfo (@var{inmat})
## Determines the type of system matrix.  @var{inmat} can be a varying, 
## a system, a constant, and an empty matrix.
##
## @strong{Outputs}
## @table @var
## @item systype 
## Can be one of: varying, system, constant, and empty.
## @item nout 
## The number of outputs of the system.
## @item nin
## The number of inputs of the system.
## @item ncstates
## The number of continuous states of the system.
## @item ndstates 
## The number of discrete states of the system.
## @end table
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 29, 1994
## Modified by David Clem November 13, 1994
## Modified by A. S. Hodel July 1995

function [systype, nout, nin, ncstates, ndstates] = minfo (inmat)

  warning("minfo: obsolete.  Use sys2ss, sys2tf, or sys2zp.");

  if (nargin ~= 1 )
    disp ("MINFO: Wrong number of arguments")
    systype = nout = nin = ncstates = ndstates = [];
  endif

  [rr,cc] = size(inmat);

  ## Check for empty matrix first!
  if (isempty(inmat))
    systype = "empty";
    nout = nin = ncstates = ndstates = 0;
    return

  ## Check for Constant matrix

  elseif (rr == 1 || cc == 1)
    systype = "constant";
    nout = nin = ncstates = ndstates = 1;
    return

  ## Check for system type matrix
  elseif (inmat(rr,cc) == -Inf)
    systype = "system";
    ncstates = inmat(1,cc);
    ndstates = inmat(rr,1);
    nstates = ncstates + ndstates;
    nout = rr - nstates - 1;
    nin = cc - nstates - 1;

  ## Check for Varying type matrix
  elseif (inmat(rr,cc) == Inf)
    systype = "varying";
    npoints = inmat(rr,cc-1);
    nin = cc - 1;
    nout = rr / npoints;
    nstates = 0;

    ## Must be a standard matrix
  else
    systype = "constant";
    nin = cc;
    nout = rr;
    ncstates = 0;
    ndstates = 0;
  endif
endfunction
