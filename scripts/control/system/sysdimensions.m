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
## @deftypefn {Function File} {[@var{n}, @var{nz}, @var{m}, @var{p}, @var{yd}] =} sysdimensions (@var{sys}, @var{opt})
## return the number of states, inputs, and/or outputs in the system
## @var{sys}.
##
## @strong{Inputs}
## @table @var
## @item sys
## system data structure
##
## @item opt
## String indicating which dimensions are desired.  Values:
## @table @code
## @item "all"
## (default) return all parameters as specified under Outputs below.
##
## @item "cst"
## return @var{n}= number of continuous states
##
## @item "dst"
## return @var{n}= number of discrete states
##
## @item "in"
## return @var{n}= number of inputs
##
## @item "out"
## return @var{n}= number of outputs
## @end table
## @end table
##
## @strong{Outputs}
## @table @var
## @item  n
##  number of continuous states (or individual requested dimension as specified
## by @var{opt}).
## @item  nz
##  number of discrete states
## @item  m
##  number of system inputs
## @item  p
##  number of system outputs
## @item  yd
##  binary vector; @var{yd}(@var{ii}) is nonzero if output @var{ii} is
## discrete.
## @math{yd(ii) = 0} if output @var{ii} is continuous
## @end table
## @seealso{sysgetsignals, sysgettsam}
## @end deftypefn

function [n, nz, m, p, yd] = sysdimensions (sys, opt)

  if(nargout > 5 | nargin < 1 | nargin > 2)
    print_usage ();
  elseif(!isstruct(sys))
    print_usage ();
  elseif(nargin == 1)
    opt = "all";
  endif

  n = sys.n;
  nz = sys.nz;
  m = length(sysgetsignals(sys,"in"));
  p = length(sysgetsignals(sys,"out"));
  yd = sys.yd;
  valid_options = {"all","cst","dst","st","in","out"};
  valid_values = {n,n,nz,n+nz,m,p};

  valid_opt = 0;
  for ii=1:length(valid_options)
    if(strcmp(valid_options{ii},opt))
      n = valid_values{ii};
      valid_opt = 1;
      if(ii > 1 & nargout > 1)
        warning("opt=%s, %d output arguments requested",opt,nargout);
      endif
    endif
  endfor

  if (! valid_opt)
    error ("invalid option passed = %s", opt);
  endif

endfunction
