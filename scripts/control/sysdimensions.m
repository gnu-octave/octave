# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved.
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function [n,nz,m,p,yd] = sysdimensions(sys,opt)
# [n,nz,m,p,yd] = sysdimensions(sys{,opt})
# return the number of states, inputs, and/or outputs in the system sys.
# inputs: sys: system data structure
#         opt: string
#              "all" (default): return all output arguments (see below)
#              "cst": return n=number of continuous states
#              "dst": return n=number of discrete states
#              "st":  return n=number of states (continuous and discrete)
#              "in":  return n=number of inputs
#              "out": return n=number of outputs
# outputs:
#  n: number of continuous states (or the specified dimension as shown above)
#  nz: number of discrete states
#  m: number of system inputs
#  p: number of system outputs
#  yd: is the discrete output vector: yd(ii) = 1 if output ii is sampled,
#   				    yd(ii) = 0 if output ii is continous
#
# see also: sysgetsignals, sysgettsam

if(nargout > 5 | nargin < 1 | nargin > 2)
  usage("[n,nz,m,p[,yd]] = sysdimensions(sys{,opt})");
elseif(!is_struct(sys))
  usage("[n,nz,m,p] = sysdimensions(sys)");
elseif(nargin == 1)
  opt = "all";
endif

n = sys.n;
nz = sys.nz;
m = length(sysgetsignals(sys,"in"));
p = length(sysgetsignals(sys,"out"));
yd = sys.yd;
legal_options = list("all","cst","dst","st","in","out");
legal_values = list(n,n,nz,n+nz,m,p);

legal_opt = 0;
for ii=1:length(legal_options)
  if(strcmp(nth(legal_options,ii),opt))
    n = nth(legal_values,ii);
    legal_opt = 1;
    if(ii > 1 & nargout > 1)
      warning("opt=%s, %d output arguments requested",opt,nargout);
    endif
  endif
endfor
if(!legal_opt)
  error("illegal option passed = %s",opt);
endif

endfunction
