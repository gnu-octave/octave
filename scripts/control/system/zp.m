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

## -*- texinfo -*-
## @deftypefn {Function File} {} zp (@var{zer}, @var{pol}, @var{k}, @var{tsam}, @var{inname}, @var{outname})
## Create system data structure from zero-pole data.
##
## @strong{Inputs}
## @table @var
## @item   zer
## vector of system zeros
## @item   pol
## vector of system poles
## @item   k
## scalar leading coefficient
## @item   tsam
## sampling period. default: 0 (continuous system)
## @item   inname
## @itemx  outname
## input/output signal names (lists of strings)
## @end table
##
## @strong{Outputs}
## sys: system data structure
##
## @strong{Example}
## @example
## octave:1> sys=zp([1 -1],[-2 -2 0],1);
## octave:2> sysout(sys)
## Input(s)
##         1: u_1
## Output(s):
##         1: y_1
## zero-pole form:
## 1 (s - 1) (s + 1)
## -----------------
## s (s + 2) (s + 2)
## @end example
## @end deftypefn

## Modified by John Ingram  July 20, 1996

function outsys = zp (zer, pol, k, tsam, inname, outname)

  ## Test for the correct number of input arguments
  if ((nargin < 3) || (nargin > 6))
    print_usage ();
  endif

  ## check input format
  if( ! (isvector(zer) | isempty(zer) ) )
    error("zer must be a vector or empty");
  endif
  if(!isempty(zer))
    zer = reshape(zer,1,length(zer));           # make it a row vector
  endif

  if( ! (isvector(pol) | isempty(pol)))
    error("pol must be a vector");
  endif
  if(!isempty(pol))
    pol = reshape(pol,1,length(pol));
  endif

  if (! isscalar(k))
     error("k must be a scalar");
  endif

  ## Test proper numbers of poles and zeros.  The number of poles must be
  ## greater than or equal to the number of zeros.
  if (length(zer) >  length(pol))
    error(["number of poles (", num2str(length(pol)), ...
        ") < number of zeros (", num2str(length(zer)),")"]);
  endif

  ## Set the system transfer function
  outsys.zer = zer;
  outsys.pol = pol;
  outsys.k = k;

  ## Set the system vector:  active = 1, updated = [0 1 0];
  outsys.sys = [1, 0, 1, 0];

  ## Set defaults
  outsys.tsam = 0;
  outsys.n = length(pol);
  outsys.nz = 0;
  outsys.yd = 0;        # assume (for now) continuous time outputs

  ## Set the type of system
  if (nargin > 3)
    if( !isscalar(tsam) )
      error("tsam must be a nonnegative scalar");
    endif
    if (tsam < 0)
      error("sampling time must be positve")
    elseif (tsam > 0)
      [outsys.n,outsys.nz] = swap(outsys.n, outsys.nz);
      outsys.yd = 1;            # discrete-time output
    endif

    outsys.tsam = tsam;
  endif

  outsys.inname = __sysdefioname__ (1, "u");
  outsys.outname = __sysdefioname__ (1, "y");
  outsys.stname = __sysdefstname__ (outsys.n, outsys.nz);

  ## Set name of input
  if (nargin > 4)
    ## make sure its a string
    if(!isempty(inname))
      if(!iscell(inname))
        inname = {inname}; 
      endif
      if(!is_signal_list(inname))
        error("inname must be a single signal name");
      endif
      outsys.inname = inname(1);
    endif
  endif

  ## Set name of output
  if (nargin > 5)
    if(!isempty(outname))
      if(!iscell(outname))
        outname = {outname};
      endif
      if(!is_signal_list(outname))
        error("outname must be a single signal name");
      endif
      outsys.outname = outname(1);
    endif
  endif

endfunction
