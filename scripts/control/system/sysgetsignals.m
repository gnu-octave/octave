## Copyright (C) 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{stname}, @var{inname}, @var{outname}, @var{yd}] =} sysgetsignals (@var{sys})
## @deftypefnx{Function File} {@var{siglist} =} sysgetsignals (@var{sys}, @var{sigid})
## @deftypefnx{Function File} {@var{signame} =} sysgetsignals (@var{sys}, @var{sigid}, @var{signum}, @var{strflg})
## Get signal names from a system
##
## @strong{Inputs}
## @table @var
## @item sys
## system data structure for the state space system
##
## @item sigid
## signal id.  String.  Must be one of
## @table @code
## @item "in"
## input signals
## @item "out"
## output signals
## @item "st"
## stage signals
## @item "yd"
## value of logical vector @var{yd}
## @end table
##
## @item signum
## index(indices) or name(s) or signals; see @code{sysidx}
##
## @item strflg
## flag to return a string instead of a cell array;  Values:
## @table @code
## @item 0
## (default) return a cell array (even if signum specifies an individual signal)
##
## @item 1
## return a string.  Exits with an error if signum does not specify an 
## individual signal.
## @end table
##
## @end table
##
## @strong{Outputs}
## @table @bullet
## @item If @var{sigid} is not specified:
## @table @var
## @item stname
## @itemx inname
## @itemx outname
## signal names (cell array of strings);  names of states,
## inputs, and outputs, respectively.
## @item yd
## binary vector; @var{yd}(@var{ii}) is nonzero if output @var{ii} is
## discrete.
## @end table
##
## @item If @var{sigid} is specified but @var{signum} is not specified:
## @table @code
## @item sigid="in"
## @var{siglist} is set to the cell array of input names.
##
## @item sigid="out"
## @var{siglist} is set to the cell array of output names.
##
## @item sigid="st"
## @var{siglist} is set to the cell array of state names.
##
## stage signals
## @item sigid="yd"
## @var{siglist} is set to logical vector indicating discrete outputs;
## @var{siglist}(@var{ii}) = 0 indicates that output @var{ii} is continuous
## (unsampled), otherwise it is discrete.
##
## @end table
##
## @item If the first three input arguments are specified:
## @var{signame} is a cell array of the specified signal names (@var{sigid} is 
## @code{"in"}, @code{"out"}, or @code{"st"}), or else the logical flag
## indicating whether output(s) @var{signum} is(are) discrete (@var{sigval}=1)
## or continuous (@var{sigval}=0).
## @end table
##
## @strong{Examples} (From @code{sysrepdemo})
## @example
## octave> sys=ss(rand(4),rand(4,2),rand(3,4));
## octave># get all signal names
## octave> [Ast,Ain,Aout,Ayd] = sysgetsignals(sys)
## Ast =
## (
##   [1] = x_1
##   [2] = x_2
##   [3] = x_3
##   [4] = x_4
## )
## Ain =
## (
##   [1] = u_1
##   [2] = u_2
## )
## Aout =
## (
##   [1] = y_1
##   [2] = y_2
##   [3] = y_3
## )
## Ayd =
##
##   0  0  0
## octave> # get only input signal names:
## octave> Ain = sysgetsignals(sys,"in")
## Ain =
## (
##   [1] = u_1
##   [2] = u_2
## )
## octave> # get name of output 2 (in cell array):
## octave> Aout = sysgetsignals(sys,"out",2)
## Aout =
## (
##   [1] = y_2
## )
## octave> # get name of output 2 (as string):
## octave> Aout = sysgetsignals(sys,"out",2,1)
## Aout = y_2
## @end example
## @end deftypefn

function [stname, inname, outname, yd] = sysgetsignals (sys, sigid, signum, strflg)

  ## Adapted from ss

  if(nargin < 1 | nargin > 4 | nargout > 4)
    usage("[stname{,inname,outname,yd}] = sysgetsignals(sys{,sigid,signum})")
  elseif(nargin > 1 & nargout > 1)
    usage("sig = sysgetsignals(sys,sigid{,signum,strflg})")
  elseif( ! isstruct(sys) )
    error("input argument must be a system data structure");
  endif
  if(nargin < 4)  strflg = 0; endif
  if(nargin == 1)
    sys = sysupdate(sys,"ss");          #make sure ss is up to date
    stname = sysgetsignals(sys,"st");
    inname = sysgetsignals(sys,"in");
    outname = sysgetsignals(sys,"out");
    yd = sysgetsignals(sys,"yd");
  elseif(!(isstr(sigid) & min(size(sigid)) == 1))
    error(sprintf("sigid(%dx%d) must be a string)",rows(sigid),columns(sigid)));
  else
    if(strcmp("st",sigid))         stname = sys.stname;
    elseif(strcmp("in",sigid))     stname = sys.inname;
    elseif(strcmp("out",sigid))    stname = sys.outname;
    elseif(strcmp("yd",sigid))     stname = vec(sys.yd)';
    else
      error(sprintf("sigid=%s must be \"st\", \"in\", \"out\", or \"yd\"", ...
        sigid));
    endif
    if(nargin >= 3)
      if( is_signal_list(signum) | isstr(signum) )
        signum = cellidx(stname,signum);
      end
      if(max(signum) > length(stname))
        error(sprintf("sysgetsignals(sys,\"%s\",%d):only %d entries.\n", ...
          sigid,signum, rows(stname)));
      else
        if(!isscalar(strflg))
          error("strflg must be a scalar");
        endif
        switch(strflg)
        case(0),
          stname = stname(signum);
        case(1),
          if(length(signum) > 1)
            error("strflg=1, length(signum) = %d",length(signum));
          endif
          stname = stname{signum};
        otherwise,
          error ("invalid value of strflg = %e", strflg);
        endswitch

      endif
    endif
  endif

endfunction

