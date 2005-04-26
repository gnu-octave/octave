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
## @deftypefn {Function File} {} syssetsignals (@var{sys}, @var{opt}, @var{names}, @var{sig_idx})
## change the names of selected inputs, outputs and states.
##
## @strong{Inputs}
## @table @var
## @item sys
## System data structure.
##
## @item opt
## Change default name (output).
##
## @table @code
## @item "out"
## Change selected output names.
## @item "in"
## Change selected input names.
## @item "st"
## Change selected state names.
## @item "yd"
## Change selected outputs from discrete to continuous or
## from continuous to discrete.
## @end table
##
## @item names
## @table @code
## @item opt = "out", "in", "st"
## string or string array containing desired signal names or values.
## @item opt = "yd"
## To desired output continuous/discrete flag.
## Set name to 0 for continuous, or 1 for discrete.
## @end table
## @item sig_idx
## indices or names of outputs, yd, inputs, or
## states whose respective names/values should be changed.
##
## Default: replace entire cell array of names/entire yd vector.
## @end table
##
## @strong{Outputs}
## @table @var
## @item retsys
## @var{sys} with appropriate signal names changed
## (or @var{yd} values, where appropriate).
## @end table
##
## @strong{Example}
## @example
## octave:1> sys=ss([1 2; 3 4],[5;6],[7 8]);
## octave:2> sys = syssetsignals(sys,"st",str2mat("Posx","Velx"));
## octave:3> sysout(sys)
## Input(s)
##         1: u_1
## Output(s):
##         1: y_1
## state-space form:
## 2 continuous states, 0 discrete states
## State(s):
##         1: Posx
##         2: Velx
## A matrix: 2 x 2
##   1  2
##   3  4
## B matrix: 2 x 1
##   5
##   6
## C matrix: 1 x 2
##   7  8
## D matrix: 1 x 1
## 0
## @end example
## @end deftypefn

## Author: John Ingram <ingraje@eng.auburn.edu>
## Created: August 1996

function retsys = syssetsignals (sys, opt, names, sig_idx)

  if (nargin < 3 | nargin > 4)
    usage("retsys=syssetsignals(sys,opt,names{,sig_idx})");
  elseif (!isstruct(sys))
    error("sys must be a system data structure");
  elseif (isempty(opt))
    opt = "out";
  elseif( ! isstr(opt)  )
    error("opt must be a string");
  elseif( ! (strcmp(opt,"out") + strcmp(opt,"yd") + ...
    strcmp(opt,"in") + strcmp(opt,"st") ) )
    error("opt must be one of [], ""out"", ""yd"", ""in"", or ""st""");
  elseif(nargin == 4)
    if(is_signal_list(sig_idx) | isstr(sig_idx))
      ## convert to vector of indices
      if(opt == "yd")
        sig_idx = sysidx(sys,"out",sig_idx);
      else
        sig_idx = sysidx(sys,opt,sig_idx);
      endif
    endif

    ## check index vector
    if(min(size(sig_idx)) > 1)
      disp("syssetsignals: sig_idx=")
      disp(sig_idx);
      error("sig_idx must be a vector")
    endif
  endif

  sig_vals = sysgetsignals(sys,opt);

  ## make sure it's in state space form if state names are given
  if(strcmp(opt,"st"))
    sys = sysupdate(sys,"ss");
  endif

  if(strcmp(opt,"yd") == 0)
    ## it's a signal name list we're changing
    if(!iscell(names))
      names = {names};
    endif
    if( (!is_signal_list(names)) & (!isempty(names)) )
      if(isstr(names{1}))
        warning("syssetsignals(opt=%s): converting string matrix \"names\" to a cell array of strings",opt);
        tmpstr = names{1};
        for ii=1:rows(tmpstr)
          names{ii} = deblank(tmpstr(ii,:));
        endfor
      else
        names
        error("parameter \"names\" must be a cell array of strings");
      endif
    endif
    nsigs = length(sig_vals);

    if(nargin == 3)
      ## replace all signal names
      if(length(names) != nsigs)
        error("opt=%s, sig_idx omitted: names(len=%d) should have %d entries ", ...
          opt,length(names),nsigs);
      endif
      sig_idx = 1:nsigs;
    elseif(length(names) != length(sig_idx))
      ## replace specified signal names
      error("opt=%s, sig_idx(len=%d), names(len=%d) mismatch",opt, ...
        length(sig_idx), length(names));
    endif

    for ii=1:length(sig_idx)
      jj = sig_idx(ii);
      if(jj < 1 | jj > nsigs | jj != floor(jj+0.5))
        error("opt=%s, sig_idx(%d)=%d, %e: must be an integer between 1 and %d", ...
          opt, ii, jj, jj, nsigs);
      endif
      sig_vals{jj} = names{ii};
    endfor

  else
    ## update yd
    ## 1st check pathological case: no outputs
    nout = sysdimensions(sys,"out");
    if(nout == 0)
      if(nargin != 3)
        error("opt=%s, %d outputs, sysgetsignals cannot take 4 arguments", ...
          yd,nout);
      endif
      if(!isempty(names))
        error("opt=%s, %d outputs, names is not empty");
      endif
      sigvals = [];
    else
      nsigs = length(sig_vals);
      if(!isvector(names))
        error("syssetsignals: opt=yd, names(%dx%d) must be a vector", ...
          rows(names), columns(names));
      endif
      if(nargin == 3)
        if(length(names) != nsigs)
          error("opt=yd, sig_idx omitted: names(%d) should be length(%d)", ...
            length(names), nsigs);
        endif
        sig_idx = 1:nsigs;
      elseif(length(names) != length(sig_idx))
        error("opt=yd: length(names)=%d, length(sig_idx)=%d",length(names), ...
          length(sig_idx) );
      endif

      badidx = find(names != 0 & names != 1);
      if(! isempty(badidx) )
        for ii=1:length(badidx)
          warning("syssetsignals: opt=yd: names(%d)=%e, must be 0 or 1", ...
            badidx(ii), names(badidx(ii)) );
        endfor
        error ("opt=yd: invalid values in names");
      endif

      for ii=1:length(sig_idx)
        jj = sig_idx(ii);
        if(jj < 1 | jj > nsigs | jj != floor(jj))
          error("sig_idx(%d)=%d, %e: must be an integer between 1 and %d", ...
            ii,jj, jj, nsigs);
        endif
        sig_vals(jj) = names(ii);
      endfor
      if(any(sig_vals == 1) & sysgettsam(sys) == 0)
        warning("Setting system sampling time to 1");
        printf("syssetsignals: original system sampling time=0 but output(s)\n");
        disp(find(sig_vals==1))
        printf("are digital\n");
        sys = syschtsam(sys,1);
      endif

    endif
  endif

  if(strcmp(opt,"st"))
    sys.stname = sig_vals;
  elseif(strcmp(opt,"in"))
    sys.inname = sig_vals;
  elseif(strcmp(opt,"out"))
    sys.outname = sig_vals;
  elseif(strcmp(opt,"yd"))
    sys.yd = sig_vals;
  endif

  retsys = sys;

endfunction
