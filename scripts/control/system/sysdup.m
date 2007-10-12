## Copyright (C) 1996, 1998, 2000, 2002, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {@var{retsys} =} sysdup (@var{asys}, @var{out_idx}, @var{in_idx})
## Duplicate specified input/output connections of a system
##
## @strong{Inputs}
## @table @var
## @item asys
## system data structure
## @item out_idx
## @itemx in_idx
## indices or names of desired signals (see @code{sigidx}).
## duplicates are made of @code{y(out_idx(ii))} and @code{u(in_idx(ii))}.
## @end table
##
## @strong{Output}
## @table @var
## @item retsys
## Resulting closed loop system:
## duplicated i/o names are appended with a @code{"+"} suffix.
## @end table
##
## @strong{Method}
##
## @code{sysdup} creates copies of selected inputs and outputs as
## shown below.  @var{u1}, @var{y1} is the set of original inputs/outputs, and
## @var{u2}, @var{y2} is the set of duplicated inputs/outputs in the order 
## specified in @var{in_idx}, @var{out_idx}, respectively
## @example
## @group
##           ____________________
## u1  ----->|                  |----> y1
##           |       asys       |
## u2 ------>|                  |----->y2
## (in_idx)  -------------------- (out_idx)
## @end group
## @end example
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## modified by John Ingram July 1996

function retsys = sysdup (Asys, output_list, input_list)

  if( nargin != 3)
    print_usage ();
  endif

  if( !isstruct(Asys))
    error("Asys must be a system data structure (see ss, tf, or zp)")
  endif

  Asys = sysupdate(Asys,"ss");
  [nn,nz,mm,pp] = sysdimensions(Asys);
  [aa,bb,cc,dd] = sys2ss(Asys);

  ## check for signal names
  if(is_signal_list(input_list) | ischar(input_list))
    input_list = sysidx(Asys,"in",input_list);
  endif
  if(is_signal_list(output_list) | ischar(output_list))
    output_list = sysidx(Asys,"out",output_list);
  endif

  ## first duplicate inputs
  if(isvector(input_list))
    for ii=1:length(input_list);
      bb(:,mm+ii) = bb(:,input_list(ii));
      dd(:,mm+ii) = dd(:,input_list(ii));
    end
  elseif(!isempty(input_list))
    error("input_list must be a vector or empty");
  endif


  ## now duplicate outputs
  osize = min(size(output_list));
  if(osize == 1)
    for ii=1:length(output_list);
      cc(pp+ii,:) = cc(output_list(ii),:);
      dd(pp+ii,:) = dd(output_list(ii),:);
    end
  elseif(osize != 0)
    error("output_list must be a vector or empty");
  endif

  [stnam,innam,outnam,yd] = sysgetsignals(Asys);
  tsam = sysgettsam(Asys);

  ## pack system and then rename signals
  retsys = ss(aa,bb,cc,dd,tsam,nn,nz);
  retsys = syssetsignals(retsys,"in",innam,1:mm);
  retsys = syssetsignals(retsys,"out",outnam,1:pp);
  retsys = syssetsignals(retsys,"yd",yd,1:pp);

  ## update added input names
  for ii=(mm+1):(mm+length(input_list))
    onum = input_list(ii-mm);
    strval = sprintf("%s(dup)",sysgetsignals(retsys,"in",onum,1) );
    retsys = syssetsignals(retsys,"in",strval,ii);
  endfor

  ## update added output names/discrete flags
  ## give default names to the added outputs
  for jj=(pp+1):(pp+length(output_list))
    onum = output_list(jj-pp);
    strval = sprintf("%s(dup)",sysgetsignals(retsys,"out",onum,1) );
    retsys = syssetsignals(retsys,"out",strval,jj);
    dflg = sysgetsignals(retsys,"yd",onum);
    retsys = syssetsignals(retsys,"yd",dflg,jj);
  endfor

endfunction
