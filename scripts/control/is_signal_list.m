## Copyright (C) 1996,1998 Auburn University.  All Rights Reserved
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
## @deftypefn {Function File } {@var{flg} =} is_signal_list (@var{mylist})
## Returns true if mylist is a list of individual strings (legal for input
## to @var{syssetsignals}).
## @end deftypefn

function flg = is_signal_list(mylist)
flg = is_list(mylist);
if(flg)
  for ii=1:length(mylist)
    if(!(isstr(nth(mylist,ii)) & rows(nth(mylist,ii)) ==1) )
      flg = 0;
    endif
  endfor
endif

endfunction
