## Copyright (C) 1996, 1998, 2000, 2004 Auburn University.  All rights reserved.
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
## Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} is_signal_list (@var{mylist})
## Return true if @var{mylist} is a list of individual strings.
## @end deftypefn

function flg = is_signal_list (mylist)

  flg = iscell (mylist);
  if(flg)
    flg = (rows(mylist) == 1 | columns(mylist) == 1);
  end
  if (flg)
    for ii = 1:length (mylist)
      if (! (ischar (mylist{ii}) && rows (mylist{ii}) == 1))
	flg = 0;
      endif
    endfor
  endif

endfunction
