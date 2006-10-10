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
## @deftypefn {Function File} {} sysidx (@var{sys}, @var{sigtype}, @var{signamelist})
## Return indices of signals with specified signal names
## inputs given a system data structure @var{sys}, a signal type to be
## selected @var{sigtype} (@code{"in"}, @code{"out"}, @code{"st"}), and
## a list of desired signal names @var{signamelist}.
## @end deftypefn

function idxvec = sysidx (sys, sigtype, signamelist)

  if (nargin != 3)
    print_usage ();
  elseif (! isstruct (sys))
    error ("sys must be a system data structure");
  elseif (! ischar (sigtype))
    error ("sigtype must be a string");
  elseif (rows (sigtype) != 1)
    [nr, nc] = size (sigtype);
    error ("sigtype (%d x %d) must be a single string", nr, nc);
  endif

  ## extract correct set of signal names values
  [idxvec, msg] = cellidx ({"in", "out", "st", "yd"}, sigtype);
  if (msg)
    error ("invalid sigtype = %s", sigtype);
  endif

  syssiglist = sysgetsignals (sys, sigtype);
  [idxvec, msg] = cellidx (syssiglist, signamelist);
  if (length (msg))
    error ("sysidx (sigtype = %s): %s", sigtype,
	   strrep (msg, "strlist", "signamelist"));
  endif

endfunction
