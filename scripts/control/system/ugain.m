## Copyright (C) 1997 Kai P. Mueller
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
## @deftypefn {Function File} {} ugain (@var{n})
## Creates a system with unity gain, no states.
## This trivial system is sometimes needed to create arbitrary
## complex systems from simple systems with @command{buildssic}.
## Watch out if you are forming sampled systems since @command{ugain}
## does not contain a sampling period.
## @end deftypefn
##
## @seealso{hinfdemo and jet707}

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: April 1998

function outsys = ugain (n)

  if (nargin != 1 || nargout > 1)
    usage ("outsys = ugain(n)");
  endif
  outsys = ss ([], [], [], eye (n));

endfunction
