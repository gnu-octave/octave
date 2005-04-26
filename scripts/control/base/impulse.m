## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{y}, @var{t}] =} impulse (@var{sys}, @var{inp}, @var{tstop}, @var{n})
## Impulse response for a linear system.
## The system can be discrete or multivariable (or both).
## If no output arguments are specified, @code{impulse}
## produces a plot or the impulse response data for system @var{sys}.
##
## @strong{Inputs}
## @table @var
## @item sys
## System data structure.
## @item inp
## Index of input being excited
## @item tstop
## The argument @var{tstop} (scalar value) denotes the time when the
## simulation should end.
## @item n
## the number of data values.
##
## Both parameters @var{tstop} and @var{n} can be omitted and will be
## computed from the eigenvalues of the A Matrix.
## @end table
## @strong{Outputs}
## @table @var
## @item y
## Values of the impulse response.
## @item t
## Times of the impulse response.
## @end table
## @end deftypefn
##
## @seealso{step, __stepimp__}

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: October 2, 1997
## based on lsim.m of Scottedward Hodel
## modified by

function [y, t] = impulse (sys, inp, tstop, n)

  if ((nargin < 1) || (nargin > 4))
    usage ("[y, u] = impulse (sys, inp, tstop, n)");
  endif

  if (nargout > 2)
    usage ("[y, u] = impulse (sys, inp, tstop, n)");
  endif

  if (! isstruct (sys))
    error ("impulse: sys must be a system data structure.");
  endif

  if (nargout == 0)
    switch (nargin)
      case (1)
        __stepimp__ (2, sys);
      case (2)
        __stepimp__ (2, sys, inp);
      case (3)
        __stepimp__ (2, sys, inp, tstop);
      case (4)
        __stepimp__ (2, sys, inp, tstop, n);
    endswitch
  else
    switch (nargin)
      case (1)
        [y, t] = __stepimp__ (2, sys);
      case (2)
        [y, t] = __stepimp__ (2, sys, inp);
      case (3)
        [y, t] = __stepimp__ (2, sys, inp, tstop);
      case (4)
        [y, t] = __stepimp__ (2, sys, inp, tstop, n);
    endswitch
  endif

endfunction
