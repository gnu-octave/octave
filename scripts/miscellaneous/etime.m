## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} etime (@var{t1}, @var{t2})
## Return the difference (in seconds) between two time values returned from
## @code{clock}.  For example:
##
## @example
## t0 = clock ();
## # many computations later...
## elapsed_time = etime (clock (), t0);
## @end example
##
## @noindent
## will set the variable @code{elapsed_time} to the number of seconds since
## the variable @code{t0} was set.
## @end deftypefn
##
## @seealso{tic, toc, clock, and cputime}

## Author: jwe

function secs = etime (t1, t0)

  if (nargin != 2)
    usage ("etime (t1, t0)");
  endif

  if (isvector (t1) && length (t1) == 6 && isvector (t0) && length (t0) == 6)

    if (t1 (1) != t0 (1))
      error ("etime: can't handle timings over year boundaries yet");
    endif

    ## XXX FIXME XXX -- could check here to ensure that t1 and t0 really do
    ## make sense as vectors returned from clock().

    days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

    if (is_leap_year (t1 (1)))
      days_in_months (2) = days_in_months (2) + 1;
    endif

    d1 = sum (days_in_months (1:(t1 (2) - 1))) + t1 (3);
    d0 = sum (days_in_months (1:(t0 (2) - 1))) + t0 (3);

    s1 = 86400 * d1 + 3600 * t1 (4) + 60 * t1 (5) + t1 (6);
    s0 = 86400 * d0 + 3600 * t0 (4) + 60 * t0 (5) + t0 (6);

    secs = s1 - s0;

  else
    error ("etime: args are not 6-element vectors");
  endif


endfunction
