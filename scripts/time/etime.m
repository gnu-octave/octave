## Copyright (C) 1996, 1997, 2006, 2007 John W. Eaton
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
## @seealso{tic, toc, clock, cputime}
## @end deftypefn

## Author: jwe

function secs = etime (t1, t0)

  if (nargin != 2)
    print_usage ();
  endif

  [d1, s1] = datenum (t1);
  [d0, s0] = datenum (t0);

  secs = s1 - s0;

endfunction

%!assert(etime([1900,12,31,23,59,59],[1901,1,1,0,0,0]),-1)
%!assert(etime([1900,2,28,23,59,59],[1900,3,1,0,0,0]),-1)
%!assert(etime([2000,2,28,23,59,59],[2000,3,1,0,0,0]),-86401)
%!assert(etime([1996,2,28,23,59,59],[1996,3,1,0,0,0]),-86401)
%!test
%!  t1 = [1900,12,31,23,59,59; 1900,2,28,23,59,59];
%!  t2 = [1901,1,1,0,0,0; 1900,3,1,0,0,0];
%!  assert(etime(t2, t1), [1;1]);

%!test
%! t1 = [1993, 8, 20, 4, 56, 1];
%! t2 = [1993, 8, 21, 4, 56, 1];
%! t3 = [1993, 8, 20, 5, 56, 1];
%! t4 = [1993, 8, 20, 4, 57, 1];
%! t5 = [1993, 8, 20, 4, 56, 14];
%! 
%! assert((etime (t2, t1) == 86400 && etime (t3, t1) == 3600
%! && etime (t4, t1) == 60 && etime (t5, t1) == 13));

%!error etime ();

%!error etime (1, 2, 3);

