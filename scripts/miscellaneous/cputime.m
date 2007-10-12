## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {[@var{total}, @var{user}, @var{system}] =} cputime ();
## Return the CPU time used by your Octave session.  The first output is
## the total time spent executing your process and is equal to the sum of
## second and third outputs, which are the number of CPU seconds spent
## executing in user mode and the number of CPU seconds spent executing in
## system mode, respectively.  If your system does not have a way to report
## CPU time usage, @code{cputime} returns 0 for each of its output values.
## Note that because Octave used some CPU time to start, it is reasonable
## to check to see if @code{cputime} works by checking to see if the total
## CPU time used is nonzero.
## @end deftypefn

## Author: jwe

function [total, user, system] = cputime ()

  if (nargin != 0)
    warning ("cputime: ignoring extra arguments");
  endif

  resource_stats = getrusage ();

  usr = resource_stats.utime;
  sys = resource_stats.stime;

  user = usr.sec + usr.usec / 1e6;
  system = sys.sec + sys.usec / 1e6;
  total = user + system;

endfunction
