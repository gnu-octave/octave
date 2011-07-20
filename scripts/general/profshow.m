## Copyright (C) 2011 Daniel Kraft
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
## @deftypefn {Function File} {} profshow (@var{data})
## @deftypefnx {Function File} {} profshow (@var{data}, @var{n})
## Show flat profiler results.
##
## This command prints out profiler data as a flat profile.  @var{data} is the
## structure returned by @code{profile ('info')}.  If @var{n} is given, it
## specifies the number of functions to show in the profile; functions are
## sorted in descending order by total time spent in them.  If there are more
## than @var{n} included in the profile, those will not be shown.  @var{n}
## defaults to 20.
## @end deftypefn

## Built-in profiler.
## Author: Daniel Kraft <d@domob.eu>

function profshow (data, n)

  if (nargin < 2)
    n = 20;
  endif

  m = length (data.FunctionTable);
  n = min (n, m);

  % We want to sort by times in descending order.  For this, extract the
  % times to an array, then sort this, and use the resulting index permutation
  % to print out our table.
  times = NA (1, m);
  for i = 1 : m
    times(i) = - data.FunctionTable(i).TotalTime;
  endfor
  [~, p] = sort (times);

  % For printing the table, find out the maximum length of a function name
  % so that we can proportion the table accordingly.  Based on this,
  % we can build the format used for printing table rows.
  nameLen = length ('Function');
  for i = 1 : n
    nameLen = max (nameLen, length (data.FunctionTable(p(i)).FunctionName));
  endfor
  headerFormat = sprintf ('%%%ds %%12s %%12s\n', nameLen);
  rowFormat = sprintf ('%%%ds%%13.3f%%13d\n', nameLen);

  printf (headerFormat, 'Function', 'Time (s)', 'Calls');
  for i = 1 : nameLen + 2 * 13
    printf ('-');
  endfor
  printf ('\n');
  for i = 1 : n
    row = data.FunctionTable(p(i));
    printf (rowFormat, row.FunctionName, row.TotalTime, row.NumCalls);
  endfor

endfunction

%!demo
%! profile ('on');
%! A = rand (100);
%! B = expm (A);
%! profile ('off');
%! T = profile ('info');
%! profshow (T, 10);
