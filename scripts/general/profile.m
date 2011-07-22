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
## @deftypefn  {Function File} {} profile on
## @deftypefnx {Function File} {} profile off
## @deftypefnx {Function File} {} profile resume
## @deftypefnx {Function File} {} profile clear
## @deftypefnx {Function File} {@var{S} =} profile ('status')
## @deftypefnx {Function File} {@var{T} =} profile ('info')
## Control the built-in profiler.
##
## @table @code
## @item profile on
## Start the profiler, clearing all previously collected data if there
## is any.
##
## @item profile off
## Stop profiling.  The collected data can later be retrieved and examined
## with calls like @code{S = profile ('info')}.
##
## @item profile clear
## Clear all collected profiler data.
##
## @item profile resume
## Restart profiling without cleaning up the old data and instead
## all newly collected statistics are added to the already existing ones.
##
## @item @var{S} = profile ('status')
## Return a structure filled with certain information about the current status
## of the profiler.  At the moment, the only field is @code{ProfilerStatus}
## which is either 'on' or 'off'.
##
## @item @var{T} = profile ('info')
## Return the collected profiling statistics in the structure @var{T}.
## Currently, the only field is @code{FunctionTable} which is an array
## of structures, each entry corresponding to a function which was called
## and for which profiling statistics are present.
## @end table
## @end deftypefn

## Built-in profiler.
## Author: Daniel Kraft <d@domob.eu>

function retval = profile (option)

  if (nargin != 1)
    print_usage ();
  endif

  switch (option)
    case 'on'
      __profiler_reset ();
      __profiler_enable (true);

    case 'off'
      __profiler_enable (false);

    case 'clear'
      __profiler_reset ();

    case 'resume'
      __profiler_enable (true);

    case 'status'
      enabled = __profiler_enable ();
      if (enabled)
        enabled = 'on';
      else
        enabled = 'off';
      endif
      retval = struct ('ProfilerStatus', enabled);

    case 'info'
      data = __profiler_data ();
      retval = struct ('FunctionTable', data);

    otherwise
      warning ("profile: Unrecognized option '%s'", option);
      print_usage ();

  endswitch

endfunction


%!demo
%! profile ('on');
%! A = rand (100);
%! B = expm (A);
%! profile ('off');
%! profile ('resume');
%! C = sqrtm (A);
%! profile ('off');
%! T = profile ('info');
%! profshow (T);
