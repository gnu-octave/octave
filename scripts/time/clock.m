########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{datevec} =} clock ()
## @deftypefnx {} {[@var{datevec}, @var{isdst}] =} clock ()
## Return the current local date and time as a date vector.
##
## The date vector contains the following fields: current year, month (1-12),
## day (1-31), hour (0-23), minute (0-59), and second (0-61).  The seconds
## field has a fractional part after the decimal point for extended accuracy.
##
## The optional second output @var{isdst} is true if Daylight Savings Time
## (DST) is in effect for the system's time zone.
##
## For example:
##
## @example
## @group
## fix (clock ())
##   @result{}   1993      8     20      4     56      1
## @end group
## @end example
##
## @code{clock} is more accurate on systems that have the @code{gettimeofday}
## function.
## @seealso{now, date, datevec}
## @end deftypefn

function [datevec, isdst] = clock ()

  tm = localtime (time ());

  datevec = zeros (1, 6);
  datevec(1) = tm.year + 1900;
  datevec(2) = tm.mon + 1;
  datevec(3) = tm.mday;
  datevec(4) = tm.hour;
  datevec(5) = tm.min;
  datevec(6) = tm.sec + tm.usec / 1e6;

  isdst = tm.isdst;

endfunction


%!test
%! t1 = clock ();
%! t2 = str2num (strftime ("[%Y, %m, %d, %H, %M, %S]", localtime (time ())));
%! assert (etime (t1, t2) < 1);
