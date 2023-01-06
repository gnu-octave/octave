########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn {} {t =} now ()
## Return the current local date/time as a serial day number
## (@pxref{XREFdatenum,,@code{datenum}}).
##
## The integral part, @code{floor (now)} corresponds to the number of days
## between today and Jan 1, 0000.
##
## The fractional part, @code{rem (now, 1)} corresponds to the current time.
## @seealso{clock, date, datenum}
## @end deftypefn

function t = now ()

  t = datenum (clock ());

  ## The following doesn't work (e.g., one hour off on 2005-10-04):
  ##
  ##   seconds since 1970-1-1 corrected by seconds from GMT to local time
  ##   divided by 86400 sec/day plus day num for 1970-1-1
  ##   t = (time - mktime(gmtime(0)))/86400 + 719529;
  ##
  ## mktime (gmtime (0)) does indeed return the offset from Greenwich to the
  ## local time zone, but we need to account for daylight saving time
  ## changing by an hour the offset from CUT for part of the year.

endfunction


%!assert (isnumeric (now ()))
%!assert (now () > 0)
%!assert (now () <= now ())
