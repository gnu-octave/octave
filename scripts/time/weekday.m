## Copyright (C) 2000-2011 Paul Kienzle
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
## @deftypefn  {Function File} {[@var{n}, @var{s}] =} weekday (@var{d})
## @deftypefnx {Function File} {[@var{n}, @var{s}] =} weekday (@var{d}, @var{format})
## Return the day of week as a number in @var{n} and a string in @var{s},
## for example @code{[1, "Sun"]}, @code{[2, "Mon"]}, @dots{}, or
## @code{[7, "Sat"]}.
##
## @var{d} is a serial date number or a date string.
##
## If the string @var{format} is given and is @code{"long"}, @var{s} will
## contain the full name of the weekday; otherwise (or if @var{format} is
## @code{"short"}), @var{s} will contain the abbreviated name of the weekday.
## @seealso{datenum, datevec, eomday}
## @end deftypefn

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 10 October 2001 (CVS)
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function [d, s] = weekday (d, format)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin < 2)
    format = "short";
  endif

  if (iscell (d) || isnumeric (d))
    endsize = size (d);
  elseif (ischar (d))
    endsize = [size(d, 1), 1];
  endif
  if (ischar (d) || iscell (d))
    ## Make sure the date is numeric
    d = datenum (d);
  endif
  ## Find the offset from a known Sunday (2008-Jan-6), mod 7.
  d = floor (reshape (mod(d - 733048, 7), endsize));
  ## Make Saturdays a 7 and not a 0.
  d(!d) = 7;

  if (nargout > 1)
    if (strcmpi (format, "long"))
      names = {"Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
               "Friday" "Saturday"};
    else
      names = {"Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"};
    endif
    s = strvcat (names(d));
  endif

endfunction

# tests
%!assert(weekday(728647),2)
## Test vector inputs for both directions
%!assert(weekday([728647 728648]), [2 3])
%!assert(weekday([728647;728648]), [2;3])
## Test a full week before our reference day
%!assert(weekday('19-Dec-1994'),2)
%!assert(weekday('20-Dec-1994'),3)
%!assert(weekday('21-Dec-1994'),4)
%!assert(weekday('22-Dec-1994'),5)
%!assert(weekday('23-Dec-1994'),6)
%!assert(weekday('24-Dec-1994'),7)
%!assert(weekday('25-Dec-1994'),1)
## Test our reference day
%!assert(weekday('6-Jan-2008'),1)
## Test a full week after our reference day
%!assert(weekday('1-Feb-2008'),6)
%!assert(weekday('2-Feb-2008'),7)
%!assert(weekday('3-Feb-2008'),1)
%!assert(weekday('4-Feb-2008'),2)
%!assert(weekday('5-Feb-2008'),3)
%!assert(weekday('6-Feb-2008'),4)
%!assert(weekday('7-Feb-2008'),5)
## Test fractional dates
%!assert(weekday(728647.1),2)
# demos
%!demo
%! [n, s] = weekday (now ())
%!demo
%! [n, s] = weekday (728647)
%!demo
%! [n, s] = weekday ('19-Dec-1994')
