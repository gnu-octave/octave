## Copyright (C) 2006, 2007, 2008 Paul Kienzle
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
## @deftypefn  {Function File} {} datenum (@var{year}, @var{month}, @var{day})
## @deftypefnx {Function File} {} datenum (@var{year}, @var{month}, @var{day}, @var{hour})
## @deftypefnx {Function File} {} datenum (@var{year}, @var{month}, @var{day}, @var{hour}, @var{minute})
## @deftypefnx {Function File} {} datenum (@var{year}, @var{month}, @var{day}, @var{hour}, @var{minute}, @var{second})
## @deftypefnx {Function File} {} datenum (@code{"date"})
## @deftypefnx {Function File} {} datenum (@code{"date"}, @var{p})
## Returns the specified local time as a day number, with Jan 1, 0000
## being day 1.  By this reckoning, Jan 1, 1970 is day number 719529.  
## The fractional portion, @var{p}, corresponds to the portion of the
## specified day.
##
## Notes:
##
## @itemize
## @item
## Years can be negative and/or fractional.
## @item
## Months below 1 are considered to be January.
## @item
## Days of the month start at 1.
## @item
## Days beyond the end of the month go into subsequent months.
## @item
## Days before the beginning of the month go to the previous month.
## @item
## Days can be fractional.
## @end itemize
##
## @strong{Warning:} this function does not attempt to handle Julian
## calendars so dates before Octave 15, 1582 are wrong by as much
## as eleven days.  Also be aware that only Roman Catholic countries
## adopted the calendar in 1582.  It took until 1924 for it to be 
## adopted everywhere.  See the Wikipedia entry on the Gregorian 
## calendar for more details.
##
## @strong{Warning:} leap seconds are ignored.  A table of leap seconds
## is available on the Wikipedia entry for leap seconds.
## @seealso{date, clock, now, datestr, datevec, calendar, weekday}
## @end deftypefn

## Algorithm: Peter Baum (http://vsg.cape.com/~pbaum/date/date0.htm)
## Author: pkienzle <pkienzle@users.sf.net>

function [days, secs] = datenum (Y, M, D, h, m, s)

  ## Days until start of month assuming year starts March 1.
  persistent monthstart = [306; 337; 0; 31; 61; 92; 122; 153; 184; 214; 245; 275];

  if (nargin == 0 || (nargin > 2  && ischar (Y)) || nargin > 6)
    print_usage ();
  endif
  if (ischar (Y))
    if (nargin < 2)
      M = [];
    endif
    [Y, M, D, h, m, s] = datevec (Y, M);
  else
    if (nargin < 6) s = 0; endif
    if (nargin < 5) m = 0; endif
    if (nargin < 4) h = 0; endif
    if (nargin == 1)
      nc = columns (Y);
      if (nc > 6 || nc < 3)
        error ("datenum: expected date vector containing [Y, M, D, h, m, s]");
      endif
      s = m = h = 0;
      if (nc >= 6) s = Y(:,6); endif
      if (nc >= 5) m = Y(:,5); endif
      if (nc >= 4) h = Y(:,4); endif
      D = Y(:,3);
      M = Y(:,2);
      Y = Y(:,1);
    endif 
  endif

  M(M<1) = 1; ## For compatibility.  Otherwise allow negative months.

  ## Set start of year to March by moving Jan. and Feb. to previous year.
  ## Correct for months > 12 by moving to subsequent years.
  Y += fix ((M-14)/12);

  ## Lookup number of days since start of the current year.
  if (numel (M) == 1 || numel (D) == 1)
    ## Allow M or D to be scalar while other values may be vectors or
    ## matrices.
    D += monthstart (mod (M-1,12) + 1) + 60;
    if (numel (M) > 1)
      D = reshape (D, size (M));
    endif
  else
    D += reshape (monthstart (mod (M-1,12) + 1), size (D)) + 60;
  endif

  ## Add number of days to the start of the current year. Correct
  ## for leap year every 4 years except centuries not divisible by 400.
  D += 365*Y + floor (Y/4) - floor (Y/100) + floor (Y/400);

  ## Add fraction representing current second of the day.
  days = D + (h+(m+s/60)/60)/24;

  ## Output seconds if asked so that etime can be more accurate
  secs = 86400*D + h*3600 + m*60 + s;

endfunction

%!shared part
%! part = 0.514623842592593;
%!assert(datenum(2001,5,19), 730990)
%!assert(datenum([1417,6,12]), 517712)
%!assert(datenum([2001,5,19;1417,6,12]), [730990;517712])
%!assert(datenum(2001,5,19,12,21,3.5), 730990+part, eps)
%!assert(datenum([1417,6,12,12,21,3.5]), 517712+part, eps)
## Test vector inputs
%!test
%! t = [2001,5,19,12,21,3.5; 1417,6,12,12,21,3.5];
%! n = [730990; 517712] + part;
%! assert(datenum(t), n, 2*eps);
## Make sure that the vectors can have either orientation
%!test
%! t = [2001,5,19,12,21,3.5; 1417,6,12,12,21,3.5]';
%! n = [730990 517712] + part;
%! assert(datenum(t(1,:), t(2,:), t(3,:), t(4,:), t(5,:), t(6,:)), n, 2*eps);

## Test mixed vectors and scalars
%!assert (datenum([2008;2009], 1, 1), [datenum(2008, 1, 1);datenum(2009, 1, 1)]);
%!assert (datenum(2008, [1;2], 1), [datenum(2008, 1, 1);datenum(2008, 2, 1)]);
%!assert (datenum(2008, 1, [1;2]), [datenum(2008, 1, 1);datenum(2008, 1, 2)]);
%!assert (datenum([2008;2009], [1;2], 1), [datenum(2008, 1, 1);datenum(2009, 2, 1)]);
%!assert (datenum([2008;2009], 1, [1;2]), [datenum(2008, 1, 1);datenum(2009, 1, 2)]);
%!assert (datenum(2008, [1;2], [1;2]), [datenum(2008, 1, 1);datenum(2008, 2, 2)]);
## And the other orientation
%!assert (datenum([2008 2009], 1, 1), [datenum(2008, 1, 1) datenum(2009, 1, 1)]);
%!assert (datenum(2008, [1 2], 1), [datenum(2008, 1, 1) datenum(2008, 2, 1)]);
%!assert (datenum(2008, 1, [1 2]), [datenum(2008, 1, 1) datenum(2008, 1, 2)]);
%!assert (datenum([2008 2009], [1 2], 1), [datenum(2008, 1, 1) datenum(2009, 2, 1)]);
%!assert (datenum([2008 2009], 1, [1 2]), [datenum(2008, 1, 1) datenum(2009, 1, 2)]);
%!assert (datenum(2008, [1 2], [1 2]), [datenum(2008, 1, 1) datenum(2008, 2, 2)]);
