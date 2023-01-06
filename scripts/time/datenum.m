########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{days} =} datenum (@var{datevec})
## @deftypefnx {} {@var{days} =} datenum (@var{year}, @var{month}, @var{day})
## @deftypefnx {} {@var{days} =} datenum (@var{year}, @var{month}, @var{day}, @var{hour})
## @deftypefnx {} {@var{days} =} datenum (@var{year}, @var{month}, @var{day}, @var{hour}, @var{minute})
## @deftypefnx {} {@var{days} =} datenum (@var{year}, @var{month}, @var{day}, @var{hour}, @var{minute}, @var{second})
## @deftypefnx {} {@var{days} =} datenum ("datestr")
## @deftypefnx {} {@var{days} =} datenum ("datestr", @var{f})
## @deftypefnx {} {@var{days} =} datenum ("datestr", @var{p})
## @deftypefnx {} {[@var{days}, @var{secs}] =} datenum (@dots{})
## Return the date/time input as a serial day number, with Jan 1, 0000
## defined as day 1.
##
## The integer part, @code{floor (@var{days})} counts the number of
## complete days in the date input.
##
## The fractional part, @code{rem (@var{days}, 1)} corresponds to the time
## on the given day.
##
## The input may be a date vector (@pxref{XREFdatevec,,@code{datevec}}),
## date string (@pxref{XREFdatestr,,@code{datestr}}), or directly specified
## as input.
##
## When processing input datestrings, @var{f} is the format string used to
## interpret date strings (@pxref{XREFdatestr,,@code{datestr}}).  If no
## format @var{f} is specified, then a relatively slow search is performed
## through various formats.  It is always preferable to specify the format
## string @var{f} if it is known.  Formats which do not specify a particular
## time component will have the value set to zero.  Formats which do not
## specify a date will default to January 1st of the current year.
##
## @var{p} is the year at the start of the century to which two-digit years
## will be referenced.  If not specified, it defaults to the current year
## minus 50.
##
## The optional output @var{secs} holds the time on the specified day with
## greater precision than @var{days}.
##
## Notes:
##
## @itemize
## @item
## Years can be negative and/or fractional.
##
## @item
## Months below 1 are considered to be January.
##
## @item
## Days of the month start at 1.
##
## @item
## Days beyond the end of the month go into subsequent months.
##
## @item
## Days before the beginning of the month go to the previous month.
##
## @item
## Days can be fractional.
## @end itemize
##
## @strong{Caution:} datenums represent a specific time for the Earth as a
## whole.  They do not take in to account time zones (shifts in time based
## on location), nor seasonal changes due to Daylight Savings Time (shifts in
## time based on local regulation).  Be aware that it is possible to create
## datenums that, when interpreted by a function which accounts for time zone
## and DST shifts such as @code{datestr}, are nonexistent or ambiguous.
##
## @strong{Caution:} this function does not attempt to handle Julian calendars
## so dates before October 15, 1582 are wrong by as much as eleven days.  Also,
## be aware that only Roman Catholic countries adopted the calendar in 1582.
## It took until 1924 for it to be adopted everywhere.  See the Wikipedia entry
## on the Gregorian calendar for more details.
##
## @strong{Warning:} leap seconds are ignored.  A table of leap seconds is
## available on the Wikipedia entry for leap seconds.
##
## @seealso{datestr, datevec, now, clock, date}
## @end deftypefn

## Algorithm: Peter Baum (http://vsg.cape.com/~pbaum/date/date0.htm)

function [days, secs] = datenum (year, month = [], day = [], hour = 0, minute = 0, second = 0)

  ## Days until start of month assuming year starts March 1.
  persistent monthstart = [306; 337; 0; 31; 61; 92; 122; 153; 184; 214; 245; 275];
  persistent monthlength = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31];

  if (nargin == 0 || (nargin > 2 && (ischar (year) || iscellstr (year))))
    print_usage ();
  endif

  do_reshape = false;
  if (ischar (year) || iscellstr (year))
    [year, month, day, hour, minute, second] = datevec (year, month);
  else
    if (nargin == 1)
      nc = columns (year);
      if (nc > 6 || nc < 3)
        error ("datenum: expected date vector containing [YEAR, MONTH, DAY, HOUR, MINUTE, SECOND]");
      endif
      if (nc >= 6) second = year(:,6); endif
      if (nc >= 5) minute = year(:,5); endif
      if (nc >= 4) hour   = year(:,4); endif
      day   = year(:,3);
      month = year(:,2);
      year  = year(:,1);
    else
      [err, year, month, day] = common_size (year, month, day);
      if (err)
        error ("datenum: incompatible sizes for YEAR, MONTH, DAY");
      endif
      ## Preserve shape if necessary, and work with column vectors
      ## for the remainder of the function.
      do_reshape = ! iscolumn (day);
      if (do_reshape)
        sz_reshape = size (day);
        year = year(:);
        month = month(:);
        day = day(:);
      endif
    endif
  endif

  if (! (isa (year, "double") && isa (month, "double")
         && isa (day, "double") && isa (hour, "double")
         && isa (minute, "double") && isa (second, "double")))
    error ("datenum: all inputs must be of class double");
  endif

  ## For Matlab compatibility.  Otherwise, could allow negative months.
  month(month < 1) = 1;

  ## Treat fractional months, by converting the fraction to days
  if (any (month != fix (month)))
    fracmonth = month - floor (month);
    month = floor (month);
    ## Separate regular months from leap months
    idx = mod (month-1,12) + 1 != 2 | ! is_leap_year (floor (year));
    day(idx) += fracmonth(idx) .* monthlength(mod (month(idx)-1,12) + 1);
    day(! idx) += fracmonth(! idx) * 29;
  endif

  ## Set start of year to March by moving Jan. and Feb. to previous year.
  ## Correct for months > 12 by moving to subsequent years.
  year += ceil ((month-14)/12);

  ## Lookup number of days since start of the current year.
  day += monthstart(mod (month-1,12) + 1) + 60;

  ## Treat fractional years, by converting the fraction to days
  if (any (year != fix (year)))
    fracyear = year - floor (year);
    year = floor (year);
    day += fracyear .* (365 + is_leap_year (year+1));
  endif

  ## Add number of days to the start of the current year.  Correct
  ## for leap year every 4 years except centuries not divisible by 400.
  day += 365*year + floor (year/4) - floor (year/100) + floor (year/400);

  if (do_reshape)
    day = reshape (day, sz_reshape);
  endif

  ## Add fraction representing current second of the day.
  days = day + (hour + (minute + second/60)/60)/24;

  ## Output seconds if asked so that etime can be more accurate
  if (isargout (2))
    secs = day*86400 + hour*3600 + minute*60 + second;
  endif

endfunction


%!shared part
%! part = 0.514623842592593;
%!assert (datenum (2001,5,19), 730990)
%!assert (datenum ([1417,6,12]), 517712)
%!assert (datenum ([2001,5,19;1417,6,12]), [730990;517712])
%!assert (datenum (2001,5,19,12,21,3.5), 730990+part, eps)
%!assert (datenum ([1417,6,12,12,21,3.5]), 517712+part, eps)

## Test vector inputs
%!test
%! t = [2001,5,19,12,21,3.5; 1417,6,12,12,21,3.5];
%! n = [730990; 517712] + part;
%! assert (datenum (t), n, 2*eps);
%! ## Check that vectors can have either orientation
%! t = t';
%! n = n';
%! assert (datenum (t(1,:), t(2,:), t(3,:), t(4,:), t(5,:), t(6,:)), n, 2*eps);

%!assert (size (datenum (2000, 1, ones(1, 1, 3))), [1 1 3])

## Test fractional years including leap years
%!assert (fix (datenum ([2001.999 1 1; 2001.999 2 1])), [731216; 731247])
%!assert (fix (datenum ([2004.999 1 1; 2004.999 2 1])), [732312; 732343])

## Test fractional months including leap months
%!assert (fix (datenum ([2001 1.999 1; 2001 2.999 1])), [730882; 730910])
%!assert (fix (datenum ([2004 1.999 1; 2004 2.999 1])), [731977; 732006])


## Test mixed vectors and scalars
%!assert (datenum ([2008;2009],1,1), [datenum(2008,1,1);datenum(2009,1,1)])
%!assert (datenum (2008, [1;2], 1), [datenum(2008,1,1);datenum(2008,2,1)])
%!assert (datenum (2008, 1, [1;2]), [datenum(2008,1,1);datenum(2008,1,2)])
%!assert (datenum ([2008;2009], [1;2], 1),
%!        [datenum(2008,1,1);datenum(2009,2,1)])
%!assert (datenum ([2008;2009], 1, [1;2]),
%!        [datenum(2008,1,1);datenum(2009,1,2)])
%!assert (datenum (2008, [1;2], [1;2]), [datenum(2008,1,1);datenum(2008,2,2)])
## And the other orientation
%!assert (datenum ([2008 2009], 1, 1), [datenum(2008,1,1) datenum(2009,1,1)])
%!assert (datenum (2008, [1 2], 1), [datenum(2008,1,1) datenum(2008,2,1)])
%!assert (datenum (2008, 1, [1 2]), [datenum(2008,1,1) datenum(2008,1,2)])
%!assert (datenum ([2008 2009], [1 2], 1),
%!        [datenum(2008,1,1) datenum(2009,2,1)])
%!assert (datenum ([2008 2009], 1, [1 2]),
%!        [datenum(2008,1,1) datenum(2009,1,2)])
%!assert (datenum (2008, [1 2], [1 2]), [datenum(2008,1,1) datenum(2008,2,2)])

## Test string and cellstr inputs
%!assert (datenum ("5/19/2001"), 730990)
%!assert (datenum ({"5/19/2001"}), 730990)
%!assert (datenum (char ("5/19/2001", "6/6/1944")), [730990; 710189])
%!assert (datenum ({"5/19/2001", "6/6/1944"}), [730990; 710189])

## Test string input with format string
%!assert (datenum ("5-19, 2001", "mm-dd, yyyy"), 730990)

## Test input validation
%!error <Invalid call> datenum ()
%!error <expected date vector containing> datenum ([1, 2])
%!error <expected date vector containing> datenum ([1,2,3,4,5,6,7])
%!error <all inputs must be of class double> datenum (int32 (2000), int32 (1), int32 (1))
