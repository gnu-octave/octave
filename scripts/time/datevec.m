## Copyright (C) 2000-2012 Paul Kienzle
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
## @deftypefn  {Function File} {@var{v} =} datevec (@var{date})
## @deftypefnx {Function File} {@var{v} =} datevec (@var{date}, @var{f})
## @deftypefnx {Function File} {@var{v} =} datevec (@var{date}, @var{p})
## @deftypefnx {Function File} {@var{v} =} datevec (@var{date}, @var{f}, @var{p})
## @deftypefnx {Function File} {[@var{y}, @var{m}, @var{d}, @var{h}, @var{mi}, @var{s}] =} datevec (@dots{})
## Convert a serial date number (see @code{datenum}) or date string (see
## @code{datestr}) into a date vector.
##
## A date vector is a row vector with six members, representing the year,
## month, day, hour, minute, and seconds respectively.
##
## @var{f} is the format string used to interpret date strings
## (see @code{datestr}).
##
## @var{p} is the year at the start of the century to which two-digit years
## will be referenced.  If not specified, it defaults to the current year
## minus 50.
## @seealso{datenum, datestr, clock, now, date}
## @end deftypefn

## Algorithm: Peter Baum (http://vsg.cape.com/~pbaum/date/date0.htm)

## Author: pkienzle <pkienzle@users.sf.net>
## Modified: bdenney <bill@givebillmoney.com>
## Created: 10 October 2001 (CVS)
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

## The function __date_str2vec__ is based on datesplit by Bill Denney.

function [y, m, d, h, mi, s] = datevec (date, f = [], p = [])

  persistent std_formats nfmt;

  if (isempty (std_formats))
    std_formats = cell ();
    nfmt = 0;
    ## These formats are specified by Matlab to be parsed
    ## The '# XX' refers to the datestr numerical format code
    std_formats{++nfmt} = "dd-mmm-yyyy HH:MM:SS";   # 0
    std_formats{++nfmt} = "dd-mmm-yyyy";            # 1
    std_formats{++nfmt} = "mm/dd/yy";               # 2
    std_formats{++nfmt} = "mm/dd";                  # 6
    std_formats{++nfmt} = "HH:MM:SS";               # 13
    std_formats{++nfmt} = "HH:MM:SS PM";            # 14
    std_formats{++nfmt} = "HH:MM";                  # 15
    std_formats{++nfmt} = "HH:MM PM";               # 16
    std_formats{++nfmt} = "mm/dd/yyyy";             # 23

    ## These are other formats that Octave tries
    std_formats{++nfmt} = "mmm-dd-yyyy HH:MM:SS";
    std_formats{++nfmt} = "mmm-dd-yyyy";
    std_formats{++nfmt} = "dd mmm yyyy HH:MM:SS";
    std_formats{++nfmt} = "dd mmm yyyy";
    std_formats{++nfmt} = "mmm dd yyyy HH:MM:SS";
    std_formats{++nfmt} = "mmm dd yyyy";
    std_formats{++nfmt} = "dd.mmm.yyyy HH:MM:SS";
    std_formats{++nfmt} = "dd.mmm.yyyy";
    std_formats{++nfmt} = "mmm.dd.yyyy HH:MM:SS";
    std_formats{++nfmt} = "mmm.dd.yyyy";
    std_formats{++nfmt} = "mmmyy";                  # 12
    std_formats{++nfmt} = "mm/dd/yyyy HH:MM";
  endif

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (ischar (date))
    date = cellstr (date);
  endif

  if (isnumeric (f))
    p = f;
    f = [];
  endif

  if (isempty (f))
    f = -1;
  endif

  if (isempty (p))
    p = (localtime (time ())).year + 1900 - 50;
  endif

  if (iscell (date))

    nd = numel (date);

    y = m = d = h = mi = s = zeros (nd, 1);

    if (f == -1)
      for k = 1:nd
        found = false;
        for l = 1:nfmt
          [f, rY, ry, fy, fm, fd, fh, fmi, fs] = __date_vfmt2sfmt__ (std_formats{l});
          [found y(k) m(k) d(k) h(k) mi(k) s(k)] = __date_str2vec__ (date{k}, p, f, rY, ry, fy, fm, fd, fh, fmi, fs);
          if (found)
            break;
          endif
        endfor
        if (! found)
          error ("datevec: none of the standard formats match the DATE string");
        endif
      endfor
    else
      ## Decipher the format string just once for speed.
      [f, rY, ry, fy, fm, fd, fh, fmi, fs] = __date_vfmt2sfmt__ (f);
      for k = 1:nd
        [found y(k) m(k) d(k) h(k) mi(k) s(k)] = __date_str2vec__ (date{k}, p, f, rY, ry, fy, fm, fd, fh, fmi, fs);
        if (! found)
          error ("datevec: DATE not parsed correctly with given format");
        endif
      endfor
    endif

  else   # datenum input

    date = date(:);

    ## Move day 0 from midnight -0001-12-31 to midnight 0000-3-1
    z = floor (date) - 60;
    ## Calculate number of centuries; K1 = 0.25 is to avoid rounding problems.
    a = floor ((z - 0.25) / 36524.25);
    ## Days within century; K2 = 0.25 is to avoid rounding problems.
    b = z - 0.25 + a - floor (a / 4);
    ## Calculate the year (year starts on March 1).
    y = floor (b / 365.25);
    ## Calculate day in year.
    c = fix (b - floor (365.25 * y)) + 1;
    ## Calculate month in year.
    m = fix ((5 * c + 456) / 153);
    d = c - fix ((153 * m - 457) / 5);
    ## Move to Jan 1 as start of year.
    ++y(m > 12);
    m(m > 12) -= 12;

    ## Convert hour-minute-seconds.  Attempt to account for precision of
    ## datenum format.

    fracd = date - floor (date);
    tmps = abs (eps*86400*date);
    tmps(tmps == 0) = 1;
    srnd = 2 .^ floor (- log2 (tmps));
    s = round (86400 * fracd .* srnd) ./ srnd;
    h = floor (s / 3600);
    s = s - 3600 * h;
    mi = floor (s / 60);
    s = s - 60 * mi;

  endif

  if (nargout <= 1)
    y = [y, m, d, h, mi, s];
  endif

endfunction

function [f, rY, ry, fy, fm, fd, fh, fmi, fs] = __date_vfmt2sfmt__ (f)

  ## Play safe with percent signs.
  f = strrep (f, "%", "%%");

  if (! isempty (strfind (f, "PM")) || ! isempty (strfind (f, "AM")))
    ampm = true;
  else
    ampm = false;
  endif

  ## Date part.
  f = regexprep (f, '[Yy][Yy][Yy][Yy]', "%Y");
  f = regexprep (f, '[Yy][Yy]', "%y");
  f = strrep (f, "mmmm", "%B");
  f = strrep (f, "mmm", "%b");
  f = strrep (f, "mm", "%m");
  f = regexprep (f, '[Dd][Dd][Dd][Dd]', "%A");
  f = regexprep (f, '[Dd][Dd][Dd]', "%a");
  f = regexprep (f, '[Dd][Dd]', "%d");

  ## Time part.
  if (ampm)
    f = strrep (f, "HH", "%I");
    f = strrep (f, "PM", "%p");
    f = strrep (f, "AM", "%p");
  else
    f = strrep (f, "HH", "%H");
  endif
  f = strrep (f, "MM", "%M");
  f = regexprep (f, '[Ss][Ss]', "%S");

  rY = rindex (f, "%Y");
  ry = rindex (f, "%y");

  ## Check whether we need to give default values.
  ## Possible error when string contains "%%".
  fy = rY || ry;
  fm = index (f, "%m") || index (f, "%b") || index (f, "%B");
  fd = index (f, "%d") || index (f, "%a") || index (f, "%A");
  fh = index (f, "%H") || index (f, "%I");
  fmi = index (f, "%M");
  fs = index (f, "%S");

endfunction

function [found, y, m, d, h, mi, s] = __date_str2vec__ (ds, p, f, rY, ry, fy, fm, fd, fh, fmi, fs)

  idx = strfind (f, "FFF");
  if (! isempty (idx)) 
    ## Kludge to handle FFF millisecond format since strptime does not
    f(idx:idx+2) = []; 
    [~, nc] = strptime (ds, f);
    if (nc > 0)
      msec = ds(nc:min(nc+2, end)); 
      f = [f(1:idx-1) msec f(idx:end)]; 
      [tm, nc] = strptime (ds, f);
      tm.usec = 1000 * str2double (msec);
    endif
  else
    [tm, nc] = strptime (ds, f);
  endif
  
  if (nc == columns (ds) + 1)
    found = true;
    y = tm.year + 1900; m = tm.mon + 1; d = tm.mday;
    h = tm.hour; mi = tm.min; s = tm.sec + tm.usec / 1e6;
    if (rY < ry)
      if (y > 1999)
        y -= 2000;
      else
        y -= 1900;
      endif
      y += p - mod (p, 100);
      if (y < p)
        y += 100;
      endif
    endif
    if (! fy && ! fm && ! fd)
      tmp = localtime (time ());
      y = tmp.year + 1900;
      m = tmp.mon + 1;
      d = tmp.mday;
    elseif (! fy && fm && fd)
      tmp = localtime (time ());
      y = tmp.year + 1900;
    elseif (fy && fm && ! fd)
      d = 1;
    endif
    if (! fh && ! fmi && ! fs)
      h = mi = s = 0;
    elseif (fh && fmi && ! fs)
      s = 0;
    endif
  else
    y = m = d = h = mi = s = 0;
    found = false;
  endif

endfunction


%!demo
%! ## Current date and time
%! datevec (now ())

%!shared nowvec
%! nowvec = datevec (now); # Some tests could fail around midnight!
%!# tests for standard formats: 0, 1, 2, 6, 13, 14, 15, 16, 23
%!assert (datevec ("07-Sep-2000 15:38:09"), [2000,9,7,15,38,9])
%!assert (datevec ("07-Sep-2000"), [2000,9,7,0,0,0])
%!assert (datevec ("09/07/00"), [2000,9,7,0,0,0])
%!assert (datevec ("09/13"), [nowvec(1),9,13,0,0,0])
%!assert (datevec ("15:38:09"), [nowvec(1:3),15,38,9])
%!assert (datevec ("3:38:09 PM"), [nowvec(1:3),15,38,9])
%!assert (datevec ("15:38"), [nowvec(1:3),15,38,0])
%!assert (datevec ("03:38 PM"), [nowvec(1:3),15,38,0])
%!assert (datevec ("03/13/1962"), [1962,3,13,0,0,0])

%% Test millisecond format FFF
%!assert (datevec ("15:38:21.25", "HH:MM:SS.FFF"), [nowvec(1:3),15,38,21.025])

# Other tests
%!assert (datenum (datevec ([-1e4:1e4])), [-1e4:1e4]')
%!test
%! t = linspace (-2e5, 2e5, 10993);
%! assert (all (abs (datenum (datevec (t)) - t') < 1e-5));

