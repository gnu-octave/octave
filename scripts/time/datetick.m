## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {} datetick ()
## @deftypefnx {Function File} {} datetick (@var{form})
## @deftypefnx {Function File} {} datetick (@var{axis}, @var{form})
## @deftypefnx {Function File} {} datetick (@dots{}, "keeplimits")
## @deftypefnx {Function File} {} datetick (@dots{}, "keepticks")
## @deftypefnx {Function File} {} datetick (@dots{ax}, @dots{})
## Add date formatted tick labels to an axis.  The axis the apply the
## ticks to is determined by @var{axis} that can take the values "x",
## "y" or "z".  The default value is "x".  The formatting of the labels is
## determined by the variable @var{form}, that can either be a string in
## the format needed by @code{dateform}, or a positive integer that can
## be accepted by @code{datestr}.
## @seealso{datenum, datestr}
## @end deftypefn

function datetick (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("datetick", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);
    __datetick__ (varargin{:});
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

%!demo
%! yr = 1900:10:2000;
%! pop = [76.094, 92.407, 106.461, 123.077 131.954, 151.868, 179.979, ...
%!        203.984, 227.225, 249.623, 282.224];
%! plot (datenum (yr, 1, 1), pop);
%! title ("US population (millions)");
%! xlabel ("Year");
%! datetick ("x", "YYYY");

%!demo
%! yr =1988:2:2002;
%! yr =datenum(yr,1,1);
%! pr = [12.1 13.3 12.6 13.1 13.3 14.1 14.4 15.2];
%! plot(yr,pr);
%! xlabel('year')
%! ylabel('average price')
%! ax=gca;
%! set(ax,'xtick',datenum(1990:5:2005,1,1))
%! datetick(2,'keepticks')
%! set(ax,'ytick',12:16)

## Remove from test statistics.  No real tests possible.
%!assert (1)

function __datetick__ (varargin)

  keeplimits = false;
  keepticks = false;
  idx = [];
  for i = 1 : nargin
    arg = varargin {i};
    if (ischar (arg))
      if (strcmpi (arg, "keeplimits"))
        keeplimits = true;
        idx = [idx, i];
      elseif (strcmpi (arg, "keepticks"))
        keepticks = true;
        idx = [idx, i];
      endif
    endif
  endfor

  varargin(idx) = [];
  nargin = length (varargin);
  form = [];
  ax = "x";

  if (nargin != 0)
    arg = varargin{1};
    if (ischar (arg) && (strcmp (arg, "x") || strcmp (arg, "y")
                         || strcmp (arg, "z")))
      ax = arg;
      if (nargin > 1)
        form = varargin{2};
        varargin(1:2) = [];
      else
        varargin(1) = [];
      endif
    else
      form = arg;
      varargin(1) = [];
    endif
  endif

  ## Don't publish the existence of this variable for use with dateaxis
  if (length (varargin) > 0)
    startdate = varargin{1};
  else
    startdate = [];
  endif

  if (! isempty (form))
    if (isnumeric (form))
      if (! isscalar (form) || floor (form) != form || form < 0)
        error ("datetick: expecting FORM argument to be a positive integer");
      endif
    elseif (! ischar (form))
      error ("datetick: expecting valid date format string");
    endif
  endif

  if (keepticks)
    ticks = get (gca (), strcat (ax, "tick"));
  else
    ## Need to do our own axis tick position calculation as
    ## year, etc, don't fallback on nice datenum values.
    objs = findall (gca());
    xmax = NaN;
    xmin = NaN;
    for i = 1 : length (objs)
      fld = get (objs (i));
      if (isfield (fld, strcat (ax, "data")))
        xdata = getfield (fld, strcat (ax, "data"))(:);
        xmin = min (xmin, min (xdata));
        xmax = max (xmax, max (xdata));
      endif
    endfor

    if (isnan (xmin) || isnan (xmax))
      xmin = 0;
      xmax = 1;
    elseif (xmin == xmax)
      xmax = xmin + 1;
    endif

    N = 3;
    if (xmax - xmin < N)
      ## Day scale or less
      if (xmax - xmin < N / 24 / 60 / 60)
        scl = 1 / 24 / 60 / 60;
      elseif (xmax - xmin < N / 24 / 6)
        scl = 1 / 24 / 60;
      else
        scl = 1 / 24;
      endif
      sep = __calc_tick_sep__ (xmin / scl , xmax / scl);
      xmin = sep * floor (xmin / scl / sep);
      xmax = sep * ceil (xmax / scl / sep);
      nticks = (xmax - xmin) / sep + 1;
      xmin *= scl;
      xmax *= scl;
    else
      [ymin, mmin, dmin] = datevec (xmin);
      [ymax, mmax, dmax] = datevec (xmax);
      minyear = ymin + (mmin - 1) / 12 + (dmin - 1) / 12 / 30;
      maxyear = ymax + (mmax - 1) / 12 + (dmax - 1) / 12 / 30;
      minmonth = mmin + (dmin - 1) / 30;
      maxmonth = (ymax  - ymin) * 12 + mmax + (dmax - 1) / 30;

      if (maxmonth - minmonth < N)
        sep = __calc_tick_sep__ (xmin, xmax);
        xmin = sep * floor (xmin / sep);
        xmax = sep * ceil (xmax / sep);
        nticks = (xmax - xmin) / sep + 1;
      elseif (maxyear - minyear < N)
        sep = __calc_tick_sep__ (minmonth , maxmonth);
        xmin = datenum (ymin, sep * floor (minmonth / sep), 1);
        xmax = datenum (ymin, sep * ceil (maxmonth / sep), 1);
        nticks = ceil (maxmonth / sep) - floor (minmonth / sep) + 1;
      else
        sep = __calc_tick_sep__ (minyear , maxyear);
        xmin = datenum (sep * floor (minyear / sep), 1, 1);
        xmax = datenum (sep * ceil (maxyear / sep), 1, 1);
        nticks = ceil (maxyear / sep) - floor (minyear / sep) + 1;
      endif
    endif
    ticks = xmin + [0 : nticks - 1] / (nticks - 1) * (xmax - xmin);
  endif

  if (isempty (form))
    r = max(ticks) - min(ticks);
    if r < 10/60/24
      ## minutes and seconds
      form = 13;
    elseif r < 2
      ## hours
      form = 15;
    elseif r < 15
      ## days
      form = 8;
    elseif r < 365
      ## FIXME -- FORM should be 19 for European users who use dd/mm
      ## instead of mm/dd.  How can that be determined automatically?
      ## months
      form = 6;
    elseif r < 90*12
      ## quarters
      form = 27;
    else
      ## years
      form = 10;
    endif
  endif

  if (length (ticks) == 6)
    ## Careful that its not treated as a datevec
    if (! isempty (startdate))
      sticks = strvcat (datestr (ticks(1:end-1) - ticks(1) + startdate, form),
      datestr (ticks(end) - ticks(1) + startdate, form));
    else
      sticks = strvcat (datestr (ticks(1:end-1), form),
      datestr (ticks(end), form));
    endif
  else
    if (! isempty (startdate))
      sticks = datestr (ticks - ticks(1) + startdate, form);
    else
      sticks = datestr (ticks, form);
    endif
  endif

  sticks = mat2cell (sticks, ones (rows (sticks), 1), columns (sticks));

  if (keepticks)
    if (keeplimits)
      set (gca(), strcat (ax, "ticklabel"), sticks);
    else
      set (gca(), strcat (ax, "ticklabel"), sticks, strcat (ax, "lim"),
      [min(ticks), max(ticks)]);
    endif
  else
    if (keeplimits)
      set (gca(), strcat (ax, "tick"), ticks, strcat (ax, "ticklabel"), sticks);
    else
      set (gca(), strcat (ax, "tick"), ticks, strcat (ax, "ticklabel"), sticks,
      strcat (ax, "lim"), [min(ticks), max(ticks)]);
    endif
  endif
endfunction

function [a, b] = __magform__ (x)
  if (x == 0)
    a = 0;
    b = 0;
  else
    l = log10 (abs (x));
    r = fmod (l, 1);
    a = 10 .^ r;
    b = fix (l - r);
    if (a < 1)
      a *= 10;
      b -= 1;
    endif
    if (x < 0)
      a = -a;
    endif
  endif
endfunction

## A translation from Tom Holoryd's python code at
## http://kurage.nimh.nih.gov/tomh/tics.py
function sep = __calc_tick_sep__ (lo, hi)
  persistent sqrt_2  = sqrt (2.0);
  persistent sqrt_10 = sqrt (10.0);
  persistent sqrt_50 = sqrt (50.0);

  ticint = 5;

  ## Reference: Lewart, C. R., "Algorithms SCALE1, SCALE2, and
  ## SCALE3 for Determination of Scales on Computer Generated
  ## Plots", Communications of the ACM, 10 (1973), 639-640.
  ## Also cited as ACM Algorithm 463.

  [a, b] = __magform__ ((hi - lo) / ticint);

  if (a < sqrt_2)
    x = 1;
  elseif (a < sqrt_10)
    x = 2;
  elseif (a < sqrt_50)
    x = 5;
  else
    x = 10;
  endif
  sep = x * 10 .^ b;
endfunction

