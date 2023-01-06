////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "defun.h"
#include "error.h"
#include "oct-map.h"
#include "oct-time.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Date and time functions.

static octave_scalar_map
mk_tm_map (const sys::base_tm& t)
{
  octave_scalar_map m;

  m.assign ("usec", static_cast<double> (t.usec ()));
  m.assign ("sec", static_cast<double> (t.sec ()));
  m.assign ("min", static_cast<double> (t.min ()));
  m.assign ("hour", static_cast<double> (t.hour ()));
  m.assign ("mday", static_cast<double> (t.mday ()));
  m.assign ("mon", static_cast<double> (t.mon ()));
  m.assign ("year", static_cast<double> (t.year ()));
  m.assign ("wday", static_cast<double> (t.wday ()));
  m.assign ("yday", static_cast<double> (t.yday ()));
  m.assign ("isdst", static_cast<double> (t.isdst ()));
  m.assign ("gmtoff", static_cast<double> (t.gmtoff ()));
  m.assign ("zone", t.zone ());

  return m;
}

static inline int
intfield (const octave_scalar_map& m, const std::string& k, const char *who)
{
  int retval = 0;

  octave_value v = m.getfield (k);

  if (! v.isempty ())
    retval = v.xint_value ("%s: invalid TM_STRUCT argument", who);

  return retval;
}

static inline std::string
stringfield (const octave_scalar_map& m, const std::string& k, const char *who)
{
  std::string retval;

  octave_value v = m.getfield (k);

  if (! v.isempty ())
    retval = v.xstring_value ("%s: invalid TM_STRUCT argument", who);

  return retval;
}

static sys::base_tm
extract_tm (const octave_scalar_map& m, const char *who)
{
  sys::base_tm tm;

  tm.usec (intfield (m, "usec", who));
  tm.sec (intfield (m, "sec", who));
  tm.min (intfield (m, "min", who));
  tm.hour (intfield (m, "hour", who));
  tm.mday (intfield (m, "mday", who));
  tm.mon (intfield (m, "mon", who));
  tm.year (intfield (m, "year", who));
  tm.wday (intfield (m, "wday", who));
  tm.yday (intfield (m, "yday", who));
  tm.isdst (intfield (m, "isdst", who));
  tm.gmtoff (intfield (m, "gmtoff", who));
  tm.zone (stringfield (m, "zone", who));

  return tm;
}

DEFUN (time, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{seconds} =} time ()
Return the current time as the number of seconds since the epoch.

The epoch is referenced to 00:00:00 UTC (Coordinated Universal Time) 1 Jan
1970.  For example, on Monday February 17, 1997 at 07:15:06 UTC, the value
returned by @code{time} was 856163706.
@seealso{strftime, strptime, localtime, gmtime, mktime, now, date, clock,
datenum, datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (sys::time ());
}

/*
%!assert (time () > 0)

%!error time (1)
*/

DEFUN (gmtime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tm_struct} =} gmtime (@var{t})
Given a value returned from @code{time}, or any non-negative integer,
return a time structure corresponding to UTC (Coordinated Universal Time).

For example:

@example
@group
gmtime (time ())
     @result{} @{
           usec = 0
           sec = 6
           min = 15
           hour = 7
           mday = 17
           mon = 1
           year = 97
           wday = 1
           yday = 47
           isdst = 0
           gmtoff = 0
           zone = GMT
        @}
@end group
@end example
@seealso{strftime, strptime, localtime, mktime, time, now, date, clock, datenum,
datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 1 || args(0).numel () != 1)
    print_usage ();

  double tmp = args(0).double_value ();

  return ovl (mk_tm_map (sys::gmtime (tmp)));
}

/*
%!test
%! ts = gmtime (time ());
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error gmtime ()
%!error gmtime (1, 2)
*/

DEFUN (localtime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tm_struct} =} localtime (@var{t})
Given a value returned from @code{time}, or any non-negative integer,
return a time structure corresponding to the local time zone.

@example
@group
localtime (time ())
     @result{} @{
           usec = 0
           sec = 6
           min = 15
           hour = 1
           mday = 17
           mon = 1
           year = 97
           wday = 1
           yday = 47
           isdst = 0
           gmtoff = -21600
           zone = CST
        @}
@end group
@end example
@seealso{strftime, strptime, gmtime, mktime, time, now, date, clock, datenum,
datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 1 || args(0).numel () != 1)
    print_usage ();

  double tmp = args(0).double_value ();

  return ovl (mk_tm_map (sys::localtime (tmp)));
}

/*
%!test
%! ts = localtime (time ());
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error localtime ()
%!error localtime (1, 2)
*/

DEFUN (mktime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{seconds} =} mktime (@var{tm_struct})
Convert a time structure corresponding to the local time to the number of
seconds since the epoch.

For example:

@example
@group
mktime (localtime (time ()))
     @result{} 856163706
@end group
@end example
@seealso{strftime, strptime, localtime, gmtime, time, now, date, clock, datenum,
datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_scalar_map map = args(
                            0).xscalar_map_value ("mktime: TM_STRUCT argument must be a structure");

  sys::base_tm tm = extract_tm (map, "mktime");

  return ovl (sys::time (tm));
}

/*
%!test
%! t = time ();
%! assert (fix (mktime (localtime (t))) == fix (t));

## These tests fail on systems with mktime functions of limited
## intelligence:
%!assert (datestr (datenum (1969, 1, 1), 0), "01-Jan-1969 00:00:00")
%!assert (datestr (datenum (1901, 1, 1), 0), "01-Jan-1901 00:00:00")
%!assert (datestr (datenum (1795, 1, 1), 0), "01-Jan-1795 00:00:00")

%!error mktime ()
%!error mktime (1)
%!error mktime (1, 2, 3)
%!error mktime (struct ("year", "foo"))
*/

DEFUN (strftime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{str} =} strftime (@var{fmt}, @var{tm_struct})
Format the time structure @var{tm_struct} in a flexible way using the format
string @var{fmt} that contains @samp{%} substitutions similar to those in
@code{printf}.

Except where noted, substituted fields have a fixed size; numeric fields are
padded if necessary.  Padding is with zeros by default; for fields that
display a single number, padding can be changed or inhibited by following
the @samp{%} with one of the modifiers described below.  Unknown field
specifiers are copied as normal characters.  All other characters are copied
to the output without change.  For example:

@example
@group
strftime ("%r (%Z) %A %e %B %Y", localtime (time ()))
      @result{} "01:15:06 AM (CST) Monday 17 February 1997"
@end group
@end example

Octave's @code{strftime} function supports a superset of the ANSI C field
specifiers.

@noindent
Literal character fields:

@table @code
@item %%
% character.

@item %n
Newline character.

@item %t
Tab character.
@end table

@noindent
Numeric modifiers (a nonstandard extension):

@table @code
@item - (dash)
Do not pad the field.

@item _ (underscore)
Pad the field with spaces.
@end table

@noindent
Time fields:

@table @code
@item %H
Hour (00-23).

@item %I
Hour (01-12).

@item %k
Hour (0-23).

@item %l
Hour (1-12).

@item %M
Minute (00-59).

@item %p
Locale's AM or PM.

@item %r
Time, 12-hour (hh:mm:ss [AP]M).

@item %R
Time, 24-hour (hh:mm).

@item %s
Time in seconds since 00:00:00, Jan 1, 1970 (a nonstandard extension).

@item %S
Second (00-61).

@item %T
Time, 24-hour (hh:mm:ss).

@item %X
Locale's time representation (%H:%M:%S).

@item %z
Offset from UTC (±@nospell{hhmm}), or nothing if no time zone is
determinable.

@item %Z
Time zone (EDT), or nothing if no time zone is determinable.
@end table

@noindent
Date fields:

@table @code
@item %a
Locale's abbreviated weekday name (Sun-Sat).

@item %A
Locale's full weekday name, variable length (Sunday-Saturday).

@item %b
Locale's abbreviated month name (Jan-Dec).

@item %B
Locale's full month name, variable length (January-December).

@item %c
Locale's date and time (Sat Nov 04 12:02:33 EST 1989).

@item %C
Century (00-99).

@item %d
Day of month (01-31).

@item %e
Day of month ( 1-31).

@item %D
Date (mm/dd/yy).

@item %h
Same as %b.

@item %j
Day of year (001-366).

@item %m
Month (01-12).

@item %U
Week number of year with Sunday as first day of week (00-53).

@item %w
Day of week (0-6).

@item %W
Week number of year with Monday as first day of week (00-53).

@item %x
Locale's date representation (mm/dd/yy).

@item %y
Last two digits of year (00-99).

@item %Y
Year (1970-).
@end table
@seealso{strptime, localtime, gmtime, mktime, time, now, date, clock, datenum,
datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string fmt = args(0).xstring_value ("strftime: FMT must be a string");

  octave_scalar_map map = args(1).xscalar_map_value ("strftime: TM_STRUCT must be a structure");

  sys::base_tm tm = extract_tm (map, "strftime");

  return ovl (tm.strftime (fmt));
}

/*
%!assert (ischar (strftime ("%%%n%t%H%I%k%l", localtime (time ()))))
%!assert (ischar (strftime ("%M%p%r%R%s%S%T", localtime (time ()))))
%!assert (ischar (strftime ("%X%Z%z%a%A%b%B", localtime (time ()))))
%!assert (ischar (strftime ("%c%C%d%e%D%h%j", localtime (time ()))))
%!assert (ischar (strftime ("%m%U%w%W%x%y%Y", localtime (time ()))))

%!error strftime ()
%!error strftime ("foo", 1)
%!error strftime ("foo", struct ("year", "foo"))
%!error strftime ("foo", localtime (time ()), 1)
*/

DEFUN (strptime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{tm_struct}, @var{nchars}] =} strptime (@var{str}, @var{fmt})
Convert the string @var{str} to the time structure @var{tm_struct} under
the control of the format string @var{fmt}.

If @var{fmt} fails to match, @var{nchars} is 0; otherwise, it is set to the
position of last matched character plus 1.  Always check for this unless
you're absolutely sure the date string will be parsed correctly.
@seealso{strftime, localtime, gmtime, mktime, time, now, date, clock, datenum,
datestr, datevec, calendar, weekday}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string str = args(0).xstring_value ("strptime: argument STR must be a string");

  std::string fmt = args(1).xstring_value ("strptime: FMT must be a string");

  sys::strptime t (str, fmt);

  return ovl (mk_tm_map (t), t.characters_converted ());
}

/*
%!test
%! fmt = "%Y-%m-%d %H:%M:%S";
%! s = strftime (fmt, localtime (time ()));
%! ts = strptime  (s, fmt);
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error strptime ()
*/

OCTAVE_END_NAMESPACE(octave)
