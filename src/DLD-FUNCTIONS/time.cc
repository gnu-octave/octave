/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include<iostream.h>

#include "defun-dld.h"
#include "error.h"
#include "oct-map.h"
#include "systime.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

// Date and time functions.

static Octave_map
mk_tm_map (struct tm *tm, double fraction)
{
  Octave_map m;

  m ["usec"] = fraction * 1e6;
  m ["sec"] = static_cast<double> (tm->tm_sec);
  m ["min"] = static_cast<double> (tm->tm_min);
  m ["hour"] = static_cast<double> (tm->tm_hour);
  m ["mday"] = static_cast<double> (tm->tm_mday);
  m ["mon"] = static_cast<double> (tm->tm_mon);
  m ["year"] = static_cast<double> (tm->tm_year);
  m ["wday"] = static_cast<double> (tm->tm_wday);
  m ["yday"] = static_cast<double> (tm->tm_yday);
  m ["isdst"] = static_cast<double> (tm->tm_isdst);

#if defined (HAVE_TM_ZONE)
  m ["zone"]  = tm->tm_zone;
#elif defined (HAVE_TZNAME)
  if (tm->tm_isdst == 0 || tm->tm_isdst == 1)
    m ["zone"] = tzname[tm->tm_isdst];
#endif

  return m;
}

static struct tm*
extract_tm (Octave_map &m, double& fraction)
{
  static struct tm tm;

  fraction = (m ["usec"] . double_value ()) / 1e6;
  tm.tm_sec = static_cast<int> (m ["sec"] . double_value ());
  tm.tm_min = static_cast<int> (m ["min"] . double_value ());
  tm.tm_hour = static_cast<int> (m ["hour"] . double_value ());
  tm.tm_mday = static_cast<int> (m ["mday"] . double_value ());
  tm.tm_mon = static_cast<int> (m ["mon"] . double_value ());
  tm.tm_year = static_cast<int> (m ["year"] . double_value ());
  tm.tm_wday = static_cast<int> (m ["wday"] . double_value ());
  tm.tm_yday = static_cast<int> (m ["yday"] . double_value ());
  tm.tm_isdst = static_cast<int> (m ["isdst"] . double_value ());

#if defined (HAVE_TM_ZONE)
  static char *tm_zone = 0;

  string tstr = m ["zone"] . string_value ();

  delete [] tm_zone;
  tm_zone = strsave (tstr.c_str ());

  tm.tm_zone = tm_zone;
#endif

  return &tm;
}

DEFUN_DLD (time, , ,
  "time ()\n\
\n\
Return current time.  On Unix systems, this is the number of\n\
seconds since the epoch.")
{
  time_t now;
  double fraction = 0.0;

#if defined (HAVE_GETTIMEOFDAY)

  struct timeval tp;

#if defined  (GETTIMEOFDAY_NO_TZ)
  gettimeofday (&tp);
#else
  gettimeofday (&tp, 0);
#endif

  now = tp.tv_sec;

  fraction = tp.tv_usec / 1e6;

#else

  now = time (0);

#endif
 
  return static_cast<double> (now) + fraction;
}

DEFUN_DLD (gmtime, args, ,
  "gmtime (TIME)\n\
\n\
Given a value returned from time(), return a structure like that\n\
returned from localtime() but with values corresponding to\n\
Coordinated Universal Time (UTC).")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
	{
	  time_t timeval = static_cast<int> (tmp);
	  double ip;
	  double fraction = modf (tmp, &ip); 

	  retval = octave_value (mk_tm_map (gmtime (&timeval), fraction));
	}
    }
  else
    print_usage ("gmtime");

  return retval;
}

DEFUN_DLD (localtime, args, ,
  "localtime (TIME)\n\
\n\
Given a value returned from time(), return a structure with\n\
the following elements:\n\
\n\
  usec  : microseconds after the second (0, 999999)\n\
  sec   : seconds after the minute (0, 61)\n\
  min   : minutes after the hour (0, 59)\n\
  hour  : hours since midnight (0, 23)\n\
  mday  : day of the month (1, 31)\n\
  mon   : months since January (0, 11)\n\
  year  : years since 1900\n\
  wday  : days since Sunday (0, 6)\n\
  yday  : days since January 1 (0, 365)\n\
  isdst : Daylight Savings Time flag\n\
  zone  : Time zone")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
	{
	  time_t timeval = static_cast<int> (tmp);
	  double ip;
	  double fraction = modf (tmp, &ip); 

	  retval = octave_value (mk_tm_map (localtime (&timeval), fraction));
	}
    }
  else
    print_usage ("localtime");

  return retval;
}

DEFUN_DLD (mktime, args, ,
  "mktime (TMSTRUCT)")
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).is_map ()) 
    {
      Octave_map map = args(0).map_value ();

      double fraction;

      struct tm *tm = extract_tm (map, fraction);

      if (! error_state)
	retval = static_cast<double> (mktime (tm)) + fraction;
    }
  else
    print_usage ("mktime");

  return retval;
}

DEFUN_DLD (strftime, args, ,
  "strftime (FMT, TMSTRUCT)\n\
\n\
Performs `%' substitutions similar to those in printf.  Except where\n\
noted, substituted fields have a fixed size; numeric fields are\n\
padded if necessary.  Padding is with zeros by default; for fields\n\
that display a single number, padding can be changed or inhibited by\n\
following the `%' with one of the modifiers described below.\n\
Unknown field specifiers are copied as normal characters.  All other\n\
characters are copied to the output without change.\n\
\n\
Supports a superset of the ANSI C field specifiers.\n\
\n\
Literal character fields:\n\
\n\
  %	%\n\
  n	newline\n\
  t	tab\n\
\n\
Numeric modifiers (a nonstandard extension):\n\
\n\
  -	do not pad the field\n\
  _	pad the field with spaces\n\
\n\
Time fields:\n\
\n\
  %H  hour (00..23)\n\
  %I  hour (01..12)\n\
  %k  hour ( 0..23)\n\
  %l  hour ( 1..12)\n\
  %M  minute (00..59)\n\
  %p  locale's AM or PM\n\
  %r  time, 12-hour (hh:mm:ss [AP]M)\n\
  %R  time, 24-hour (hh:mm)\n\
  %s  time in seconds since 00:00:00, Jan 1, 1970 (a nonstandard extension)\n\
  %S  second (00..61)\n\
  %T  time, 24-hour (hh:mm:ss)\n\
  %X  locale's time representation (%H:%M:%S)\n\
  %Z  time zone (EDT), or nothing if no time zone is determinable\n\
  %z  offset from GMT\n\
\n\
Date fields:\n\
\n\
  %a  locale's abbreviated weekday name (Sun..Sat)\n\
  %A  locale's full weekday name, variable length (Sunday..Saturday)\n\
  %b  locale's abbreviated month name (Jan..Dec)\n\
  %B  locale's full month name, variable length (January..December)\n\
  %c  locale's date and time (Sat Nov 04 12:02:33 EST 1989)\n\
  %C  century (00..99)\n\
  %d  day of month (01..31)\n\
  %e  day of month ( 1..31)\n\
  %D  date (mm/dd/yy)\n\
  %h  same as %b\n\
  %j  day of year (001..366)\n\
  %m  month (01..12)\n\
  %U  week number of year with Sunday as first day of week (00..53)\n\
  %w  day of week (0..6)\n\
  %W  week number of year with Monday as first day of week (00..53)\n\
  %x  locale's date representation (mm/dd/yy)\n\
  %y  last two digits of year (00..99)\n\
  %Y  year (1970...)")
{
  octave_value_list retval;

  if (args.length () == 2 && args(0).is_string () && args(1).is_map ()) 
    {
      string fmt = args(0).string_value ();

      Octave_map map = args(1).map_value ();

      double fraction;

      struct tm *tm = extract_tm (map, fraction);

      if (! error_state)
	{
	  const char *fmt_str = fmt.c_str ();

	  size_t bufsize = strftime (0, (size_t) UINT_MAX, fmt_str, tm);

	  char *buf = new char [++bufsize];

	  buf[0] = '\0';

	  strftime (buf, bufsize, fmt_str, tm);

	  retval = buf;

	  delete [] buf;
	}
    }
  else
    print_usage ("strftime");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
