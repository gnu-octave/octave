// timefns.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#include "defun.h"
#include "help.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "systime.h"
#include "tree-const.h"
#include "utils.h"

// Date and time functions.

static Octave_map
mk_tm_map (struct tm *tm, double fraction)
{
  Octave_map m;

  m ["tm_usec"] = fraction * 1e6;
  m ["tm_sec"] = (double) tm->tm_sec;
  m ["tm_min"] = (double) tm->tm_min;
  m ["tm_hour"] = (double) tm->tm_hour;
  m ["tm_mday"] = (double) tm->tm_mday;
  m ["tm_mon"] = (double) tm->tm_mon;
  m ["tm_year"] = (double) tm->tm_year;
  m ["tm_wday"] = (double) tm->tm_wday;
  m ["tm_yday"] = (double) tm->tm_yday;
  m ["tm_isdst"] = (double) tm->tm_isdst;
#if defined (HAVE_TM_ZONE)
  m ["tm_zone"]  = tm->tm_zone;
#elif defined (HAVE_TZNAME)
  if (tm->tm_isdst && tzname[1] && *tzname[1])
    m ["tm_zone"] = tzname[1];
  else
    m ["tm_zone"] = tzname[0];
#else
  m ["tm_zone"] = zone_name (tm);
#endif

  return m;
}

static struct tm*
extract_tm (Octave_map &m, double& fraction)
{
  static struct tm tm;

  fraction = (m ["tm_usec"] . double_value ()) / 1e6;
  tm.tm_sec = NINT (m ["tm_sec"] . double_value ());
  tm.tm_min = NINT (m ["tm_min"] . double_value ());
  tm.tm_hour = NINT (m ["tm_hour"] . double_value ());
  tm.tm_mday = NINT (m ["tm_mday"] . double_value ());
  tm.tm_mon = NINT (m ["tm_mon"] . double_value ());
  tm.tm_year = NINT (m ["tm_year"] . double_value ());
  tm.tm_wday = NINT (m ["tm_wday"] . double_value ());
  tm.tm_yday = NINT (m ["tm_yday"] . double_value ());
  tm.tm_isdst = NINT (m ["tm_isdst"] . double_value ());
#ifdef HAVE_TMZONE
  tm.tm_zone = (m ["tm_zone"] . string_value ());
#endif

  return &tm;
}

DEFUN ("time", Ftime, Stime, 1, 0,
  "time ()\n\
\n\
  Return current time.  On Unix systems, this is the number of\n\
  seconds since the epoch.")
{
  time_t now;
  double fraction = 0.0;

#ifdef HAVE_GETTIMEOFDAY

  struct timeval tp;

#ifdef GETTIMEOFDAY_NO_TZ
  gettimeofday (&tp);
#else
  gettimeofday (&tp, 0);
#endif

  now = tp.tv_sec;

  fraction = tp.tv_usec / 1e6;

#else

  now = time (0);

#endif
 
  return (double) now + fraction;
}

DEFUN ("gmtime", Fgmtime, Sgmtime, 1, 1,
  "gmtime (TIME)\n\
\n\
  Given a value returned from time(), return a structure like that\n\
  returned from localtime() but with values corresponding to\n\
  Coordinated Universal Time (UTC).")
{
  Octave_object retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
	{
	  time_t timeval = NINT (tmp);
	  double ip;
	  double fraction = modf (tmp, &ip); 

	  retval = tree_constant (mk_tm_map (gmtime (&timeval), fraction));
	}
    }
  else
    print_usage ("gmtime");

  return retval;
}

DEFUN ("localtime", Flocaltime, Slocaltime, 1, 1,
  "localtime (TIME)\n\
\n\
  Given a value returned from time(), return a structure with\n\
  the following elements:\n\
\n\
    tm_usec  : microseconds after the second (0, 999999)\n\
    tm_sec   : seconds after the minute (0, 61)\n\
    tm_min   : minutes after the hour (0, 59)\n\
    tm_hour  : hours since midnight (0, 23)\n\
    tm_mday  : day of the month (1, 31)\n\
    tm_mon   : months since January (0, 11)\n\
    tm_year  : years since 1900\n\
    tm_wday  : days since Sunday (0, 6)\n\
    tm_yday  : days since January 1 (0, 365)\n\
    tm_isdst : Daylight Savings Time flag\n\
    tm_zone  : Time zone")
{
  Octave_object retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
	{
	  time_t timeval = NINT (tmp);
	  double ip;
	  double fraction = modf (tmp, &ip); 

	  retval = tree_constant (mk_tm_map (localtime (&timeval), fraction));
	}
    }
  else
    print_usage ("localtime");

  return retval;
}

DEFUN ("mktime", Fmktime, Smktime, 1, 2,
  "mktime (TMSTRUCT)")
{
  Octave_object retval;

  if (args.length () == 1 && args(0).is_map ()) 
    {
      Octave_map map = args(0).map_value ();

      double fraction;

      struct tm *tm = extract_tm (map, fraction);

      if (! error_state)
	retval = (double) mktime (tm) + fraction;
    }
  else
    print_usage ("mktime");

  return retval;
}

DEFUN ("strftime", Fstrftime, Sstrftime, 1, 2,
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
    %H	hour (00..23)\n\
    %I	hour (01..12)\n\
    %k	hour ( 0..23)\n\
    %l	hour ( 1..12)\n\
    %M	minute (00..59)\n\
    %p	locale's AM or PM\n\
    %r	time, 12-hour (hh:mm:ss [AP]M)\n\
    %R	time, 24-hour (hh:mm)\n\
    %s	time in seconds since 00:00:00, Jan 1, 1970 (a nonstandard extension)\n\
    %S	second (00..61)\n\
    %T	time, 24-hour (hh:mm:ss)\n\
    %X	locale's time representation (%H:%M:%S)\n\
    %Z	time zone (EDT), or nothing if no time zone is determinable\n\
\n\
  Date fields:\n\
\n\
    %a	locale's abbreviated weekday name (Sun..Sat)\n\
    %A	locale's full weekday name, variable length (Sunday..Saturday)\n\
    %b	locale's abbreviated month name (Jan..Dec)\n\
    %B	locale's full month name, variable length (January..December)\n\
    %c	locale's date and time (Sat Nov 04 12:02:33 EST 1989)\n\
    %C	century (00..99)\n\
    %d	day of month (01..31)\n\
    %e	day of month ( 1..31)\n\
    %D	date (mm/dd/yy)\n\
    %h	same as %b\n\
    %j	day of year (001..366)\n\
    %m	month (01..12)\n\
    %U	week number of year with Sunday as first day of week (00..53)\n\
    %w	day of week (0..6)\n\
    %W	week number of year with Monday as first day of week (00..53)\n\
    %x	locale's date representation (mm/dd/yy)\n\
    %y	last two digits of year (00..99)\n\
    %Y	year (1970...)")
{
  Octave_object retval;

  if (args.length () == 2 && args(0).is_string () && args(1).is_map ()) 
    {
      char *fmt = args(0).string_value ();
      Octave_map map = args(1).map_value ();

      double fraction;

      struct tm *tm = extract_tm (map, fraction);

      if (! error_state)
	{
	  int bufsize = 128;
	  char *buf = new char [bufsize];

	  while (! strftime (buf, bufsize, fmt, tm))
	    {
	      delete [] buf;
	      bufsize *= 2;
	      buf = new char [bufsize];
	    }

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
