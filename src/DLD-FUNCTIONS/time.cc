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

#include "defun-dld.h"
#include "error.h"
#include "oct-map.h"
#include "oct-time.h"
#include "ov.h"
#include "oct-obj.h"

// Date and time functions.

static Octave_map
mk_tm_map (const octave_base_tm& t)
{
  Octave_map m;

  m ["usec"] = static_cast<double> (t.usec ());
  m ["sec"] = static_cast<double> (t.sec ());
  m ["min"] = static_cast<double> (t.min ());
  m ["hour"] = static_cast<double> (t.hour ());
  m ["mday"] = static_cast<double> (t.mday ());
  m ["mon"] = static_cast<double> (t.mon ());
  m ["year"] = static_cast<double> (t.year ());
  m ["wday"] = static_cast<double> (t.wday ());
  m ["yday"] = static_cast<double> (t.yday ());
  m ["isdst"] = static_cast<double> (t.isdst ());
  m ["zone"]  = t.zone ();

  return m;
}

static octave_base_tm
extract_tm (Octave_map &m)
{
  octave_base_tm tm;

  tm.usec (static_cast<int> (m ["usec"] . double_value ()));
  tm.sec (static_cast<int> (m ["sec"] . double_value ()));
  tm.min (static_cast<int> (m ["min"] . double_value ()));
  tm.hour (static_cast<int> (m ["hour"] . double_value ()));
  tm.mday (static_cast<int> (m ["mday"] . double_value ()));
  tm.mon (static_cast<int> (m ["mon"] . double_value ()));
  tm.year (static_cast<int> (m ["year"] . double_value ()));
  tm.wday (static_cast<int> (m ["wday"] . double_value ()));
  tm.yday (static_cast<int> (m ["yday"] . double_value ()));
  tm.isdst (static_cast<int> (m ["isdst"] . double_value ()));
  tm.zone (m ["zone"] . string_value ());

  return tm;
}

DEFUN_DLD (time, args, ,
  "time ()\n\
\n\
Return current time.  On Unix systems, this is the number of\n\
seconds since the epoch.")
{
  octave_value retval;

  if (args.length () == 0)
    retval = static_cast<double> (octave_time ());
  else
    print_usage ("time");

  return retval;
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
	retval = octave_value (mk_tm_map (octave_gmtime (tmp)));
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
  isdst : daylight savings time flag\n\
  zone  : time zone")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
	retval = octave_value (mk_tm_map (octave_localtime (tmp)));
    }
  else
    print_usage ("localtime");

  return retval;
}

DEFUN_DLD (mktime, args, ,
  "mktime (TMSTRUCT)")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      Octave_map map = args(0).map_value ();

      if (! error_state)
	{
	  octave_base_tm tm = extract_tm (map);

	  if (! error_state)
	    retval = static_cast<double> (octave_time (tm));
	  else
	    error ("mktime: invalid TMSTRUCT argument");
	}
      else
	error ("mktime: expecting structure argument");
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

  if (args.length () == 2)
    {
      string fmt = args(0).string_value ();

      if (! error_state)
	{
	  Octave_map map = args(1).map_value ();

	  if (! error_state)
	    {
	      octave_base_tm tm = extract_tm (map);

	      if (! error_state)
		retval = tm.strftime (fmt);
	      else
		error ("strftime: invalid TMSTRUCT argument");
	    }
	  else
	    error ("strftime: expecting structure as second argument");
	}
      else
	error ("strftime: expecting format string as first argument");
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
