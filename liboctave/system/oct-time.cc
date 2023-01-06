////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2023 The Octave Project Developers
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

#include <cmath>
#include <ctime>

#include <iomanip>
#include <limits>
#include <ostream>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#endif

#include "lo-error.h"
#include "lo-utils.h"
#include "lo-sysdep.h"
#include "oct-locbuf.h"
#include "oct-time.h"
#include "octave-preserve-stream-state.h"
#include "strftime-wrapper.h"
#include "strptime-wrapper.h"
#include "time-wrappers.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

time::time (double d)
  : m_ot_unix_time (static_cast<OCTAVE_TIME_T> (d)), m_ot_usec (0)
{
  double ip;
  m_ot_usec = static_cast<int> (std::modf (d, &ip) * 1e6);
}

time::time (const base_tm& tm)
  : m_ot_unix_time (), m_ot_usec ()
{
  struct ::tm t;

  t.tm_sec = tm.sec ();
  t.tm_min = tm.min ();
  t.tm_hour = tm.hour ();
  t.tm_mday = tm.mday ();
  t.tm_mon = tm.mon ();
  t.tm_year = tm.year ();
  t.tm_wday = tm.wday ();
  t.tm_yday = tm.yday ();
  t.tm_isdst = tm.isdst ();

#if defined (HAVE_TM_GMTOFF)
  t.tm_gmtoff = tm.gmtoff ();
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  std::string s = tm.zone ();
  char *ps = strsave (s.c_str ());
  t.tm_zone = ps;
#endif

  m_ot_unix_time = octave_mktime_wrapper (&t);

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  delete [] ps;
#endif

  m_ot_usec = tm.usec ();
}

std::string
time::ctime (void) const
{
  return localtime (*this).asctime ();
}

std::ostream&
operator << (std::ostream& os, const time& ot)
{
  preserve_stream_state stream_state (os);

  os << ot.m_ot_unix_time << '.'
     << std::setw (6) << std::setfill ('0') << ot.m_ot_usec;

  return os;
}

void
time::stamp (void)
{
  time_t ot_unix_time;
  octave_gettimeofday_wrapper (&ot_unix_time, &m_ot_usec);
  m_ot_unix_time = ot_unix_time;
}

// From the mktime() manual page:
//
//     The mktime() function converts a broken-down time structure,
//     expressed as local time, to calendar time representation.
//
//     <snip>
//
//     If structure members are outside their legal interval, they
//     will be normalized (so that, e.g., 40 October is changed into
//     9 November).
//
// So, we no longer check limits here.

#define DEFINE_SET_FIELD_FCN(type, f, lo, hi)   \
    base_tm&                                    \
    base_tm::f (type v)                         \
    {                                           \
      m_ ## f = v;                              \
                                                \
      return *this;                             \
    }

#define DEFINE_SET_INT_FIELD_FCN(f, lo, hi)     \
    DEFINE_SET_FIELD_FCN (int, f, lo, hi)

DEFINE_SET_INT_FIELD_FCN (usec, 0, 1000000)
DEFINE_SET_INT_FIELD_FCN (sec, 0, 61)
DEFINE_SET_INT_FIELD_FCN (min, 0, 59)
DEFINE_SET_INT_FIELD_FCN (hour, 0, 23)
DEFINE_SET_INT_FIELD_FCN (mday, 1, 31)
DEFINE_SET_INT_FIELD_FCN (mon, 0, 11)
DEFINE_SET_INT_FIELD_FCN (year, std::numeric_limits<int>::min (),
                          std::numeric_limitd<int>::max ())
DEFINE_SET_INT_FIELD_FCN (wday, 0, 6)
DEFINE_SET_INT_FIELD_FCN (yday, 0, 365)
DEFINE_SET_INT_FIELD_FCN (isdst, 0, 1)
DEFINE_SET_FIELD_FCN (long, gmtoff, -86400, 0)

base_tm&
base_tm::zone (const std::string& s)
{
  m_zone = s;
  return *this;
}

#if ! defined STRFTIME_BUF_INITIAL_SIZE
#  define STRFTIME_BUF_INITIAL_SIZE 128
#endif

std::string
base_tm::strftime (const std::string& fmt) const
{
  std::string retval;

  if (! fmt.empty ())
    {
      struct ::tm t;

      t.tm_sec = m_sec;
      t.tm_min = m_min;
      t.tm_hour = m_hour;
      t.tm_mday = m_mday;
      t.tm_mon = m_mon;
      t.tm_year = m_year;
      t.tm_wday = m_wday;
      t.tm_yday = m_yday;
      t.tm_isdst = m_isdst;

#if defined (HAVE_TM_GMTOFF)
      t.tm_gmtoff = m_gmtoff;
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
      char *ps = strsave (m_zone.c_str ());
      t.tm_zone = ps;
#endif

      const char *fmt_str = fmt.c_str ();

      char *buf = nullptr;
      std::size_t bufsize = STRFTIME_BUF_INITIAL_SIZE;
      std::size_t chars_written = 0;

      while (chars_written == 0)
        {
          delete [] buf;
          buf = new char [bufsize];
          buf[0] = '\0';

          chars_written
            = octave_strftime_wrapper (buf, bufsize, fmt_str, &t);

          bufsize *= 2;
        }

#if defined (HAVE_STRUCT_TM_TM_ZONE)
      delete [] ps;
#endif

      retval = buf;

      delete [] buf;
    }

  return retval;
}

void
base_tm::init (void *p)
{
  if (! p)
    return;

  struct ::tm *t = static_cast<struct ::tm *> (p);

  m_sec = t->tm_sec;
  m_min = t->tm_min;
  m_hour = t->tm_hour;
  m_mday = t->tm_mday;
  m_mon = t->tm_mon;
  m_year = t->tm_year;
  m_wday = t->tm_wday;
  m_yday = t->tm_yday;
  m_isdst = t->tm_isdst;

#if defined (HAVE_TM_GMTOFF)
  m_gmtoff = t->tm_gmtoff;
#elif defined (OCTAVE_USE_WINDOWS_API)
  TIME_ZONE_INFORMATION tzi;

  GetTimeZoneInformationForYear (m_year, nullptr, &tzi);

  if (m_isdst)
    m_gmtoff = -60 * (tzi.Bias + tzi.DaylightBias);
  else
    m_gmtoff = -60 * (tzi.Bias + tzi.StandardBias);
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  if (t->tm_zone)
    m_zone = t->tm_zone;
#elif defined (OCTAVE_USE_WINDOWS_API)
  if (m_isdst)
    m_zone = sys::u8_from_wstring (tzi.DaylightName);
  else
    m_zone = sys::u8_from_wstring (tzi.StandardName);
#elif defined (HAVE_TZNAME)
  if (t->tm_isdst == 0 || t->tm_isdst == 1)
    m_zone = tzname[t->tm_isdst];
#endif
}

void
localtime::init (const time& ot)
{
  m_usec = ot.usec ();

  time_t t = ot.unix_time ();

  base_tm::init (std::localtime (&t));
}

void
gmtime::init (const time& ot)
{
  m_usec = ot.usec ();

  time_t t = ot.unix_time ();

  base_tm::init (std::gmtime (&t));
}

void
strptime::init (const std::string& str, const std::string& fmt)
{
  struct ::tm t;

  t.tm_sec = 0;
  t.tm_min = 0;
  t.tm_hour = 0;
  t.tm_mday = 0;
  t.tm_mon = -1;
  t.tm_year = std::numeric_limits<int>::min ();
  t.tm_wday = 0;
  t.tm_yday = 0;
  t.tm_isdst = 0;

#if defined (HAVE_TM_GMTOFF)
  t.tm_gmtoff = 0;
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  char *ps = strsave ("");
  t.tm_zone = ps;
#endif

  const char *p = str.c_str ();

  char *q = octave_strptime_wrapper (p, fmt.c_str (), &t);

  // Fill in wday and yday, but only if mday is valid and the mon and year
  // are filled in, avoiding issues with mktime and invalid dates.
  if (t.tm_mday != 0 && t.tm_mon >= 0
      && t.tm_year != std::numeric_limits<int>::min ())
    {
      t.tm_isdst = -1;
      octave_mktime_wrapper (&t);
    }

  if (t.tm_mon < 0)
    t.tm_mon = 0;

  if (t.tm_year == std::numeric_limits<int>::min ())
    t.tm_year = 0;

  if (q)
    m_nchars = q - p + 1;
  else
    m_nchars = 0;

  base_tm::init (&t);

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  delete [] ps;
#endif
}

void
cpu_time::stamp (void)
{
  time_t usr_sec, sys_sec;
  octave_cpu_time (&usr_sec, &sys_sec, &m_usr_usec, &m_sys_usec);
  m_usr_sec = usr_sec;
  m_sys_sec = sys_sec;
}

void
resource_usage::stamp (void)
{
  time_t usr_sec, sys_sec;
  long usr_usec, sys_usec;

  octave_getrusage_wrapper (&usr_sec, &sys_sec, &usr_usec,
                            &sys_usec, &m_maxrss, &m_ixrss,
                            &m_idrss, &m_isrss, &m_minflt,
                            &m_majflt, &m_nswap, &m_inblock,
                            &m_oublock, &m_msgsnd, &m_msgrcv,
                            &m_nsignals, &m_nvcsw, &m_nivcsw);

  m_cpu = cpu_time (usr_sec, sys_sec, usr_usec, sys_usec);
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
