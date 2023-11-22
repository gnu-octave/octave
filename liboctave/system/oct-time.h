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

#if ! defined (octave_oct_time_h)
#define octave_oct_time_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

#if defined (OCTAVE_USE_WINDOWS_API)
// Some Windows headers must be included in a certain order.
// Don't include "windows.h" here to avoid potential issues due to that.
// Instead just define the one type we need for the interface of one function.
struct OCTAVE_WIN_FILETIME
{
  uint32_t dwLowDateTime;
  uint32_t dwHighDateTime;
};
#endif


static inline double
as_double (OCTAVE_TIME_T sec, long usec)
{
  // Unix time will be exactly representable as a double for more than
  // 100 million years, so no worry there, and microseconds has a
  // range of 0-1e6, so we are safe there as well.

  return (static_cast<double> (sec) + static_cast<double> (usec) / 1.0e6);
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class base_tm;

class
time
{
public:

  time ()
    : m_ot_unix_time (0), m_ot_usec (0) { stamp (); }

  time (OCTAVE_TIME_T t)
    : m_ot_unix_time (t), m_ot_usec (0) { }

  time (OCTAVE_TIME_T t, long us)
    : m_ot_unix_time (t), m_ot_usec ()
  {
    long rem, extra;

    if (us >= 0)
      {
        rem = us % 1000000;
        extra = (us - rem) / 1000000;
      }
    else
      {
        us = -us;
        rem = us % 1000000;
        extra = - (1 + (us - rem) / 1000000);
        rem = 1000000 - us % 1000000;
      }

    m_ot_usec = rem;
    m_ot_unix_time += extra;
  }

  OCTAVE_API time (double d);

  OCTAVE_API time (const base_tm& tm);

  time (const time& ot)
    : m_ot_unix_time (ot.m_ot_unix_time), m_ot_usec (ot.m_ot_usec) { }

  time& operator = (const time& ot)
  {
    if (this != &ot)
      {
        m_ot_unix_time = ot.m_ot_unix_time;
        m_ot_usec = ot.m_ot_usec;
      }

    return *this;
  }

  ~time () = default;

  OCTAVE_API void stamp ();

  double double_value () const
  {
    return as_double (m_ot_unix_time, m_ot_usec);
  }

  OCTAVE_TIME_T unix_time () const { return m_ot_unix_time; }

  long usec () const { return m_ot_usec; }

  OCTAVE_API std::string ctime () const;

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const time& ot);

private:

  // Seconds since the epoch.
  OCTAVE_TIME_T m_ot_unix_time;

  // Additional microseconds.
  long m_ot_usec;
};

inline bool
operator == (const time& t1, const time& t2)
{
  return (t1.unix_time () == t2.unix_time () && t1.usec () == t2.usec ());
}

inline bool
operator != (const time& t1, const time& t2)
{
  return ! (t1 == t2);
}

inline bool
operator < (const time& t1, const time& t2)
{
  if (t1.unix_time () < t2.unix_time ())
    return true;
  else if (t1.unix_time () > t2.unix_time ())
    return false;
  else if (t1.usec () < t2.usec ())
    return true;
  else
    return false;
}

inline bool
operator <= (const time& t1, const time& t2)
{
  return (t1 < t2 || t1 == t2);
}

inline bool
operator > (const time& t1, const time& t2)
{
  if (t1.unix_time () > t2.unix_time ())
    return true;
  else if (t1.unix_time () < t2.unix_time ())
    return false;
  else if (t1.usec () > t2.usec ())
    return true;
  else
    return false;
}

inline bool
operator >= (const time& t1, const time& t2)
{
  return (t1 > t2 || t1 == t2);
}

inline time
operator + (const time& t1, const time& t2)
{
  return time (t1.unix_time () + t2.unix_time (),
               t1.usec () + t2.usec ());
}

class
base_tm
{
public:

  base_tm ()
    : m_usec (0), m_sec (0), m_min (0), m_hour (0),
      m_mday (0), m_mon (0), m_year (0), m_wday (0),
      m_yday (0), m_isdst (0), m_gmtoff (0), m_zone ("unknown")
  { }

  base_tm (const base_tm& tm)
    : m_usec (tm.m_usec), m_sec (tm.m_sec), m_min (tm.m_min),
      m_hour (tm.m_hour), m_mday (tm.m_mday), m_mon (tm.m_mon),
      m_year (tm.m_year), m_wday (tm.m_wday), m_yday (tm.m_yday),
      m_isdst (tm.m_isdst), m_gmtoff (tm.m_gmtoff), m_zone (tm.m_zone)
  { }

  base_tm& operator = (const base_tm& tm)
  {
    if (this != &tm)
      {
        m_usec = tm.m_usec;
        m_sec = tm.m_sec;
        m_min = tm.m_min;
        m_hour = tm.m_hour;
        m_mday = tm.m_mday;
        m_mon = tm.m_mon;
        m_year = tm.m_year;
        m_wday = tm.m_wday;
        m_yday = tm.m_yday;
        m_isdst = tm.m_isdst;
        m_gmtoff = tm.m_gmtoff;
        m_zone = tm.m_zone;
      }

    return *this;
  }

  virtual ~base_tm () = default;

  int usec () const { return m_usec; }
  int sec () const { return m_sec; }
  int min () const { return m_min; }
  int hour () const { return m_hour; }
  int mday () const { return m_mday; }
  int mon () const { return m_mon; }
  int year () const { return m_year; }
  int wday () const { return m_wday; }
  int yday () const { return m_yday; }
  int isdst () const { return m_isdst; }
  long gmtoff () const { return m_gmtoff; }
  std::string zone () const { return m_zone; }

  OCTAVE_API base_tm& usec (int v);
  OCTAVE_API base_tm& sec (int v);
  OCTAVE_API base_tm& min (int v);
  OCTAVE_API base_tm& hour (int v);
  OCTAVE_API base_tm& mday (int v);
  OCTAVE_API base_tm& mon (int v);
  OCTAVE_API base_tm& year (int v);
  OCTAVE_API base_tm& wday (int v);
  OCTAVE_API base_tm& yday (int v);
  OCTAVE_API base_tm& isdst (int v);
  OCTAVE_API base_tm& gmtoff (long v);
  OCTAVE_API base_tm& zone (const std::string& s);

  OCTAVE_API std::string strftime (const std::string& fmt) const;

  std::string asctime () const
  { return strftime ("%a %b %d %H:%M:%S %Y\n"); }

protected:

  // Microseconds after the second (0, 999999).
  int m_usec;

  // Seconds after the minute (0, 61).
  int m_sec;

  // Minutes after the hour (0, 59).
  int m_min;

  // Hours since midnight (0, 23).
  int m_hour;

  // Day of the month (1, 31).
  int m_mday;

  // Months since January (0, 11).
  int m_mon;

  // Years since 1900.
  int m_year;

  // Days since Sunday (0, 6).
  int m_wday;

  // Days since January 1 (0, 365).
  int m_yday;

  // Daylight saving time flag.
  int m_isdst;

  // Time zone.
  long m_gmtoff;

  // Time zone.
  std::string m_zone;

  OCTAVE_API void init (void *p);
};

class
localtime : public base_tm
{
public:

  localtime ()
    : base_tm () { init (time ()); }

  localtime (const time& ot)
    : base_tm () { init (ot); }

  localtime (const localtime& t)
    : base_tm (t) { }

  localtime& operator = (const localtime& t)
  {
    base_tm::operator = (t);
    return *this;
  }

  ~localtime () = default;

private:

  OCTAVE_API void init (const time& ot);
};

class
gmtime : public base_tm
{
public:

  gmtime ()
    : base_tm () { init (time ()); }

  gmtime (const time& ot)
    : base_tm () { init (ot); }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (gmtime)

private:

  OCTAVE_API void init (const time& ot);
};

class
strptime : public base_tm
{
public:

  strptime () = delete;

  strptime (const std::string& str, const std::string& fmt)
    : base_tm (), m_nchars (0)
  {
    init (str, fmt);
  }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (strptime)

  int characters_converted () const { return m_nchars; }

private:

  int m_nchars;

  OCTAVE_API void init (const std::string& str, const std::string& fmt);
};

class
cpu_time
{
public:

  friend class resource_usage;

  cpu_time ()
    : m_usr_sec (0), m_sys_sec (0), m_usr_usec (0), m_sys_usec (0)
  {
    stamp ();
  }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (cpu_time)

  OCTAVE_API void stamp ();

  double user () const
  {
    return as_double (m_usr_sec, m_usr_usec);
  }

  double system () const
  {
    return as_double (m_sys_sec, m_sys_usec);
  }

  OCTAVE_TIME_T user_sec () const { return m_usr_sec; }
  long user_usec () const { return m_usr_usec; }

  OCTAVE_TIME_T system_sec () const { return m_sys_sec; }
  long system_usec () const { return m_sys_usec; }

private:

  OCTAVE_TIME_T m_usr_sec;
  OCTAVE_TIME_T m_sys_sec;

  long m_usr_usec;
  long m_sys_usec;

  cpu_time (OCTAVE_TIME_T usr_sec, OCTAVE_TIME_T sys_sec, long usr_usec, long sys_usec)
    : m_usr_sec (usr_sec), m_sys_sec (sys_sec),
      m_usr_usec (usr_usec), m_sys_usec (sys_usec)
  { }
};

class
resource_usage
{
public:

  resource_usage ()
    : m_cpu (), m_maxrss (0), m_ixrss (0), m_idrss (0),
      m_isrss (0), m_minflt (0), m_majflt (0), m_nswap (0),
      m_inblock (0), m_oublock (0), m_msgsnd (0), m_msgrcv (0),
      m_nsignals (0), m_nvcsw (0), m_nivcsw (0)
  {
    stamp ();
  }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (resource_usage)

  OCTAVE_API void stamp ();

  cpu_time cpu () const { return m_cpu; }

  long maxrss () const { return m_maxrss; }
  long ixrss () const { return m_ixrss; }
  long idrss () const { return m_idrss; }
  long isrss () const { return m_isrss; }
  long minflt () const { return m_minflt; }
  long majflt () const { return m_majflt; }
  long nswap () const { return m_nswap; }
  long inblock () const { return m_inblock; }
  long oublock () const { return m_oublock; }
  long msgsnd () const { return m_msgsnd; }
  long msgrcv () const { return m_msgrcv; }
  long nsignals () const { return m_nsignals; }
  long nvcsw () const { return m_nvcsw; }
  long nivcsw () const { return m_nivcsw; }

private:

  cpu_time m_cpu;

  long m_maxrss;
  long m_ixrss;
  long m_idrss;
  long m_isrss;
  long m_minflt;
  long m_majflt;
  long m_nswap;
  long m_inblock;
  long m_oublock;
  long m_msgsnd;
  long m_msgrcv;
  long m_nsignals;
  long m_nvcsw;
  long m_nivcsw;
};

// class to handle file time efficiently on different platforms

class OCTAVE_API file_time
{
public:

  file_time ();

  file_time (OCTAVE_TIME_T t)
    : m_time (t)
  { }

#if defined (OCTAVE_USE_WINDOWS_API)
  file_time (OCTAVE_WIN_FILETIME& t)
  {
    m_time = (static_cast<OCTAVE_TIME_T> (t.dwHighDateTime)) >> 32
             | t.dwLowDateTime;
  }
#endif

  file_time (const std::string& filename);

  file_time (const file_time& ot)
  {
    m_time = ot.time ();
  }

  file_time& operator = (const file_time& ot)
  {
    if (this != &ot)
      m_time = ot.time ();

    return *this;
  }

  ~file_time () = default;

  inline static file_time time_resolution ()
  {
#if defined (OCTAVE_USE_WINDOWS_API)
    // FAT file systems have 2 seconds resolution for the modification time.
    static OCTAVE_TIME_T time_resolution = 20000;
#else
    // Assume 1 second (see file_stat)
    static OCTAVE_TIME_T time_resolution = 1;
#endif
    return time_resolution;
  }

  inline bool
  operator == (const file_time& t2) const
  {
    return time () == t2.time ();
  }

  inline bool
  operator != (const file_time& t2) const
  {
    return ! (*this == t2);
  }

  inline bool
  operator < (const file_time& t2) const
  {
    return time () < t2.time ();
  }

  inline bool
  operator <= (const file_time& t2) const
  {
    return (*this < t2 || *this == t2);
  }

  inline bool
  operator > (const file_time& t2) const
  {
    return time () > t2.time ();
  }

  inline bool
  operator >= (const file_time& t2) const
  {
    return (*this > t2 || *this == t2);
  }

  inline file_time
  operator + (const file_time& t2) const
  {
    return file_time (time () + t2.time ());
  }

  inline file_time
  operator + (const OCTAVE_TIME_T t2) const
  {
    return file_time (time () + t2);
  }

  OCTAVE_TIME_T time () const { return m_time; }

private:

  // The native file time type differs per platform.
  // On POSIX, this is the number of 1 second intervals since the epoch.
  // On Windows, this is the number of 0.1 ms intervals since a different epoch.
  OCTAVE_TIME_T m_time;
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
