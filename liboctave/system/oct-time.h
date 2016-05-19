/*

Copyright (C) 1999-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_time_h)
#define octave_oct_time_h 1

#include "octave-config.h"

#include <ctime>
#include <string>

namespace octave
{
  namespace sys
  {
    class base_tm;

    class
    OCTAVE_API
    time
    {
    public:

      time (void)
        : ot_unix_time (0), ot_usec (0) { stamp (); }

      time (time_t t)
        : ot_unix_time (t), ot_usec (0) { }

      time (time_t t, int us)
        : ot_unix_time (t), ot_usec ()
        {
          int rem, extra;

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

          ot_usec = rem;
          ot_unix_time += extra;
        }

      time (double d);

      time (const base_tm& tm);

      time (const time& ot)
        : ot_unix_time (ot.ot_unix_time), ot_usec (ot.ot_usec) { }

      time& operator = (const time& ot)
        {
          if (this != &ot)
            {
              ot_unix_time = ot.ot_unix_time;
              ot_usec = ot.ot_usec;
            }

          return *this;
        }

      ~time (void) { }

      void stamp (void);

      double double_value (void) const { return ot_unix_time + ot_usec / 1e6; }

      time_t unix_time (void) const { return ot_unix_time; }

      int usec (void) const { return ot_usec; }

      std::string ctime (void) const;

    private:

      // Seconds since the epoch.
      time_t ot_unix_time;

      // Additional microseconds.
      int ot_usec;
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
    OCTAVE_API
    base_tm
    {
    public:

      base_tm (void)
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

      virtual ~base_tm (void) { }

      int usec (void) const { return m_usec; }
      int sec (void) const { return m_sec; }
      int min (void) const { return m_min; }
      int hour (void) const { return m_hour; }
      int mday (void) const { return m_mday; }
      int mon (void) const { return m_mon; }
      int year (void) const { return m_year; }
      int wday (void) const { return m_wday; }
      int yday (void) const { return m_yday; }
      int isdst (void) const { return m_isdst; }
      long gmtoff (void) const { return m_gmtoff; }
      std::string zone (void) const { return m_zone; }

      base_tm& usec (int v);
      base_tm& sec (int v);
      base_tm& min (int v);
      base_tm& hour (int v);
      base_tm& mday (int v);
      base_tm& mon (int v);
      base_tm& year (int v);
      base_tm& wday (int v);
      base_tm& yday (int v);
      base_tm& isdst (int v);
      base_tm& gmtoff (long v);
      base_tm& zone (const std::string& s);

      std::string strftime (const std::string& fmt) const;

      std::string asctime (void) const
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

      // Daylight Savings Time flag.
      int m_isdst;

      // Time zone.
      long m_gmtoff;

      // Time zone.
      std::string m_zone;

      void init (void *p);
    };

    class
    OCTAVE_API
    localtime : public base_tm
    {
    public:

      localtime (void)
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

      ~localtime (void) { }

    private:

      void init (const time& ot);
    };

    class
    OCTAVE_API
    gmtime : public base_tm
    {
    public:

      gmtime (void)
        : base_tm () { init (time ()); }

      gmtime (const time& ot)
        : base_tm () { init (ot); }

      gmtime& operator = (const gmtime& t)
        {
          base_tm::operator = (t);
          return *this;
        }

      ~gmtime (void) { }

    private:

      void init (const time& ot);
    };

    class
    OCTAVE_API
    strptime : public base_tm
    {
    public:

      strptime (const std::string& str, const std::string& fmt)
        : base_tm (), nchars (0)
        {
          init (str, fmt);
        }

      strptime (const strptime& s)
        : base_tm (s), nchars (s.nchars) { }

      strptime& operator = (const strptime& s)
        {
          base_tm::operator = (s);
          nchars = s.nchars;
          return *this;
        }

      int characters_converted (void) const { return nchars; }

      ~strptime (void) { }

    private:

      int nchars;

      void init (const std::string& str, const std::string& fmt);
    };
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use octave::sys::time instead")
typedef octave::sys::time octave_time;

OCTAVE_DEPRECATED ("use octave::sys::base_tm instead")
typedef octave::sys::base_tm octave_base_tm;

OCTAVE_DEPRECATED ("use octave::sys::localtime instead")
typedef octave::sys::localtime octave_localtime;

OCTAVE_DEPRECATED ("use octave::sys::gmtime instead")
typedef octave::sys::gmtime octave_gmtime;

OCTAVE_DEPRECATED ("use octave::sys::strptime instead")
typedef octave::sys::strptime octave_strptime;

#endif

#endif
