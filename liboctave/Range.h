// Range.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (_Range_h)
#define _Range_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include <iostream.h>

class Range
{
 public:
  Range (void);
  Range (const Range& r);
  Range (double b, double l);
  Range (double b, double l, double i);

  double base (void) const;
  double limit (void) const;
  double inc (void) const;
  int nelem (void) const;

  double min (void) const;
  double max (void) const;

  void set_base (double b);
  void set_limit (double l);
  void set_inc (double i);

  friend ostream& operator << (ostream& os, const Range& r);
  friend istream& operator >> (istream& is, Range& r);

  void print_range (void);

 private:
  double _base;
  double _limit;
  double _inc;
  int _nelem;

  int nelem_internal (void) const;
};

inline Range::Range (void)
  { _base = -1; _limit = -1; _inc = -1; _nelem = -1; }

inline Range::Range (const Range& r)
  { _base = r._base; _limit = r._limit; _inc = r._inc; _nelem = r._nelem; }

inline Range::Range (double b, double l)
  { _base = b; _limit = l; _inc = 1; _nelem = nelem_internal (); }

inline Range::Range (double b, double l, double i)
  { _base = b; _limit = l; _inc = i; _nelem = nelem_internal (); }

inline double Range::base (void) const { return _base;  }
inline double Range::limit (void) const { return _limit; }
inline double Range::inc (void) const { return _inc;   }
inline int Range::nelem (void) const { return _nelem; }

inline void Range::set_base (double b) { _base = b;  }
inline void Range::set_limit (double l) { _limit = l; }
inline void Range::set_inc (double i) { _inc = i;   }

// NOTE: max and min only return useful values if nelem > 0.

inline double Range::min (void) const { return _inc > 0 ? _base : _limit; }
inline double Range::max (void) const { return _inc > 0 ? _limit : _base; }

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
