// sun-utils.h                                           -*- C++ -*-
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

#if !defined (octave_sun_utils_h)
#define octave_sun_utils_h 1

#if defined (sun)

// I think that this is really only needed if linking to Fortran
// compiled libraries on a Sun.  It should never be called.
// There should probably be a sysdep.cc file, eh?

extern "C" int MAIN_ (void);

// This is only needed to dereference pointers to doubles if mixing
// GCC and Sun f77/cc compiled code.  See the GCC manual (where the
// function access_double() is described) and the Sun f77 manual,
// which explains that doubles are not always aligned on 8 byte
// boundaries.

#if defined (__GNUC__)

inline double
access_double (double *unaligned_ptr)
{
  union d2i { double d; int i[2]; };

  union d2i *p = (union d2i *) unaligned_ptr;
  union d2i u;

  u.i[0] = p->i[0];
  u.i[1] = p->i[1];

  return u.d;
}

inline void
assign_double (double *unaligned_ptr, double value)
{
  union d2i { double d; int i[2]; };

  double *ptr = &value;
  union d2i *v = (union d2i *) ptr;
  union d2i *p = (union d2i *) unaligned_ptr;

  p->i[0] = v->i[0];
  p->i[1] = v->i[1];
}

#endif
#endif
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
