// sysdep.cc                                              -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdlib.h>

#include "error.h"
#include "sysdep.h"

// Octave's idea of infinity.
double octave_Inf;

// Octave's idea of not a number.
double octave_NaN;

#if defined (__386BSD__) && defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#ifdef NeXT
extern "C"
{
  typedef void (*_cplus_fcn_int) (int);
  extern void (*malloc_error (_cplus_fcn_int)) (int);
}

static void
malloc_handler (int code)
{
  if (code == 5)
    warning ("hopefully recoverable malloc error: freeing wild pointer");
  else
    {
      panic ("probably irrecoverable malloc error: code %d", code);
    }
}

static void
NeXT_init (void)
{
  malloc_error (malloc_handler);
}
#endif

static void
octave_ieee_init (void)
{
#if defined (HAVE_ISINF) || defined (HAVE_FINITE)

// Some version of gcc on some old version of Linux used to crash when
// trying to make Inf and NaN.

#if defined (HAVE_INFINITY)
  octave_Inf = (double) infinity ();
#else
#ifdef linux
  octave_Inf = HUGE_VAL;
#else
  double tmp = 1e+10;
  octave_Inf = tmp;
  for (;;)
    {
      octave_Inf *= 1e+10;
      if (octave_Inf == tmp)
	break;
      tmp = octave_Inf;
    }
#endif
#endif

#if defined (HAVE_QUIET_NAN)
  octave_NaN = (double) quiet_nan ();
#else
#ifdef linux
  octave_NaN = NAN;
#else
  octave_NaN = octave_Inf / octave_Inf;
#endif
#endif

#else

// This is sort of cheesy, but what can we do, other than blowing it
// off completely, or writing an entire IEEE emulation package?

  octave_Inf = DBL_MAX;
  octave_NaN = DBL_MAX;

#endif
}


#if defined (EXCEPTION_IN_MATH)
extern "C"
{
int
matherr (struct exception *x)
{
// Possibly print our own message someday.  Should probably be
// user-switchable.

  switch (x->type)
    {
    case DOMAIN:
    case SING:
    case OVERFLOW:
    case UNDERFLOW:
    case TLOSS:
    case PLOSS:
    default:
      break;
    }

// But don't print the system message.

  return 1;
}
}
#endif

void
sysdep_init (void)
{
#if defined (__386BSD__) && defined (HAVE_FLOATINGPOINT_H)
// Disable trapping on common exceptions.
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#endif

#ifdef NeXT
  NeXT_init ();
#endif

  octave_ieee_init ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
