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

#include "f77-fcn.h"
#include "lo-error.h"
#include "mach-info.h"

extern "C"
{
  double F77_FCN (d1mach, D1MACH) (const int&);
}

oct_mach_info *oct_mach_info::instance = 0;

union equiv
{
  double d;
  int i[2];
};

struct
float_params
{
  oct_mach_info::float_format fp_fmt;
  equiv fp_par[4];
};

#define INIT_FLT_PAR(fp, fmt, sm1, sm2, lrg1, lrg2, rt1, rt2, dv1, dv2) \
  do \
    { \
      fp.fp_fmt = (fmt); \
      fp.fp_par[0].i[0] = (sm1);  fp.fp_par[0].i[1] = (sm2); \
      fp.fp_par[1].i[0] = (lrg1); fp.fp_par[1].i[1] = (lrg2); \
      fp.fp_par[2].i[0] = (rt1);  fp.fp_par[2].i[1] = (rt2); \
      fp.fp_par[3].i[0] = (dv1);  fp.fp_par[3].i[1] = (dv2); \
    } \
  while (0)

static int
equiv_compare (const equiv *std, const equiv *v, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (v[i].i[0] != std[i].i[0] || v[i].i[1] != std[i].i[1])
      return 0;
  return 1;
}

void
oct_mach_info::init_float_format (void)
{
  float_params fp[5];

  INIT_FLT_PAR (fp[0], oct_mach_info::ieee_big_endian,
		   1048576,  0,
		2146435071, -1,
		1017118720,  0,
		1018167296,  0);

  INIT_FLT_PAR (fp[1], oct_mach_info::ieee_little_endian,
		 0,    1048576,
		-1, 2146435071,
		 0, 1017118720,
		 0, 1018167296);

  INIT_FLT_PAR (fp[2], oct_mach_info::vax_d,
		   128,  0,
		-32769, -1,
		  9344,  0,
		  9344,  0);

  INIT_FLT_PAR (fp[3], oct_mach_info::vax_g,
		    16,  0,
		-32769, -1,
		 15552,  0,
		 15552,  0);

  INIT_FLT_PAR (fp[4], oct_mach_info::unknown,
		0, 0,
		0, 0,
		0, 0,
		0, 0);

  equiv mach_fp_par[4];

  mach_fp_par[0].d = F77_FCN (d1mach, D1MACH) (1);
  mach_fp_par[1].d = F77_FCN (d1mach, D1MACH) (2);
  mach_fp_par[2].d = F77_FCN (d1mach, D1MACH) (3);
  mach_fp_par[3].d = F77_FCN (d1mach, D1MACH) (4);

  int i = 0;
  do
    {
      if (equiv_compare (fp[i].fp_par, mach_fp_par, 4))
	{
	  native_float_fmt = fp[i].fp_fmt;
	  break;
	}
    }
  while (fp[++i].fp_fmt != oct_mach_info::unknown);
}

void
oct_mach_info::ten_little_endians (void)
{
  // Are we little or big endian?  From Harbison & Steele.

  union
  {
    long l;
    char c[sizeof (long)];
  } u;

  u.l = 1;

  big_chief = (u.c[sizeof (long) - 1] == 1);
}

oct_mach_info::oct_mach_info (void)
{
  init_float_format ();
  ten_little_endians ();
}

oct_mach_info::float_format
oct_mach_info::native_float_format (void)
{
  if (! instance)
    instance = new oct_mach_info ();

  return instance->native_float_fmt;
}

bool
oct_mach_info::words_big_endian (void)
{
  if (! instance)
    instance = new oct_mach_info ();

  return instance->big_chief;
}

bool
oct_mach_info::words_little_endian (void)
{
  if (! instance)
    instance = new oct_mach_info ();

  return ! instance->big_chief;
}

oct_mach_info::float_format
oct_mach_info::string_to_float_format (const string& s)
{
  oct_mach_info::float_format retval = oct_mach_info::unknown;

  if (s == "native" || s == "n")
    retval = oct_mach_info::native;
  else if (s == "ieee-be" || s == "b")
    retval = oct_mach_info::ieee_big_endian;
  else if (s == "ieee-le" || s == "l")
    retval = oct_mach_info::ieee_little_endian;
  else if (s == "vaxd" || s == "d")
    retval = oct_mach_info::vax_d;
  else if (s == "vaxg" || s == "g")
    retval = oct_mach_info::vax_g;
  else if (s == "cray" || s == "c")
    retval = oct_mach_info::cray;
  else if (s == "unknown")
    retval = oct_mach_info::unknown;
  else
    (*current_liboctave_error_handler)
      ("invalid architecture type specified");

  return retval;
}

string
oct_mach_info::float_format_as_string (float_format flt_fmt)
{
  string retval = "unknown";

  switch (flt_fmt)
    {
    case native:
      retval = "native";
      break;

    case ieee_big_endian:
      retval = "ieee_big_endian";
      break;

    case ieee_little_endian:
      retval = "ieee_little_endian";
      break;

    case vax_d:
      retval = "vax_d_float";
      break;

    case vax_g:
      retval = "vax_g_float";
      break;

    case cray:
      retval = "cray";
      break;

    default:
      break;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
