// float-fmt.cc                                           -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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
#include "float-fmt.h"

extern "C"
{
  double F77_FCN (d1mach, D1MACH) (const int&);
}

// The floating point format on this system.
floating_point_format native_float_format = OCTAVE_UNKNOWN_FLT_FMT;

union equiv
{
  double d;
  int i[2];
};

struct
float_params
{
  floating_point_format fp_fmt;
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

int
float_format_init (void)
{
  float_params fp[5];

  INIT_FLT_PAR (fp[0], OCTAVE_IEEE_BIG,
		   1048576,  0,
		2146435071, -1,
		1017118720,  0,
		1018167296,  0);

  INIT_FLT_PAR (fp[1], OCTAVE_IEEE_LITTLE,
		 0,    1048576,
		-1, 2146435071,
		 0, 1017118720,
		 0, 1018167296);

  INIT_FLT_PAR (fp[2], OCTAVE_VAX_D,
		   128,  0,
		-32769, -1,
		  9344,  0,
		  9344,  0);

  INIT_FLT_PAR (fp[3], OCTAVE_VAX_G,
		    16,  0,
		-32769, -1,
		 15552,  0,
		 15552,  0);

  INIT_FLT_PAR (fp[4], OCTAVE_UNKNOWN_FLT_FMT,
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
	  native_float_format = fp[i].fp_fmt;
	  break;
	}
    }
  while (fp[++i].fp_fmt != OCTAVE_UNKNOWN_FLT_FMT);

  return (native_float_format != OCTAVE_UNKNOWN_FLT_FMT);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
