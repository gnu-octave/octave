////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <stdint.h>

#include "cmach-info.h"
#include "f77-fcn.h"

extern double F77_FUNC (d1mach, D1MACH) (const F77_INT*);

typedef union
{
  double d;
  int32_t i[2];
} equiv;

typedef struct
{
  int fp_fmt;
  equiv fp_par[4];
} float_params;

#define INIT_FLT_PAR(fp, fmt, sm1, sm2, lrg1, lrg2, rt1, rt2, dv1, dv2) \
  do                                                                    \
    {                                                                   \
      fp.fp_fmt = (fmt);                                                \
      fp.fp_par[0].i[0] = (sm1);  fp.fp_par[0].i[1] = (sm2);            \
      fp.fp_par[1].i[0] = (lrg1); fp.fp_par[1].i[1] = (lrg2);           \
      fp.fp_par[2].i[0] = (rt1);  fp.fp_par[2].i[1] = (rt2);            \
      fp.fp_par[3].i[0] = (dv1);  fp.fp_par[3].i[1] = (dv2);            \
    }                                                                   \
  while (0)

static int equiv_compare (const equiv *std, const equiv *v, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (v[i].i[0] != std[i].i[0] || v[i].i[1] != std[i].i[1])
      return 0;
  return 1;
}

// Return 0 if the floating point format is unknown, 1 if it is IEEE
// little endian, or 2 if it is IEEE big endian.
//
// If the return values change, you must also change the values of the
// float format enum in mach-info.h and the correspondence between the
// integer and enum values in octave::mach_info::get_float_format.

int
octave_get_float_format (void)
{
  int retval = 0;

  float_params fp[3];

  INIT_FLT_PAR (fp[0], 1,
                0,    1048576,
                -1, 2146435071,
                0, 1017118720,
                0, 1018167296);

  INIT_FLT_PAR (fp[1], 2,
                1048576,  0,
                2146435071, -1,
                1017118720,  0,
                1018167296,  0);

  INIT_FLT_PAR (fp[2], 0,
                0, 0,
                0, 0,
                0, 0,
                0, 0);

  equiv mach_fp_par[4];

  F77_INT opt;

  opt = 1;
  mach_fp_par[0].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 2;
  mach_fp_par[1].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 3;
  mach_fp_par[2].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 4;
  mach_fp_par[3].d = F77_FUNC (d1mach, D1MACH) (&opt);

  int i = 0;
  do
    {
      if (equiv_compare (fp[i].fp_par, mach_fp_par, 4))
        {
          retval = fp[i].fp_fmt;
          break;
        }
    }
  while (fp[++i].fp_fmt != 0);

  return retval;
}

int
octave_is_big_endian (void)
{
  // Are we little or big endian?  From Harbison & Steele.

  union
  {
    long l;
    char c[sizeof (long)];
  } u;

  u.l = 1;

  return (u.c[sizeof (long) - 1] == 1);
}
