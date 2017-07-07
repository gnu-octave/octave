/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "f77-fcn.h"
#include "lo-error.h"
#include "mach-info.h"

extern "C"
{
  double F77_FUNC (d1mach, D1MACH) (const F77_INT&);
}

namespace octave
{
  namespace mach_info
  {
    union equiv
    {
      double d;
      int i[2];
    };

    struct float_params
    {
      float_format fp_fmt;
      equiv fp_par[4];
    };

#define INIT_FLT_PAR(fp, fmt, sm1, sm2, lrg1, lrg2, rt1, rt2, dv1, dv2) \
    do                                                                  \
      {                                                                 \
        fp.fp_fmt = (fmt);                                              \
        fp.fp_par[0].i[0] = (sm1);  fp.fp_par[0].i[1] = (sm2);          \
        fp.fp_par[1].i[0] = (lrg1); fp.fp_par[1].i[1] = (lrg2);         \
        fp.fp_par[2].i[0] = (rt1);  fp.fp_par[2].i[1] = (rt2);          \
        fp.fp_par[3].i[0] = (dv1);  fp.fp_par[3].i[1] = (dv2);          \
      }                                                                 \
    while (0)

    static int equiv_compare (const equiv *std, const equiv *v, int len)
    {
      int i;
      for (i = 0; i < len; i++)
        if (v[i].i[0] != std[i].i[0] || v[i].i[1] != std[i].i[1])
          return 0;
      return 1;
    }

    static float_format get_float_format (void)
    {
      float_format retval = flt_fmt_unknown;

      float_params fp[5];

      INIT_FLT_PAR (fp[0], flt_fmt_ieee_big_endian,
                    1048576,  0,
                    2146435071, -1,
                    1017118720,  0,
                    1018167296,  0);

      INIT_FLT_PAR (fp[1], flt_fmt_ieee_little_endian,
                    0,    1048576,
                    -1, 2146435071,
                    0, 1017118720,
                    0, 1018167296);

      INIT_FLT_PAR (fp[4], flt_fmt_unknown,
                    0, 0,
                    0, 0,
                    0, 0,
                    0, 0);

      equiv mach_fp_par[4];

      mach_fp_par[0].d = F77_FUNC (d1mach, D1MACH) (1);
      mach_fp_par[1].d = F77_FUNC (d1mach, D1MACH) (2);
      mach_fp_par[2].d = F77_FUNC (d1mach, D1MACH) (3);
      mach_fp_par[3].d = F77_FUNC (d1mach, D1MACH) (4);

      int i = 0;
      do
        {
          if (equiv_compare (fp[i].fp_par, mach_fp_par, 4))
            {
              retval = fp[i].fp_fmt;
              break;
            }
        }
      while (fp[++i].fp_fmt != flt_fmt_unknown);

      return retval;
    }

    static bool is_big_endian (void)
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

    float_format native_float_format (void)
    {
      static float_format fmt = get_float_format ();

      return fmt;
    }

    bool words_big_endian (void)
    {
      static bool big_endian = is_big_endian ();

      return big_endian;
    }

    bool words_little_endian (void)
    {
      static bool little_endian = ! is_big_endian ();

      return little_endian;
    }

    float_format string_to_float_format (const std::string& s)
    {
      float_format retval = flt_fmt_unknown;

      if (s == "native" || s == "n")
        retval = native_float_format ();
      else if (s == "ieee-be" || s == "b")
        retval = flt_fmt_ieee_big_endian;
      else if (s == "ieee-le" || s == "l")
        retval = flt_fmt_ieee_little_endian;
      else if (s == "unknown")
        retval = flt_fmt_unknown;
      else
        (*current_liboctave_error_handler)
          ("invalid architecture type specified");

      return retval;
    }

    std::string float_format_as_string (float_format flt_fmt)
    {
      std::string retval = "unknown";

      switch (flt_fmt)
        {
        case flt_fmt_ieee_big_endian:
          retval = "ieee-be";
          break;

        case flt_fmt_ieee_little_endian:
          retval = "ieee-le";
          break;

        default:
          break;
        }

      return retval;
    }
  }
}
