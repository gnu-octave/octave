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

#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstring>

#include <iomanip>
#include <iostream>
#include <strstream>
#include <string>

#include "CMatrix.h"
#include "Range.h"
#include "cmd-edit.h"
#include "dMatrix.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-cmplx.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pager.h"
#include "pr-output.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

// TRUE means use a scaled fixed point format for `format long' and
// `format short'.
static bool Vfixed_point_format;

// The maximum field width for a number printed by the default output
// routines.
static int Voutput_max_field_width;

// The precision of the numbers printed by the default output
// routines.
static int Voutput_precision;

// TRUE means that the dimensions of empty matrices should be printed
// like this: x = [](2x0).
static bool Vprint_empty_dimensions;

// TRUE means that the rows of big matrices should be split into
// smaller slices that fit on the screen.
static bool Vsplit_long_rows;

// Current format std::string for real numbers and the real part of complex
// numbers.
static char *curr_real_fmt = 0;

// Current format std::string for the imaginary part of complex numbers.
static char *curr_imag_fmt = 0;

// TRUE means don't do any fancy formatting.
static bool free_format = false;

// TRUE means print plus sign for nonzero, blank for zero.
static bool plus_format = false;

// TRUE means always print like dollars and cents.
static bool bank_format = false;

// TRUE means print data in hexadecimal format.
static bool hex_format = false;

// TRUE means print data in binary-bit-pattern format.
static int bit_format = 0;

// TRUE means don't put newlines around the column number headers.
static bool compact_format = false;

// TRUE means use an e format.
static bool print_e = false;

// TRUE means print E instead of e for exponent field.
static bool print_big_e = false;

// XXX FIXME XXX -- these should probably be somewhere else.

static double
pr_max_internal (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  double result = -DBL_MAX;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = m (i, j);
	if (xisinf (val) || xisnan (val))
	  continue;

	if (val > result)
	  result = val;
      }
  return result;
}

static double
pr_min_internal (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  double result = DBL_MAX;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = m (i, j);
	if (xisinf (val) || xisnan (val))
	  continue;

	if (val < result)
	  result = val;
      }
  return result;
}

// XXX FIXME XXX -- it would be nice to share more code among these
// functions,..

static void
set_real_format (bool sign, int digits, bool inf_or_nan, bool nan_or_int,
		 int &fw)
{
  static char fmt_buf[128];

  int prec = Voutput_precision;

  int ld, rd;

  if (bank_format)
    {
      fw = digits < 0 ? 4 : digits + 3;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (nan_or_int)
    {
      fw = digits;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
      rd = 0;
    }
  else
    {
      if (digits > 0)
	{
	  ld = digits;
	  rd = prec > digits ? prec - digits : prec;
	  digits++;
	}
      else
	{
	  ld = 1;
	  rd = prec > digits ? prec - digits : prec;
	  digits = -digits + 1;
	}

      fw = ld + 1 + rd;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
    }

  if (! (bank_format || hex_format || bit_format)
      && (fw > Voutput_max_field_width || print_e))
    {
      int exp_field = 4;
      if (digits > 100)
	exp_field++;

      fw = 2 + prec + exp_field;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;

      if (print_big_e)
	sprintf (fmt_buf, "%%%d.%dE", fw, prec - 1);
      else
	sprintf (fmt_buf, "%%%d.%de", fw, prec - 1);
    }
  else
    {
      sprintf (fmt_buf, "%%%d.%df", fw, rd);
    }

  curr_real_fmt = &fmt_buf[0];
}

static void
set_format (double d, int& fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  bool sign = (d < 0.0);

  bool inf_or_nan = (xisinf (d) || xisnan (d));

  bool nan_or_int = (xisnan (d) || D_NINT (d) == d);

  double d_abs = d < 0.0 ? -d : d;

  int digits = (inf_or_nan || d_abs == 0.0)
    ? 0 : static_cast<int> (floor (log10 (d_abs) + 1.0));

  set_real_format (sign, digits, inf_or_nan, nan_or_int, fw);
}

static inline void
set_format (double d)
{
  int fw;
  set_format (d, fw);
}

static void
set_real_matrix_format (bool sign, int x_max, int x_min,
			bool inf_or_nan, int int_or_inf_or_nan, int& fw)
{
  static char fmt_buf[128];

  int prec = Voutput_precision;

  int ld, rd;

  if (bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits <= 0 ? 4 : digits + 3;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (Vfixed_point_format)
    {
      rd = prec;
      fw = rd + 2;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
    }
  else if (int_or_inf_or_nan)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits <= 0 ? 1 : digits;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
      rd = 0;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
	{
	  ld_max = x_max;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max++;
	}
      else
	{
	  ld_max = 1;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max = -x_max + 1;
	}

      int ld_min, rd_min;
      if (x_min > 0)
	{
	  ld_min = x_min;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min++;
	}
      else
	{
	  ld_min = 1;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min = -x_min + 1;
	}

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      fw = ld + 1 + rd;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;
    }

  if (! (bank_format || hex_format || bit_format)
      && (print_e
	  || (! Vfixed_point_format && fw > Voutput_max_field_width)))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100)
	exp_field++;

      fw = 2 + prec + exp_field;
      if (inf_or_nan && fw < 3)
	fw = 3;
      fw += sign;

      if (print_big_e)
	sprintf (fmt_buf, "%%%d.%dE", fw, prec - 1);
      else
	sprintf (fmt_buf, "%%%d.%de", fw, prec - 1);
    }
  else
    {
      sprintf (fmt_buf, "%%%d.%df", fw, rd);
    }

  curr_real_fmt = &fmt_buf[0];
}

static void
set_format (const Matrix& m, int& fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  bool sign = m.any_element_is_negative ();

  bool inf_or_nan = m.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = m.all_elements_are_int_or_inf_or_nan ();

  Matrix m_abs = m.abs ();
  double max_abs = pr_max_internal (m_abs);
  double min_abs = pr_min_internal (m_abs);

  int x_max = max_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (max_abs) + 1.0));

  int x_min = min_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (min_abs) + 1.0));

  scale = (x_max == 0 || int_or_inf_or_nan) ? 1.0 : pow (10.0, x_max - 1);

  set_real_matrix_format (sign, x_max, x_min, inf_or_nan,
			  int_or_inf_or_nan, fw);
}

static inline void
set_format (const Matrix& m)
{
  int fw;
  double scale;
  set_format (m, fw, scale);
}

static void
set_complex_format (bool sign, int x_max, int x_min, int r_x,
		    bool inf_or_nan, int int_only, int& r_fw, int& i_fw)
{
  static char r_fmt_buf[128];
  static char i_fmt_buf[128];

  int prec = Voutput_precision;

  int ld, rd;

  if (bank_format)
    {
      int digits = r_x;
      i_fw = 0;
      r_fw = digits <= 0 ? 4 : digits + 3;
      if (inf_or_nan && r_fw < 3)
	r_fw = 3;
      r_fw += sign;
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (double);
      i_fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (double);
      i_fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (inf_or_nan || int_only)
    {
      int digits = x_max > x_min ? x_max : x_min;
      i_fw = r_fw = digits <= 0 ? 1 : digits;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
      rd = 0;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
	{
	  ld_max = x_max;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max++;
	}
      else
	{
	  ld_max = 1;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max = -x_max + 1;
	}

      int ld_min, rd_min;
      if (x_min > 0)
	{
	  ld_min = x_min;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min++;
	}
      else
	{
	  ld_min = 1;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min = -x_min + 1;
	}

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      i_fw = r_fw = ld + 1 + rd;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
    }

  if (! (bank_format || hex_format || bit_format)
      && (r_fw > Voutput_max_field_width || print_e))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100)
	exp_field++;

      i_fw = r_fw = 1 + prec + exp_field;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;

      if (print_big_e)
	{
	  sprintf (r_fmt_buf, "%%%d.%dE", r_fw, prec - 1);
	  sprintf (i_fmt_buf, "%%%d.%dE", i_fw, prec - 1);
	}
      else
	{
	  sprintf (r_fmt_buf, "%%%d.%de", r_fw, prec - 1);
	  sprintf (i_fmt_buf, "%%%d.%de", i_fw, prec - 1);
	}
    }
  else
    {
      sprintf (r_fmt_buf, "%%%d.%df", r_fw, rd);
      sprintf (i_fmt_buf, "%%%d.%df", i_fw, rd);
    }

  curr_real_fmt = &r_fmt_buf[0];
  curr_imag_fmt = &i_fmt_buf[0];
}

static void
set_format (const Complex& c, int& r_fw, int& i_fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  double rp = c.real ();
  double ip = c.imag ();

  bool sign = (rp < 0.0);

  bool inf_or_nan = (xisinf (c) || xisnan (c));

  bool int_only = (D_NINT (rp) == rp && D_NINT (ip) == ip);

  double r_abs = rp < 0.0 ? -rp : rp;
  double i_abs = ip < 0.0 ? -ip : ip;

  int r_x = r_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (r_abs) + 1.0));

  int i_x = i_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (i_abs) + 1.0));

  int x_max, x_min;

  if (r_x > i_x)
    {
      x_max = r_x;
      x_min = i_x;
    }
  else
    {
      x_max = i_x;
      x_min = r_x;
    }

  set_complex_format (sign, x_max, x_min, r_x, inf_or_nan, int_only,
		      r_fw, i_fw);
}

static inline void
set_format (const Complex& c)
{
  int r_fw, i_fw;
  set_format (c, r_fw, i_fw);
}

static void
set_complex_matrix_format (bool sign, int x_max, int x_min,
			   int r_x_max, int r_x_min, bool inf_or_nan,
			   int int_or_inf_or_nan, int& r_fw, int& i_fw)
{
  static char r_fmt_buf[128];
  static char i_fmt_buf[128];

  int prec = Voutput_precision;

  int ld, rd;

  if (bank_format)
    {
      int digits = r_x_max > r_x_min ? r_x_max : r_x_min;
      i_fw = 0;
      r_fw = digits <= 0 ? 4 : digits + 3;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (double);
      i_fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (double);
      i_fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (Vfixed_point_format)
    {
      rd = prec;
      i_fw = r_fw = rd + 2;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
    }
  else if (int_or_inf_or_nan)
    {
      int digits = x_max > x_min ? x_max : x_min;
      i_fw = r_fw = digits <= 0 ? 1 : digits;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
      rd = 0;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
	{
	  ld_max = x_max;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max++;
	}
      else
	{
	  ld_max = 1;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max = -x_max + 1;
	}

      int ld_min, rd_min;
      if (x_min > 0)
	{
	  ld_min = x_min;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min++;
	}
      else
	{
	  ld_min = 1;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min = -x_min + 1;
	}

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      i_fw = r_fw = ld + 1 + rd;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;
    }

  if (! (bank_format || hex_format || bit_format)
      && (print_e
	  || (! Vfixed_point_format && r_fw > Voutput_max_field_width)))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100)
	exp_field++;

      i_fw = r_fw = 1 + prec + exp_field;
      if (inf_or_nan && i_fw < 3)
	i_fw = r_fw = 3;
      r_fw += sign;

      if (print_big_e)
	{
	  sprintf (r_fmt_buf, "%%%d.%dE", r_fw, prec - 1);
	  sprintf (i_fmt_buf, "%%%d.%dE", i_fw, prec - 1);
	}
      else
	{
	  sprintf (r_fmt_buf, "%%%d.%de", r_fw, prec - 1);
	  sprintf (i_fmt_buf, "%%%d.%de", i_fw, prec - 1);
	}
    }
  else
    {
      sprintf (r_fmt_buf, "%%%d.%df", r_fw, rd);
      sprintf (i_fmt_buf, "%%%d.%df", i_fw, rd);
    }

  curr_real_fmt = &r_fmt_buf[0];
  curr_imag_fmt = &i_fmt_buf[0];
}

static void
set_format (const ComplexMatrix& cm, int& r_fw, int& i_fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  Matrix rp = real (cm);
  Matrix ip = imag (cm);

  bool sign = rp.any_element_is_negative ();

  bool inf_or_nan = cm.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = (rp.all_elements_are_int_or_inf_or_nan ()
			    && ip.all_elements_are_int_or_inf_or_nan ());

  Matrix r_m_abs = rp.abs ();
  double r_max_abs = pr_max_internal (r_m_abs);
  double r_min_abs = pr_min_internal (r_m_abs);

  Matrix i_m_abs = ip.abs ();
  double i_max_abs = pr_max_internal (i_m_abs);
  double i_min_abs = pr_min_internal (i_m_abs);

  int r_x_max = r_max_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (r_max_abs) + 1.0));

  int r_x_min = r_min_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (r_min_abs) + 1.0));

  int i_x_max = i_max_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (i_max_abs) + 1.0));

  int i_x_min = i_min_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (i_min_abs) + 1.0));

  int x_max = r_x_max > i_x_max ? r_x_max : i_x_max;
  int x_min = r_x_min > i_x_min ? r_x_min : i_x_min;

  scale = (x_max == 0 || int_or_inf_or_nan) ? 1.0 : pow (10.0, x_max - 1);

  set_complex_matrix_format (sign, x_max, x_min, r_x_max, r_x_min,
			     inf_or_nan, int_or_inf_or_nan, r_fw, i_fw);
}

static inline void
set_format (const ComplexMatrix& cm)
{
  int r_fw, i_fw;
  double scale;
  set_format (cm, r_fw, i_fw, scale);
}

static void
set_range_format (bool sign, int x_max, int x_min, int all_ints,
		  int& fw)
{
  static char fmt_buf[128];

  int prec = Voutput_precision;

  int ld, rd;

  if (bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = sign + digits < 0 ? 4 : digits + 3;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (all_ints)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = sign + digits;
      rd = 0;
    }
  else if (Vfixed_point_format)
    {
      rd = prec;
      fw = rd + 2 + sign;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
	{
	  ld_max = x_max;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max++;
	}
      else
	{
	  ld_max = 1;
	  rd_max = prec > x_max ? prec - x_max : prec;
	  x_max = -x_max + 1;
	}

      int ld_min, rd_min;
      if (x_min > 0)
	{
	  ld_min = x_min;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min++;
	}
      else
	{
	  ld_min = 1;
	  rd_min = prec > x_min ? prec - x_min : prec;
	  x_min = -x_min + 1;
	}

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      fw = sign + ld + 1 + rd;
    }

  if (! (bank_format || hex_format || bit_format)
      && (print_e
	  || (! Vfixed_point_format && fw > Voutput_max_field_width)))
    {
      int exp_field = 4;
      if (x_max > 100 || x_min > 100)
	exp_field++;

      fw = sign + 2 + prec + exp_field;

      if (print_big_e)
	sprintf (fmt_buf, "%%%d.%dE", fw, prec - 1);
      else
	sprintf (fmt_buf, "%%%d.%de", fw, prec - 1);
    }
  else
    {
      sprintf (fmt_buf, "%%%d.%df", fw, rd);
    }

  curr_real_fmt = &fmt_buf[0];
}

static void
set_format (const Range& r, int& fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  double r_min = r.base ();
  double r_max = r.limit ();

  if (r_max < r_min)
    {
      double tmp = r_max;
      r_max = r_min;
      r_min = tmp;
    }

  bool sign = (r_min < 0.0);

  bool all_ints = r.all_elements_are_ints ();

  double max_abs = r_max < 0.0 ? -r_max : r_max;
  double min_abs = r_min < 0.0 ? -r_min : r_min;

  int x_max = max_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (max_abs) + 1.0));

  int x_min = min_abs == 0.0
    ? 0 : static_cast<int> (floor (log10 (min_abs) + 1.0));

  scale = (x_max == 0 || all_ints) ? 1.0 : pow (10.0, x_max - 1);

  set_range_format (sign, x_max, x_min, all_ints, fw);
}

static inline void
set_format (const Range& r)
{
  int fw;
  double scale;
  set_format (r, fw, scale);
}

union equiv
{
  double d;
  unsigned char i[sizeof (double)];
};

#define PRINT_CHAR_BITS(os, c) \
  do \
    { \
      unsigned char ctmp = c; \
      char stmp[9]; \
      stmp[0] = (ctmp & 0x80) ? '1' : '0'; \
      stmp[1] = (ctmp & 0x40) ? '1' : '0'; \
      stmp[2] = (ctmp & 0x20) ? '1' : '0'; \
      stmp[3] = (ctmp & 0x10) ? '1' : '0'; \
      stmp[4] = (ctmp & 0x08) ? '1' : '0'; \
      stmp[5] = (ctmp & 0x04) ? '1' : '0'; \
      stmp[6] = (ctmp & 0x02) ? '1' : '0'; \
      stmp[7] = (ctmp & 0x01) ? '1' : '0'; \
      stmp[8] = '\0'; \
      os << stmp; \
    } \
  while (0)

#define PRINT_CHAR_BITS_SWAPPED(os, c) \
  do \
    { \
      unsigned char ctmp = c; \
      char stmp[9]; \
      stmp[0] = (ctmp & 0x01) ? '1' : '0'; \
      stmp[1] = (ctmp & 0x02) ? '1' : '0'; \
      stmp[2] = (ctmp & 0x04) ? '1' : '0'; \
      stmp[3] = (ctmp & 0x08) ? '1' : '0'; \
      stmp[4] = (ctmp & 0x10) ? '1' : '0'; \
      stmp[5] = (ctmp & 0x20) ? '1' : '0'; \
      stmp[6] = (ctmp & 0x40) ? '1' : '0'; \
      stmp[7] = (ctmp & 0x80) ? '1' : '0'; \
      stmp[8] = '\0'; \
      os << stmp; \
    } \
  while (0)

static void
pr_any_float (const char *fmt, std::ostream& os, double d, int fw = 0)
{
#if defined (SCO)
  // Apparently on some SCO systems NaN == -0.0 is true.  Compiler bug?
  if (d == -0.0 && ! xisnan (d))
    d = 0.0;
#else
  if (d == -0.0)
    d = 0.0;
#endif

  if (fmt)
    {
      if (hex_format)
	{
	  equiv tmp;
	  tmp.d = d;

	  // Unless explicitly asked for, always print in big-endian
	  // format.

	  // XXX FIXME XXX -- is it correct to swap bytes for VAX
	  // formats and not for Cray?

	  // XXX FIXME XXX -- will bad things happen if we are
	  // interrupted before resetting the format flags and fill
	  // character?

	  oct_mach_info::float_format flt_fmt =
	    oct_mach_info::native_float_format ();

	  char ofill = os.fill ('0');

	  std::ios::fmtflags oflags = os.setf (std::ios::right);
	  os.setf (std::ios::hex, std::ios::basefield);

	  if (hex_format > 1
	      || flt_fmt == oct_mach_info::ieee_big_endian
	      || flt_fmt == oct_mach_info::cray
	      || flt_fmt == oct_mach_info::unknown)
	    {
	      for (size_t i = 0; i < sizeof (double); i++)
		os << std::setw (2) << static_cast<int> (tmp.i[i]);
	    }
	  else
	    {
	      for (int i = sizeof (double) - 1; i >= 0; i--)
		os << std::setw (2) << static_cast<int> (tmp.i[i]);
	    }

	  os.fill (ofill);
	  os.setf (oflags);	  
	}
      else if (bit_format)
	{
	  equiv tmp;
	  tmp.d = d;

	  // Unless explicitly asked for, always print in big-endian
	  // format.

	  // XXX FIXME XXX -- is it correct to swap bytes for VAX
	  // formats and not for Cray?

	  oct_mach_info::float_format flt_fmt =
	    oct_mach_info::native_float_format ();

	  if (flt_fmt == oct_mach_info::ieee_big_endian
	      || flt_fmt == oct_mach_info::cray
	      || flt_fmt == oct_mach_info::unknown)
	    {
	      for (size_t i = 0; i < sizeof (double); i++)
		PRINT_CHAR_BITS (os, tmp.i[i]);
	    }
	  else
	    {
	      if (bit_format > 1)
		{
		  for (size_t i = 0; i < sizeof (double); i++)
		    PRINT_CHAR_BITS_SWAPPED (os, tmp.i[i]);
		}
	      else
		{
		  for (int i = sizeof (double) - 1; i >= 0; i--)
		    PRINT_CHAR_BITS (os, tmp.i[i]);
		}
	    }
	}
      else if (xisinf (d))
	{
	  const char *s;
	  if (d < 0.0)
	    s = "-Inf";
	  else
	    s = "Inf";

	  if (fw > 0)
	    os << std::setw (fw) << s;
	  else
	    os << s;
	}
      else if (xisnan (d))
	{
	  if (fw > 0)
	    os << std::setw (fw) << "NaN";
	  else
	    os << "NaN";
	}
      else
	os.form (fmt, d);
    }
  else
    os << d;
}

static inline void
pr_float (std::ostream& os, double d, int fw = 0)
{
  pr_any_float (curr_real_fmt, os, d, fw);
}

static inline void
pr_imag_float (std::ostream& os, double d, int fw = 0)
{
  pr_any_float (curr_imag_fmt, os, d, fw);
}

static void
pr_complex (std::ostream& os, const Complex& c, int r_fw = 0, int i_fw = 0)
{
  double r = c.real ();
  pr_float (os, r, r_fw);
  if (! bank_format)
    {
      double i = c.imag ();
      if (! (hex_format || bit_format) && i < 0)
	{
	  os << " - ";
	  i = -i;
	  pr_imag_float (os, i, i_fw);
	}
      else
	{
	  if (hex_format || bit_format)
	    os << "  ";
	  else
	    os << " + ";

	  pr_imag_float (os, i, i_fw);
	}
      os << "i";
    }
}

static void
print_empty_matrix (std::ostream& os, int nr, int nc, bool pr_as_read_syntax)
{
  assert (nr == 0 || nc == 0);

  if (pr_as_read_syntax)
    {
      if (nr == 0 && nc == 0)
	os << "[]";
      else
	os << "zeros (" << nr << ", " << nc << ")";
    }
  else
    {
      os << "[]";
      if (Vprint_empty_dimensions)
	os << "(" << nr << "x" << nc << ")";
    }
}

static void
pr_scale_header (std::ostream& os, double scale)
{
  if (Vfixed_point_format && scale != 1.0)
    {
      os << "  "
	 << std::setw (8) << std::setprecision (1)
	 << std::setiosflags (std::ios::scientific|std::ios::left)
	 << scale
	 << std::resetiosflags (std::ios::scientific|std::ios::left)
	 << " *\n";

      if (! compact_format)
	os << "\n";
    }
}

static void
pr_col_num_header (std::ostream& os, int total_width, int max_width,
		   int lim, int col, int extra_indent)
{
  if (total_width > max_width && Vsplit_long_rows)
    {
      if (col != 0 && ! compact_format)
	os << "\n\n";

      int num_cols = lim - col;

      os << std::setw (extra_indent) << "";

      if (num_cols == 1)
	os << " Column " << col + 1 << ":\n";
      else if (num_cols == 2)
	os << " Columns " << col + 1 << " and " << lim << ":\n";
      else
	os << " Columns " << col + 1 << " through " << lim << ":\n";

      if (! compact_format)
	os << "\n";
    }
}

static inline void
do_plus_format (std::ostream& os, double d)
{
  if (d == 0.0)
    os << " ";
  else if (d < 0.0)
    os << "-";
  else
    os << "+";
}

void
octave_print_internal (std::ostream& os, double d, bool pr_as_read_syntax)
{
  if (plus_format)
    {
      do_plus_format (os, d);
    }
  else
    {
      set_format (d);
      if (free_format)
	os << d;
      else
	pr_float (os, d);
    }
}

void
octave_print_internal (std::ostream& os, const Matrix& m, bool pr_as_read_syntax,
		       int extra_indent)
{
  int nr = m.rows ();
  int nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (int i = 0; i < nr; i++)
	{
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == 0)
		os << "  ";

	      do_plus_format (os, m (i, j));
	    }

	  if (i < nr - 1)
	    os << "\n";
	}
    }
  else
    {
      int fw;
      double scale = 1.0;
      set_format (m, fw, scale);
      int column_width = fw + 2;
      int total_width = nc * column_width;
      int max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
	max_width -= 4;
      else
	max_width -= extra_indent;

      if (max_width < 0)
	max_width = 0;

      if (free_format)
	{
	  if (pr_as_read_syntax)
	    os << "[\n";

	  os << m;

	  if (pr_as_read_syntax)
	    os << "]";

	  return;
	}

      int inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}

      if (pr_as_read_syntax)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      int col = 0;
	      while (col < nc)
		{
		  int lim = col + inc < nc ? col + inc : nc;

		  for (int j = col; j < lim; j++)
		    {
		      if (i == 0 && j == 0)
			os << "[ ";
		      else
			{
			  if (j > col && j < lim)
			    os << ", ";
			  else
			    os << "  ";
			}

		      pr_float (os, m (i, j));
		    }

		  col += inc;

		  if (col >= nc)
		    {
		      if (i == nr - 1)
			os << " ]";
		      else
			os << ";\n";
		    }
		  else
		    os << " ...\n";
		}
	    }
	}
      else
	{
	  pr_scale_header (os, scale);

	  for (int col = 0; col < nc; col += inc)
	    {
	      int lim = col + inc < nc ? col + inc : nc;

	      pr_col_num_header (os, total_width, max_width, lim, col,
				 extra_indent);

	      for (int i = 0; i < nr; i++)
		{
		  os << std::setw (extra_indent) << "";

		  for (int j = col; j < lim; j++)
		    {
		      os << "  ";

		      double tmp = (Vfixed_point_format && scale != 1.0)
			? m(i,j) / scale : m(i,j);

		      pr_float (os, tmp, fw);
		    }

		  if (i < nr - 1)
		    os << "\n";
		}
	    }
	}
    }
}

static inline void
do_plus_format (std::ostream& os, const Complex& c)
{
  double rp = c.real ();
  double ip = c.imag ();

  if (rp == 0.0)
    {
      if (ip == 0.0)
	os << " ";
      else
	os << "i";
    }
  else if (ip == 0.0)
    do_plus_format (os, rp);
  else
    os << "c";
}

void
octave_print_internal (std::ostream& os, const Complex& c,
		       bool pr_as_read_syntax)
{
  if (plus_format)
    {
      do_plus_format (os, c);
    }
  else
    {
      set_format (c);
      if (free_format)
	os << c;
      else
	pr_complex (os, c);
    }
}

void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
		       bool pr_as_read_syntax, int extra_indent)
{
  int nr = cm.rows ();
  int nc = cm.columns ();

 if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (int i = 0; i < nr; i++)
	{
	  for (int j = 0; j < nc; j++)
	    {
	      if (j == 0)
		os << "  ";

	      do_plus_format (os, cm (i, j));
	    }

	  if (i < nr - 1)
	    os << "\n";
	}
    }
  else
    {
      int r_fw, i_fw;
      double scale = 1.0;
      set_format (cm, r_fw, i_fw, scale);
      int column_width = i_fw + r_fw;
      column_width += (bank_format || hex_format|| bit_format) ? 2 : 7;
      int total_width = nc * column_width;
      int max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
	max_width -= 4;
      else
	max_width -= extra_indent;

      if (max_width < 0)
	max_width = 0;

      if (free_format)
	{
	  if (pr_as_read_syntax)
	    os << "[\n";

	  os << cm;

	  if (pr_as_read_syntax)
	    os << "]";

	  return;
	}

      int inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
	{
	  inc = max_width / column_width;
	  if (inc == 0)
	    inc++;
	}

      if (pr_as_read_syntax)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      int col = 0;
	      while (col < nc)
		{
		  int lim = col + inc < nc ? col + inc : nc;

		  for (int j = col; j < lim; j++)
		    {
		      if (i == 0 && j == 0)
			os << "[ ";
		      else
			{
			  if (j > col && j < lim)
			    os << ", ";
			  else
			    os << "  ";
			}

		      pr_complex (os, cm (i, j));
		    }

		  col += inc;

		  if (col >= nc)
		    {
		      if (i == nr - 1)
			os << " ]";
		      else
			os << ";\n";
		    }
		  else
		    os << " ...\n";
		}
	    }
	}
      else
	{
	  pr_scale_header (os, scale);

	  for (int col = 0; col < nc; col += inc)
	    {
	      int lim = col + inc < nc ? col + inc : nc;

	      pr_col_num_header (os, total_width, max_width, lim, col,
				 extra_indent);

	      for (int i = 0; i < nr; i++)
		{
		  os << std::setw (extra_indent) << "";

		  for (int j = col; j < lim; j++)
		    {
		      os << "  ";

		      Complex tmp = (Vfixed_point_format && scale != 1.0)
			? cm(i,j) / scale : cm(i,j);

		      pr_complex (os, tmp, r_fw, i_fw);
		    }

		  if (i < nr - 1) 
		    os << "\n";
		}
	    }
	}
    }
}

void
octave_print_internal (std::ostream& os, const Range& r,
		       bool pr_as_read_syntax, int extra_indent)
{
  double base = r.base ();
  double increment = r.inc ();
  double limit = r.limit ();
  int num_elem = r.nelem ();

  if (plus_format && ! pr_as_read_syntax)
    {
      os << "  ";
      for (int i = 0; i < num_elem; i++)
	{
	  double val = base + i * increment;
	  if (val == 0.0)
	    os << " ";
	  else
	    os << "+";
	}
    }
  else
    {
      int fw;
      double scale = 1.0;
      set_format (r, fw, scale);

      if (pr_as_read_syntax)
	{
	  if (free_format)
	    {
	      os << base << " : ";
	      if (increment != 1.0)
		os << increment << " : ";
	      os << limit;
	    }
	  else
	    {
	      pr_float (os, base, fw);
	      os << " : ";
	      if (increment != 1.0)
		{
		  pr_float (os, increment, fw);
		  os << " : ";
		}
	      pr_float (os, limit, fw);
	    }
	}
      else
	{
	  int column_width = fw + 2;
	  int total_width = num_elem * column_width;
	  int max_width = command_editor::terminal_cols ();

	  if (free_format)
	    {
	      os << r;
	      return;
	    }

	  int inc = num_elem;
	  if (total_width > max_width && Vsplit_long_rows)
	    {
	      inc = max_width / column_width;
	      if (inc == 0)
		inc++;
	    }

	  max_width -= extra_indent;

	  if (max_width < 0)
	    max_width = 0;

	  pr_scale_header (os, scale);

	  int col = 0;
	  while (col < num_elem)
	    {
	      int lim = col + inc < num_elem ? col + inc : num_elem;

	      pr_col_num_header (os, total_width, max_width, lim, col,
				 extra_indent);

	      os << std::setw (extra_indent) << "";

	      for (int i = col; i < lim; i++)
		{
		  double val = base + i * increment;

		  os << "  ";

		  if (Vfixed_point_format && scale != 1.0)
		    val /= scale;

		  pr_float (os, val, fw);
		}

	      col += inc;

	      if (col < num_elem)
		os << "\n";
	    }
	}
    }
}

void
octave_print_internal (std::ostream& os, const boolMatrix& bm,
		       bool pr_as_read_syntax,
		       int extra_indent)
{
  Matrix tmp (bm);
  octave_print_internal (os, tmp, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const charMatrix& chm,
		       bool pr_as_read_syntax,
		       int /* extra_indent XXX FIXME XXX */,
		       bool pr_as_string)
{
  if (pr_as_string)
    {
      int nstr = chm.rows ();

      if (pr_as_read_syntax && nstr > 1)
	os << "[ ";

      if (nstr != 0)
	{
	  for (int i = 0; i < nstr; i++)
	    {
	      std::string row = chm.row_as_string (i);

	      if (pr_as_read_syntax)
		{
		  os << "\"" << undo_string_escapes (row) << "\"";

		  if (i < nstr - 1)
		    os << "; ";
		}
	      else
		{
		  os << row;

		  if (i < nstr - 1)
		    os << "\n";
		}
	    }
	}

      if (pr_as_read_syntax && nstr > 1)
	os << " ]";
    }
  else
    {
      os << "sorry, printing char matrices not implemented yet\n";
    }
}

DEFUN (disp, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} disp (@var{x})\n\
Display the value of @var{x}.  For example,\n\
\n\
@example\n\
disp (\"The value of pi is:\"), disp (pi)\n\
\n\
     @print{} the value of pi is:\n\
     @print{} 3.1416\n\
@end example\n\
\n\
@noindent\n\
Note that the output from @code{disp} always ends with a newline.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    args(0).print (octave_stdout);
  else
    print_usage ("disp");

  return retval;
}

static void
init_format_state (void)
{
  free_format = false;
  plus_format = false;
  bank_format = false;
  hex_format = false;
  bit_format = 0;
  print_e = false;
  print_big_e = false;
}

static void
set_output_prec_and_fw (int prec, int fw)
{
  bind_builtin_variable ("output_precision", static_cast<double> (prec));
  bind_builtin_variable ("output_max_field_width", static_cast<double> (fw));
}

static void
set_format_style (int argc, const string_vector& argv)
{
  int idx = 1;

  if (--argc > 0)
    {
      std::string arg = argv[idx++];

      if (arg == "short")
	{
	  if (--argc > 0)
	    {
	      arg = argv[idx++];

	      if (arg == "e")
		{
		  init_format_state ();
		  print_e = true;
		}
	      else if (arg == "E")
		{
		  init_format_state ();
		  print_e = true;
		  print_big_e = true;
		}
	      else
		{
		  error ("format: unrecognized option `short %s'",
			 arg.c_str ());
		  return;
		}
	    }
	  else
	    init_format_state ();

	  set_output_prec_and_fw (3, 8);
	}
      else if (arg == "long")
	{
	  if (--argc > 0)
	    {
	      arg = argv[idx++];

	      if (arg == "e")
		{
		  init_format_state ();
		  print_e = true;
		}
	      else if (arg == "E")
		{
		  init_format_state ();
		  print_e = true;
		  print_big_e = true;
		}
	      else
		{
		  error ("format: unrecognized option `long %s'",
			 arg.c_str ());
		  return;
		}
	    }
	  else
	    init_format_state ();

	  set_output_prec_and_fw (15, 24);
	}
      else if (arg == "hex")
	{
	  init_format_state ();
	  hex_format = true;
	}
      else if (arg == "native-hex")
	{
	  init_format_state ();
	  hex_format = 2;
	}
      else if (arg == "bit")
	{
	  init_format_state ();
	  bit_format = 1;
	}
      else if (arg == "native-bit")
	{
	  init_format_state ();
	  bit_format = 2;
	}
      else if (arg == "+" || arg == "plus")
	{
	  init_format_state ();
	  plus_format = true;
	}
      else if (arg == "bank")
	{
	  init_format_state ();
	  bank_format = true;
	}
      else if (arg == "free")
	{
	  init_format_state ();
	  free_format = true;
	}
      else if (arg == "none")
	{
	  init_format_state ();
	  free_format = true;
	}
      else if (arg == "compact")
	{
	  compact_format = true;
	}
      else if (arg == "loose")
	{
	  compact_format = false;
	}
      else
	error ("format: unrecognized format state `%s'", arg.c_str ());
    }
  else
    {
      init_format_state ();
      set_output_prec_and_fw (5, 10);
    }
}

DEFUN_TEXT (format, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} format options\n\
Control the format of the output produced by @code{disp} and Octave's\n\
normal echoing mechanism.  Valid options are listed in the following\n\
table.\n\
\n\
@table @code\n\
@item short\n\
Octave will try to print numbers with at\n\
least 3 significant figures within a field that is a maximum of 8\n\
characters wide.\n\
\n\
If Octave is unable to format a matrix so that columns line up on the\n\
decimal point and all the numbers fit within the maximum field width,\n\
it switches to an @samp{e} format.\n\
\n\
@item long\n\
Octave will try to print numbers with at least 15 significant figures\n\
within a field that is a maximum of 24 characters wide.\n\
\n\
As will the @samp{short} format, Octave will switch to an @samp{e}\n\
format if it is unable to format a matrix so that columns line up on the\n\
decimal point and all the numbers fit within the maximum field width.\n\
\n\
@item long e\n\
@itemx short e\n\
The same as @samp{format long} or @samp{format short} but always display\n\
output with an @samp{e} format.  For example, with the @samp{short e}\n\
format, pi is displayed as @code{3.14e+00}.\n\
\n\
@item long E\n\
@itemx short E\n\
The same as @samp{format long e} or @samp{format short e} but always\n\
display output with an uppercase @samp{E} format.  For example, with\n\
the @samp{long E} format, pi is displayed as\n\
@code{3.14159265358979E+00}.\n\
\n\
@item free\n\
@itemx none\n\
Print output in free format, without trying to line up columns of\n\
matrices on the decimal point.  This also causes complex numbers to be\n\
formatted like this @samp{(0.604194, 0.607088)} instead of like this\n\
@samp{0.60419 + 0.60709i}.\n\
\n\
@item bank\n\
Print in a fixed format with two places to the right of the decimal\n\
point.\n\
\n\
@item +\n\
Print a @samp{+} symbol for nonzero matrix elements and a space for zero\n\
matrix elements.  This format can be very useful for examining the\n\
structure of a large matrix.\n\
\n\
@item hex\n\
Print the hexadecimal representation numbers as they are stored in\n\
memory.  For example, on a workstation which stores 8 byte real values\n\
in IEEE format with the least significant byte first, the value of\n\
@code{pi} when printed in @code{hex} format is @code{400921fb54442d18}.\n\
This format only works for numeric values.\n\
\n\
@item bit\n\
Print the bit representation of numbers as stored in memory.\n\
For example, the value of @code{pi} is\n\
\n\
@example\n\
@group\n\
01000000000010010010000111111011\n\
01010100010001000010110100011000\n\
@end group\n\
@end example\n\
\n\
(shown here in two 32 bit sections for typesetting purposes) when\n\
printed in bit format on a workstation which stores 8 byte real values\n\
in IEEE format with the least significant byte first.  This format only\n\
works for numeric types.\n\
@end table\n\
\n\
By default, Octave will try to print numbers with at least 5 significant\n\
figures within a field that is a maximum of 10 characters wide.\n\
\n\
If Octave is unable to format a matrix so that columns line up on the\n\
decimal point and all the numbers fit within the maximum field width,\n\
it switches to an @samp{e} format.\n\
\n\
If @code{format} is invoked without any options, the default format\n\
state is restored.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("format");

  if (error_state)
    return retval;

  set_format_style (argc, argv);

  return retval;
}

static int
fixed_point_format (void)
{
  Vfixed_point_format = check_preference ("fixed_point_format");

  return 0;
}

static int
output_max_field_width (void)
{
  double val;
  if (builtin_real_scalar_variable ("output_max_field_width", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival > 0 && ival == val)
	{
	  Voutput_max_field_width = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("output_max_field_width");
  return -1;
}

static int
output_precision (void)
{
  double val;
  if (builtin_real_scalar_variable ("output_precision", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && ival == val)
	{
	  Voutput_precision = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("output_precision");
  return -1;
}

static int
print_empty_dimensions (void)
{
  Vprint_empty_dimensions = check_preference ("print_empty_dimensions");

  return 0;
}

static int
split_long_rows (void)
{
  Vsplit_long_rows = check_preference ("split_long_rows");

  return 0;
}

void
symbols_of_pr_output (void)
{
  DEFVAR (fixed_point_format, 0.0, fixed_point_format,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} fixed_point_format\n\
If the value of this variable is nonzero, Octave will scale all values\n\
in a matrix so that the largest may be written with one leading digit.\n\
The scaling factor is printed on the first line of output.  For example,\n\
\n\
@example\n\
@group\n\
octave:1> logspace (1, 7, 5)'\n\
ans =\n\
\n\
  1.0e+07  *\n\
\n\
  0.00000\n\
  0.00003\n\
  0.00100\n\
  0.03162\n\
  1.00000\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Notice that first value appears to be zero when it is actually 1.  For\n\
this reason, you should be careful when setting\n\
@code{fixed_point_format} to a nonzero value.\n\
\n\
The default value of @code{fixed_point_format} is 0.\n\
@end defvr");

  DEFVAR (output_max_field_width, 10.0, output_max_field_width,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} output_max_field_width\n\
This variable specifies the maximum width of a numeric output field.\n\
The default value is 10.\n\
@end defvr");

  DEFVAR (output_precision, 5.0, output_precision,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} output_precision\n\
This variable specifies the minimum number of significant figures to\n\
display for numeric output.  The default value is 5.\n\
@end defvr");

  DEFVAR (print_empty_dimensions, 1.0, print_empty_dimensions,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} print_empty_dimensions\n\
If the value of @code{print_empty_dimensions} is nonzero, the\n\
dimensions of empty matrices are printed along with the empty matrix\n\
symbol, @samp{[]}.  For example, the expression\n\
\n\
@example\n\
zeros (3, 0)\n\
@end example\n\
\n\
@noindent\n\
will print\n\
\n\
@example\n\
ans = [](3x0)\n\
@end example\n\
@end defvr");

  DEFVAR (split_long_rows, 1.0, split_long_rows,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} split_long_rows\n\
For large matrices, Octave may not be able to display all the columns of\n\
a given row on one line of your screen.  This can result in missing\n\
information or output that is nearly impossible to decipher, depending\n\
on whether your terminal truncates or wraps long lines.\n\
\n\
If the value of @code{split_long_rows} is nonzero, Octave will display\n\
the matrix in a series of smaller pieces, each of which can fit within\n\
the limits of your terminal width.  Each set of rows is labeled so that\n\
you can easily see which columns are currently being displayed.\n\
For example:\n\
\n\
@smallexample\n\
@group\n\
octave:13> rand (2,10)\n\
ans =\n\
\n\
 Columns 1 through 6:\n\
\n\
  0.75883  0.93290  0.40064  0.43818  0.94958  0.16467\n\
  0.75697  0.51942  0.40031  0.61784  0.92309  0.40201\n\
\n\
 Columns 7 through 10:\n\
\n\
  0.90174  0.11854  0.72313  0.73326\n\
  0.44672  0.94303  0.56564  0.82150\n\
@end group\n\
@end smallexample\n\
\n\
@noindent\n\
The default value of @code{split_long_rows} is nonzero.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
