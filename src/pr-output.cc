// pr-output.cc                                               -*- C++ -*-
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

#include <cfloat>
#include <cstring>

#include <string>

#include <iostream.h>
#include <strstream.h>

#include "CMatrix.h"
#include "Range.h"
#include "dMatrix.h"
#include "oct-cmplx.h"
#include "oct-math.h"
#include "oct-term.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "pager.h"
#include "pr-output.h"
#include "pt-const.h"
#include "sysdep.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

// Current format string for real numbers and the real part of complex
// numbers.
static char *curr_real_fmt = 0;

// Current format string for the imaginary part of complex numbers.
static char *curr_imag_fmt = 0;

// Nonzero means don't do any fancy formatting.
static int free_format = 0;

// Nonzero means print plus sign for nonzero, blank for zero.
static int plus_format = 0;

// Nonzero means always print like dollars and cents.
static int bank_format = 0;

// Nonzero means print data in hexadecimal format.
static int hex_format = 0;

// Nonzero means print data in binary-bit-pattern format.
static int bit_format = 0;

// Nonzero means don't put newlines around the column number headers.
static int compact_format = 0;

// Nonzero means use an e format.
static int print_e = 0;

// Nonzero means print E instead of e for exponent field.
static int print_big_e = 0;

static int
any_element_is_negative (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a.elem (i, j) < 0.0)
	return 1;
  return 0;
}

// XXX FIXME XXX -- these should probably be somewhere else.

int
any_element_is_inf_or_nan (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = a.elem (i, j);
	if (xisinf (val) || xisnan (val))
	  return 1;
      }
  return 0;
}

int
any_element_is_inf_or_nan (const ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = a.elem (i, j);
	if (xisinf (val) || xisnan (val))
	  return 1;
      }
  return 0;
}

static int
all_elements_are_int_or_inf_or_nan (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = a.elem (i, j);
	if (xisnan (val) || D_NINT (val) == val)
	  continue;
	else
	  return 0;
      }
  return 1;
}

static Matrix
abs (const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();
  Matrix retval (nr, nc);
  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval.elem (i, j) = fabs (a.elem (i, j));
  return retval;
}

static double
pr_max_internal (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  double result = DBL_MIN;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = m.elem (i, j);
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
	double val = m.elem (i, j);
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
set_real_format (int sign, int digits, int inf_or_nan, int nan_or_int,
		 int &fw)
{
  static char fmt_buf[128];

  int prec = user_pref.output_precision;

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
      && (fw > user_pref.output_max_field_width || print_e))
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

  int sign = (d < 0.0);

  int inf_or_nan = (xisinf (d) || xisnan (d));

  int nan_or_int = (xisnan (d) || D_NINT (d) == d);

  double d_abs = d < 0.0 ? -d : d;

  int digits = (inf_or_nan || d_abs == 0.0) ? 0
    : (int) floor (log10 (d_abs) + 1.0);

  set_real_format (sign, digits, inf_or_nan, nan_or_int, fw);
}

static inline void
set_format (double d)
{
  int fw;
  set_format (d, fw);
}

static void
set_real_matrix_format (int sign, int x_max, int x_min,
			int inf_or_nan, int int_or_inf_or_nan, int& fw)
{
  static char fmt_buf[128];

  int prec = user_pref.output_precision;

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
      && (fw > user_pref.output_max_field_width || print_e))
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
set_format (const Matrix& m, int& fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  int sign = any_element_is_negative (m);

  int inf_or_nan = any_element_is_inf_or_nan (m);

  int int_or_inf_or_nan = all_elements_are_int_or_inf_or_nan (m);

  Matrix m_abs = abs (m);
  double max_abs = pr_max_internal (m_abs);
  double min_abs = pr_min_internal (m_abs);

  int x_max = max_abs == 0.0 ? 0 : (int) floor (log10 (max_abs) + 1.0);
  int x_min = min_abs == 0.0 ? 0 : (int) floor (log10 (min_abs) + 1.0);

  set_real_matrix_format (sign, x_max, x_min, inf_or_nan,
			  int_or_inf_or_nan, fw);
}

static inline void
set_format (const Matrix& m)
{
  int fw;
  set_format (m, fw);
}

static void
set_complex_format (int sign, int x_max, int x_min, int r_x,
		    int inf_or_nan, int int_only, int& r_fw, int& i_fw)
{
  static char r_fmt_buf[128];
  static char i_fmt_buf[128];

  int prec = user_pref.output_precision;

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
      && (r_fw > user_pref.output_max_field_width || print_e))
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

  int sign = (rp < 0.0);

  int inf_or_nan = (xisinf (c) || xisnan (c));

  int int_only = (D_NINT (rp) == rp && D_NINT (ip) == ip);

  double r_abs = rp < 0.0 ? -rp : rp;
  double i_abs = ip < 0.0 ? -ip : ip;

  int r_x = r_abs == 0.0 ? 0 : (int) floor (log10 (r_abs) + 1.0);
  int i_x = i_abs == 0.0 ? 0 : (int) floor (log10 (i_abs) + 1.0);

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
set_complex_matrix_format (int sign, int x_max, int x_min,
			   int r_x_max, int r_x_min, int inf_or_nan,
			   int int_or_inf_or_nan, int& r_fw, int& i_fw)
{
  static char r_fmt_buf[128];
  static char i_fmt_buf[128];

  int prec = user_pref.output_precision;

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
      && (r_fw > user_pref.output_max_field_width || print_e))
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
set_format (const ComplexMatrix& cm, int& r_fw, int& i_fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  Matrix rp = real (cm);
  Matrix ip = imag (cm);

  int sign = any_element_is_negative (rp);

  int inf_or_nan = any_element_is_inf_or_nan (cm);

  int int_or_inf_or_nan = (all_elements_are_int_or_inf_or_nan (rp)
			   && all_elements_are_int_or_inf_or_nan (ip));

  Matrix r_m_abs = abs (rp);
  double r_max_abs = pr_max_internal (r_m_abs);
  double r_min_abs = pr_min_internal (r_m_abs);

  Matrix i_m_abs = abs (ip);
  double i_max_abs = pr_max_internal (i_m_abs);
  double i_min_abs = pr_min_internal (i_m_abs);

  int r_x_max = r_max_abs == 0.0 ? 0 : (int) floor (log10 (r_max_abs) + 1.0);
  int r_x_min = r_min_abs == 0.0 ? 0 : (int) floor (log10 (r_min_abs) + 1.0);

  int i_x_max = i_max_abs == 0.0 ? 0 : (int) floor (log10 (i_max_abs) + 1.0);
  int i_x_min = i_min_abs == 0.0 ? 0 : (int) floor (log10 (i_min_abs) + 1.0);

  int x_max = r_x_max > i_x_max ? r_x_max : i_x_max;
  int x_min = r_x_min > i_x_min ? r_x_min : i_x_min;

  set_complex_matrix_format (sign, x_max, x_min, r_x_max, r_x_min,
			     inf_or_nan, int_or_inf_or_nan, r_fw, i_fw);
}

static int
all_elements_are_ints (const Range& r)
{
  // If the base and increment are ints, the final value in the range
  // will also be an integer, even if the limit is not.

  double b = r.base ();
  double i = r.inc ();

  return (! (xisnan (b) || xisnan (i))
	  && (double) NINT (b) == b && (double) NINT (i) == i);
}

static inline void
set_format (const ComplexMatrix& cm)
{
  int r_fw, i_fw;
  set_format (cm, r_fw, i_fw);
}

static void
set_range_format (int sign, int x_max, int x_min, int all_ints, int& fw)
{
  static char fmt_buf[128];

  int prec = user_pref.output_precision;

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
      && (fw > user_pref.output_max_field_width || print_e))
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
set_format (const Range& r, int& fw)
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

  int sign = (r_min < 0.0);

  int all_ints = all_elements_are_ints (r);

  double max_abs = r_max < 0.0 ? -r_max : r_max;
  double min_abs = r_min < 0.0 ? -r_min : r_min;

  int x_max = max_abs == 0.0 ? 0 : (int) floor (log10 (max_abs) + 1.0);
  int x_min = min_abs == 0.0 ? 0 : (int) floor (log10 (min_abs) + 1.0);

  set_range_format (sign, x_max, x_min, all_ints, fw);
}

static inline void
set_format (const Range& r)
{
  int fw;
  set_format (r, fw);
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
      os.form ("%s", stmp); \
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
      os.form ("%s", stmp); \
    } \
  while (0)

static inline void
pr_any_float (const char *fmt, ostream& os, double d, int fw = 0)
{
  if (d == -0.0)
    d = 0.0;

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

	  if (hex_format > 1
	      || native_float_format == OCTAVE_IEEE_BIG
	      || native_float_format == OCTAVE_CRAY
	      || native_float_format == OCTAVE_UNKNOWN_FLT_FMT)
	    {
	      for (size_t i = 0; i < sizeof (double); i++)
		os.form ("%02x", (int) tmp.i[i]);
	    }
	  else
	    {
	      for (int i = sizeof (double) - 1; i >= 0; i--)
		os.form ("%02x", (int) tmp.i[i]);
	    }
	}
      else if (bit_format)
	{
	  equiv tmp;
	  tmp.d = d;

	  // Unless explicitly asked for, always print in big-endian
	  // format.

	  // XXX FIXME XXX -- is it correct to swap bytes for VAX
	  // formats and not for Cray?

	  if (native_float_format == OCTAVE_IEEE_BIG
	      || native_float_format == OCTAVE_CRAY
	      || native_float_format == OCTAVE_UNKNOWN_FLT_FMT)
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
	  char *s;
	  if (d < 0.0)
	    s = "-Inf";
	  else
	    s = "Inf";

	  if (fw > 0)
	    os.form ("%*s", fw, s);
	  else
	    os << s;
	}
      else if (xisnan (d))
	{
	  if (fw > 0)
	    os.form ("%*s", fw, "NaN");
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
pr_float (ostream& os, double d, int fw = 0)
{
  pr_any_float (curr_real_fmt, os, d, fw);
}

static inline void
pr_imag_float (ostream& os, double d, int fw = 0)
{
  pr_any_float (curr_imag_fmt, os, d, fw);
}

static inline void
pr_complex (ostream& os, const Complex& c, int r_fw = 0, int i_fw = 0)
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
print_empty_matrix (ostream& os, int nr, int nc, int pr_as_read_syntax)
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
      if (user_pref.print_empty_dimensions)
	os << "(" << nr << "x" << nc << ")";
      os << "\n";
    }
}

static void
pr_col_num_header (ostream& os, int total_width, int max_width,
		   int lim, int col)
{
  if (total_width > max_width && user_pref.split_long_rows)
    {
      if (col != 0 && ! compact_format)
	os << "\n";

      int num_cols = lim - col;

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

void
octave_print_internal (ostream& os, double d, int pr_as_read_syntax)
{
  if (plus_format)
    {
      if (d == 0.0)
	os << " ";
      else
	os << "+";
    }
  else
    {
      set_format (d);
      if (free_format)
	os << d;
      else
	pr_float (os, d);
    }

  if (! pr_as_read_syntax)
    os << "\n";
}

void
octave_print_internal (ostream& os, const Matrix& m, int pr_as_read_syntax)
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

	      if (m.elem (i, j) == 0.0)
		os << " ";
	      else
		os << "+";
	    }
	  os << "\n";
	}
    }
  else
    {
      int fw;
      set_format (m, fw);
      int column_width = fw + 2;
      int total_width = nc * column_width;
      int max_width = terminal_columns ();

      if (pr_as_read_syntax)
	max_width -= 4;

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
      if (total_width > max_width && user_pref.split_long_rows)
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

		      pr_float (os, m.elem (i, j));
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
	  for (int col = 0; col < nc; col += inc)
	    {
	      int lim = col + inc < nc ? col + inc : nc;

	      pr_col_num_header (os, total_width, max_width, lim, col);

	      for (int i = 0; i < nr; i++)
		{
		  for (int j = col; j < lim; j++)
		    {
		      os << "  ";

		      pr_float (os, m.elem (i, j), fw);
		    }

		  os << "\n";
		}
	    }
	}
    }
}

void
octave_print_internal (ostream& os, const Complex& c,
		       int pr_as_read_syntax)
{
  if (plus_format)
    {
      if (c == 0.0)
	os << " ";
      else
	os << "+";
    }
  else
    {
      set_format (c);
      if (free_format)
	os << c;
      else
	pr_complex (os, c);
    }

  if (! pr_as_read_syntax)
    os << "\n";
}

void
octave_print_internal (ostream& os, const ComplexMatrix& cm,
		       int pr_as_read_syntax)
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

	      if (cm.elem (i, j) == 0.0)
		os << " ";
	      else
		os << "+";
	    }
	  os << "\n";
	}
    }
  else
    {
      int r_fw, i_fw;
      set_format (cm, r_fw, i_fw);
      int column_width = i_fw + r_fw;
      column_width += (bank_format || hex_format|| bit_format) ? 2 : 7;
      int total_width = nc * column_width;
      int max_width = terminal_columns ();

      if (pr_as_read_syntax)
	max_width -= 4;

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
      if (total_width > max_width && user_pref.split_long_rows)
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

		      pr_complex (os, cm.elem (i, j));
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
	  for (int col = 0; col < nc; col += inc)
	    {
	      int lim = col + inc < nc ? col + inc : nc;

	      pr_col_num_header (os, total_width, max_width, lim, col);

	      for (int i = 0; i < nr; i++)
		{
		  for (int j = col; j < lim; j++)
		    {
		      os << "  ";

		      pr_complex (os, cm.elem (i, j));
		    }
		  os << "\n";
		}
	    }
	}
    }
}

void
octave_print_internal (ostream& os, const Range& r,
		       int pr_as_read_syntax)
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
      set_format (r, fw);

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
	  int max_width = terminal_columns ();

	  if (free_format)
	    {
	      os << r;
	      return;
	    }

	  int inc = num_elem;
	  if (total_width > max_width && user_pref.split_long_rows)
	    {
	      inc = max_width / column_width;
	      if (inc == 0)
		inc++;
	    }

	  int col = 0;
	  while (col < num_elem)
	    {
	      int lim = col + inc < num_elem ? col + inc : num_elem;

	      pr_col_num_header (os, total_width, max_width, lim, col);

	      for (int i = col; i < lim; i++)
		{
		  double val = base + i * increment;
		  os << "  ";
		  pr_float (os, val, fw);
		}

	      os << "\n";

	      col += inc;
	    }
	}
    }
}

void
octave_print_internal (ostream& os, const charMatrix& chm,
		       int pr_as_read_syntax, int pr_as_string)
{
  if (pr_as_string)
    {
      int nstr = chm.rows ();

      if (pr_as_read_syntax && nstr > 1)
	os << "[ ";

      for (int i = 0; i < nstr; i++)
	{
	  string row = chm.row_as_string (i);

	  if (pr_as_read_syntax)
	    {
	      os << "\"" << undo_string_escapes (row) << "\"";

	      if (i < nstr - 1)
		os << "; ";
	    }
	  else
	    os << row << "\n";
	}

      if (pr_as_read_syntax && nstr > 1)
	os << " ]";
    }
  else
    {
      os << "sorry, printing char matrices not implemented yet\n";
    }
}

DEFUN ("disp", Fdisp, Sdisp, 10,
  "disp (X): display value without name tag")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    args(0).eval (1);
  else
    print_usage ("disp");

  return retval;
}

static void
init_format_state (void)
{
  free_format = 0;
  plus_format = 0;
  bank_format = 0;
  hex_format = 0;
  bit_format = 0;
  print_e = 0;
  print_big_e = 0;
}

static void
set_output_prec_and_fw (int prec, int fw)
{
  tree_constant *tmp = 0;

  tmp = new tree_constant ((double) prec);
  bind_builtin_variable ("output_precision", tmp);

  tmp = new tree_constant ((double) fw);
  bind_builtin_variable ("output_max_field_width", tmp);
}

static void
set_format_style (int argc, const string_vector& argv)
{
  int idx = 1;
  string arg = argv[idx++];

  if (--argc > 0)
    {
      if (arg == "short")
	{
	  if (--argc > 0)
	    {
	      arg = argv[idx++];

	      if (arg == "e")
		{
		  init_format_state ();
		  print_e = 1;
		}
	      else if (arg == "E")
		{
		  init_format_state ();
		  print_e = 1;
		  print_big_e = 1;
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
		  print_e = 1;
		}
	      else if (arg == "E")
		{
		  init_format_state ();
		  print_e = 1;
		  print_big_e = 1;
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
	  hex_format = 1;
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
	  plus_format = 1;
	}
      else if (arg == "bank")
	{
	  init_format_state ();
	  bank_format = 1;
	}
      else if (arg == "free")
	{
	  init_format_state ();
	  free_format = 1;
	}
      else if (arg == "none")
	{
	  init_format_state ();
	  free_format = 1;
	}
      else if (arg == "compact")
	{
	  compact_format = 1;
	}
      else if (arg == "loose")
	{
	  compact_format = 0;
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

DEFUN_TEXT ("format", Fformat, Sformat, 10,
  "format [style]\n\
\n\
set output formatting style")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "format");

  if (error_state)
    return retval;

  set_format_style (argc, argv);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
