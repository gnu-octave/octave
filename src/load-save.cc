// load-save.cc                                              -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <climits>
#include <cstring>
#include <cctype>

#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "load-save.h"
#include "pager.h"
#include "symtab.h"
#include "sysdep.h"
#include "tree-base.h"
#include "tree-const.h"
#include "tree-expr.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"

extern "C"
{
#include <readline/tilde.h>

#include "fnmatch.h"
}

#if CHAR_BIT != 8
LOSE! LOSE!
#endif

#if SIZEOF_SHORT == 2
#define TWO_BYTE_INT short
#elif SIZEOF_INT == 2
#define TWO_BYTE_INT int
#else
LOSE! LOSE!
#endif

#if SIZEOF_INT == 4
#define FOUR_BYTE_INT int
#elif SIZEOF_LONG == 4
#define FOUR_BYTE_INT long
#else
LOSE! LOSE!
#endif

// Used when converting Inf to something that gnuplot can read.

#ifndef OCT_RBV
#define OCT_RBV DBL_MAX / 100.0
#endif

enum load_save_format
  {
    LS_ASCII,
    LS_BINARY,
    LS_MAT_BINARY,
    LS_UNKNOWN,
  };

// Not all of the following are currently used.

enum save_type
  {
    LS_U_CHAR,
    LS_U_SHORT,
    LS_U_INT,
    LS_CHAR,
    LS_SHORT,
    LS_INT,
    LS_FLOAT,
    LS_DOUBLE,
  };

#define swap_1_bytes(x,y)

#define LS_DO_READ(TYPE,swap,data,size,len,stream) \
  do \
    { \
      volatile TYPE *ptr = (TYPE *) data; \
      stream.read ((TYPE *) ptr, size * len); \
      if (swap) \
        swap_ ## size ## _bytes ((char *) ptr, len); \
      TYPE tmp = ptr[0]; \
      for (int i = len - 1; i > 0; i--) \
        data[i] = ptr[i]; \
      data[0] = tmp; \
    } \
  while (0)

// Have to use copy here to avoid writing over data accessed via
// Matrix::data().

#define LS_DO_WRITE(TYPE,data,size,len,stream) \
  do \
    { \
      char tmp_type = (char) type; \
      stream.write (&tmp_type, 1); \
      TYPE *ptr = new TYPE [len]; \
      for (int i = 0; i < len; i++) \
        ptr[i] = (TYPE) data[i]; \
      stream.write ((TYPE *) ptr, size * len); \
      delete [] ptr ; \
    } \
  while (0)

// Loading variables from files.

// But first, some data conversion routines.

// Currently, we only handle conversions for the IEEE types.  To fix
// that, make more of the following routines work.

#define LS_SWAP_BYTES(i,j) \
  tmp = t[i]; \
  t[i] = t[j]; \
  t[j] = tmp; \

static inline void
swap_2_bytes (char *t)
{
  char tmp;
  LS_SWAP_BYTES (0, 1);
}

static inline void
swap_4_bytes (char *t)
{
  char tmp;
  LS_SWAP_BYTES (0, 3);
  LS_SWAP_BYTES (1, 2);
}

static inline void
swap_8_bytes (char *t)
{
  char tmp;
  LS_SWAP_BYTES (0, 7);
  LS_SWAP_BYTES (1, 6);
  LS_SWAP_BYTES (2, 5);
  LS_SWAP_BYTES (3, 4);
}

static inline void
swap_2_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_2_bytes (ptr);
      ptr += 2;
    }
}

static inline void
swap_4_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_4_bytes (ptr);
      ptr += 4;
    }
}

static inline void
swap_8_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_8_bytes (ptr);
      ptr += 8;
    }
}

// XXX FIXME XXX -- assumes sizeof (Complex) == 8
// XXX FIXME XXX -- assumes sizeof (double) == 8
// XXX FIXME XXX -- assumes sizeof (float) == 4

static void
IEEE_big_double_to_IEEE_little_double (double *d, int len)
{
  swap_8_bytes ((char *) d, len);
}

static void
VAX_D_double_to_IEEE_little_double (double *d, int len)
{
  gripe_data_conversion ("VAX D float", "IEEE little endian format");
}

static void
VAX_G_double_to_IEEE_little_double (double *d, int len)
{
  gripe_data_conversion ("VAX G float", "IEEE little endian format");
}

static void
Cray_to_IEEE_little_double (double *d, int len)
{
  gripe_data_conversion ("Cray", "IEEE little endian format");
}

static void
IEEE_big_float_to_IEEE_little_float (float *d, int len)
{
  swap_4_bytes ((char *) d, len);
}

static void
VAX_D_float_to_IEEE_little_float (float *d, int len)
{
  gripe_data_conversion ("VAX D float", "IEEE little endian format");
}

static void
VAX_G_float_to_IEEE_little_float (float *d, int len)
{
  gripe_data_conversion ("VAX G float", "IEEE little endian format");
}

static void
Cray_to_IEEE_little_float (float *d, int len)
{
  gripe_data_conversion ("Cray", "IEEE little endian format");
}

static void
IEEE_little_double_to_IEEE_big_double (double *d, int len)
{
  swap_8_bytes ((char *) d, len);
}

static void
VAX_D_double_to_IEEE_big_double (double *d, int len)
{
  gripe_data_conversion ("VAX D float", "IEEE big endian format");
}

static void
VAX_G_double_to_IEEE_big_double (double *d, int len)
{
  gripe_data_conversion ("VAX G float", "IEEE big endian format");
}

static void
Cray_to_IEEE_big_double (double *d, int len)
{
  gripe_data_conversion ("Cray", "IEEE big endian format");
}

static void
IEEE_little_float_to_IEEE_big_float (float *d, int len)
{
  swap_4_bytes ((char *) d, len);
}

static void
VAX_D_float_to_IEEE_big_float (float *d, int len)
{
  gripe_data_conversion ("VAX D float", "IEEE big endian format");
}

static void
VAX_G_float_to_IEEE_big_float (float *d, int len)
{
  gripe_data_conversion ("VAX G float", "IEEE big endian format");
}

static void
Cray_to_IEEE_big_float (float *d, int len)
{
  gripe_data_conversion ("Cray", "IEEE big endian format");
}

static void
IEEE_little_double_to_VAX_D_double (double *d, int len)
{
  gripe_data_conversion ("IEEE little endian", "VAX D");
}

static void
IEEE_big_double_to_VAX_D_double (double *d, int len)
{
  gripe_data_conversion ("IEEE big endian", "VAX D");
}

static void
VAX_G_double_to_VAX_D_double (double *d, int len)
{
  gripe_data_conversion ("VAX G float", "VAX D");
}

static void
Cray_to_VAX_D_double (double *d, int len)
{
  gripe_data_conversion ("Cray", "VAX D");
}

static void
IEEE_little_float_to_VAX_D_float (float *d, int len)
{
  gripe_data_conversion ("IEEE little endian", "VAX D");
}

static void
IEEE_big_float_to_VAX_D_float (float *d, int len)
{
  gripe_data_conversion ("IEEE big endian", "VAX D");
}

static void
VAX_G_float_to_VAX_D_float (float *d, int len)
{
  gripe_data_conversion ("VAX G float", "VAX D");
}

static void
Cray_to_VAX_D_float (float *d, int len)
{
  gripe_data_conversion ("Cray", "VAX D");
}

static void
IEEE_little_double_to_VAX_G_double (double *d, int len)
{
  gripe_data_conversion ("IEEE little endian", "VAX G");
}

static void
IEEE_big_double_to_VAX_G_double (double *d, int len)
{
  gripe_data_conversion ("IEEE big endian", "VAX G");
}

static void
VAX_D_double_to_VAX_G_double (double *d, int len)
{
  gripe_data_conversion ("VAX D float", "VAX G");
}

static void
Cray_to_VAX_G_double (double *d, int len)
{
  gripe_data_conversion ("VAX G float", "VAX G");
}

static void
IEEE_little_float_to_VAX_G_float (float *d, int len)
{
  gripe_data_conversion ("IEEE little endian", "VAX G");
}

static void
IEEE_big_float_to_VAX_G_float (float *d, int len)
{
  gripe_data_conversion ("IEEE big endian", "VAX G");
}

static void
VAX_D_float_to_VAX_G_float (float *d, int len)
{
  gripe_data_conversion ("VAX D float", "VAX G");
}

static void
Cray_to_VAX_G_float (float *d, int len)
{
  gripe_data_conversion ("VAX G float", "VAX G");
}

static void
do_double_format_conversion (double *data, int len,
			     floating_point_format fmt)
{
  switch (native_float_format)
    {
    case OCTAVE_IEEE_LITTLE:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_double_to_IEEE_little_double (data, len);
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_double_to_IEEE_little_double (data, len);
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_double_to_IEEE_little_double (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_IEEE_little_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_IEEE_BIG:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_double_to_IEEE_big_double (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_double_to_IEEE_big_double (data, len);
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_double_to_IEEE_big_double (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_IEEE_big_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_VAX_D:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_double_to_VAX_D_double (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_double_to_VAX_D_double (data, len);
	  break;

	case OCTAVE_VAX_D:
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_double_to_VAX_D_double (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_VAX_D_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_VAX_G:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_double_to_VAX_G_double (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_double_to_VAX_G_double (data, len);
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_double_to_VAX_G_double (data, len);
	  break;

	case OCTAVE_VAX_G:
	  break;

	case OCTAVE_CRAY:
	  Cray_to_VAX_G_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    default:
      panic_impossible ();
    }
}

static void
do_float_format_conversion (float *data, int len,
			    floating_point_format fmt)
{
  switch (native_float_format)
    {
    case OCTAVE_IEEE_LITTLE:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_float_to_IEEE_little_float (data, len);
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_float_to_IEEE_little_float (data, len);
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_float_to_IEEE_little_float (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_IEEE_little_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_IEEE_BIG:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_float_to_IEEE_big_float (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_float_to_IEEE_big_float (data, len);
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_float_to_IEEE_big_float (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_IEEE_big_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_VAX_D:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_float_to_VAX_D_float (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_float_to_VAX_D_float (data, len);
	  break;

	case OCTAVE_VAX_D:
	  break;

	case OCTAVE_VAX_G:
	  VAX_G_float_to_VAX_D_float (data, len);
	  break;

	case OCTAVE_CRAY:
	  Cray_to_VAX_D_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case OCTAVE_VAX_G:
      switch (fmt)
	{
	case OCTAVE_IEEE_LITTLE:
	  IEEE_little_float_to_VAX_G_float (data, len);
	  break;

	case OCTAVE_IEEE_BIG:
	  IEEE_big_float_to_VAX_G_float (data, len);
	  break;

	case OCTAVE_VAX_D:
	  VAX_D_float_to_VAX_G_float (data, len);
	  break;

	case OCTAVE_VAX_G:
	  break;

	case OCTAVE_CRAY:
	  Cray_to_VAX_G_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    default:
      panic_impossible ();
    }
}

static void
read_doubles (istream& is, double *data, save_type type, int len,
	      int swap, floating_point_format fmt)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_READ (unsigned char, swap, data, 1, len, is);
      break;

    case LS_U_SHORT:
      LS_DO_READ (unsigned TWO_BYTE_INT, swap, data, 2, len, is);
      break;

    case LS_U_INT:
      LS_DO_READ (unsigned FOUR_BYTE_INT, swap, data, 4, len, is);
      break;

    case LS_CHAR:
      LS_DO_READ (signed char, swap, data, 1, len, is);
      break;

    case LS_SHORT:
      LS_DO_READ (TWO_BYTE_INT, swap, data, 2, len, is);
      break;

    case LS_INT:
      LS_DO_READ (FOUR_BYTE_INT, swap, data, 4, len, is);
      break;

    case LS_FLOAT:
      {
	volatile float *ptr = (float *) data;
	is.read (data, 4 * len);
	do_float_format_conversion ((float *) data, len, fmt);
	float tmp = ptr[0];
	for (int i = len - 1; i > 0; i--)
	  data[i] = ptr[i];
	data[0] = tmp;
      }
      break;

    case LS_DOUBLE:
      is.read (data, 8 * len);
      do_double_format_conversion (data, len, fmt);
      break;

    default:
      is.clear (ios::failbit|is.rdstate ());
      break;
    }
}

static void
write_doubles (ostream& os, const double *data, save_type type, int len)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_WRITE (unsigned char, data, 1, len, os);
      break;

    case LS_U_SHORT:
      LS_DO_WRITE (unsigned TWO_BYTE_INT, data, 2, len, os);
      break;

    case LS_U_INT:
      LS_DO_WRITE (unsigned FOUR_BYTE_INT, data, 4, len, os);
      break;

    case LS_CHAR:
      LS_DO_WRITE (signed char, data, 1, len, os);
      break;

    case LS_SHORT:
      LS_DO_WRITE (TWO_BYTE_INT, data, 2, len, os);
      break;

    case LS_INT:
      LS_DO_WRITE (FOUR_BYTE_INT, data, 4, len, os);
      break;

    case LS_FLOAT:
      LS_DO_WRITE (float, data, 4, len, os);
      break;

    case LS_DOUBLE:
      {
	char tmp_type = (char) type;
	os.write (&tmp_type, 1);
	os.write (data, 8 * len);
      }
      break;

    default:
      error ("unrecognized data format requested");
      break;
    }
}

// Return nonzero if S is a valid identifier.

static int
valid_identifier (char *s)
{
  if (! s || ! (isalnum (*s) || *s == '_'))
     return 0;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return 0;

  return 1;
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

static int
all_parts_int (const Matrix& m, double& max_val, double& min_val)
{
  int nr = m.rows ();
  int nc = m.columns ();

  if (nr > 0 && nc > 0)
    {
      max_val = m.elem (0, 0);
      min_val = m.elem (0, 0);
    }
  else
    return 0;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = m.elem (i, j);

	if (val > max_val)
	  max_val = val;

	if (val < min_val)
	  min_val = val;

	if (D_NINT (val) != val)
	  return 0;
      }
  return 1;
}

// Return nonzero if any element of CM has a non-integer real or
// imaginary part.  Also extract the largest and smallest (real or
// imaginary) values and return them in MAX_VAL and MIN_VAL. 

static int
all_parts_int (const ComplexMatrix& m, double& max_val, double& min_val)
{
  int nr = m.rows ();
  int nc = m.columns ();

  if (nr > 0 && nc > 0)
    {
      Complex val = m.elem (0, 0);

      double r_val = real (val);
      double i_val = imag (val);

      max_val = r_val;
      min_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (i_val < max_val)
	min_val = i_val;
    }
  else
    return 0;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = m.elem (i, j);

	double r_val = real (val);
	double i_val = imag (val);

	if (r_val > max_val)
	  max_val = r_val;

	if (i_val > max_val)
	  max_val = i_val;

	if (r_val < min_val)
	  min_val = r_val;

	if (i_val < min_val)
	  min_val = i_val;

	if (D_NINT (r_val) != r_val || D_NINT (i_val) != i_val)
	  return 0;
      }
  return 1;
}

static int
too_large_for_float (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	double val = m.elem (i, j);

	if (val > FLT_MAX || val < FLT_MIN)
	  return 1;
      }

  return 0;
}

static int
too_large_for_float (const ComplexMatrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = m.elem (i, j);

	double r_val = real (val);
	double i_val = imag (val);

	if (r_val > FLT_MAX
	    || i_val > FLT_MAX
	    || r_val < FLT_MIN
	    || i_val < FLT_MIN)
	  return 1;
      }

  return 0;
}

// XXX FIXME XXX -- shouldn't this be implemented in terms of other
// functions that are already available?

// Install a variable with name NAME and the value specified TC in the
// symbol table.  If FORCE is nonzero, replace any existing definition
// for NAME.  If GLOBAL is nonzero, make the variable global.
//
// Assumes TC is defined.

static void
install_loaded_variable (int force, char *name, const tree_constant& tc,
			 int global, char *doc)
{
  // Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (name, 0, 0);

  int is_undefined = 1;
  int is_variable = 0;
  int is_function = 0;
  int is_global = 0;

  if (lsr)
    {
      is_undefined = ! lsr->is_defined ();
      is_variable = lsr->is_variable ();
      is_function = lsr->is_function ();
      is_global = lsr->is_linked_to_global ();
    }

  symbol_record *sr = 0;

  if (global)
    {
      if (is_global || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", name);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", name);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", name);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }
  else
    {
      if (is_global)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", name);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", name);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, 1, 0);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", name);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }

  if (sr)
    {
      tree_constant *tmp_tc = new tree_constant (tc);
      sr->define (tmp_tc);
      if (doc)
	sr->document (doc);
      return;
    }
  else
    error ("load: unable to load variable `%s'", name);

  return;
}

// Functions for reading ascii data.

// Skip white space and comments on stream IS.

static void
skip_comments (istream& is)
{
  char c = '\0';
  while (is.get (c))
    {
      if (c == ' ' || c == '\t' || c == '\n')
	; // Skip whitespace on way to beginning of next line.
      else
	break;
    }

  for (;;)
    {
      if (is && c == '#')
	while (is.get (c) && c != '\n')
	  ; // Skip to beginning of next line, ignoring everything.
      else
	break;
    }
}

// Extract a KEYWORD and its value from stream IS, returning the
// associated value in a new string.
//
// Input should look something like:
//
//  #[ \t]*keyword[ \t]*:[ \t]*string-value[ \t]*\n

static char *
extract_keyword (istream& is, char *keyword)
{
  ostrstream buf;

  char *retval = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      ostrstream value;
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      if (c != '\n')
		{
		  value << c;
		  while (is.get (c) && c != '\n')
		    value << c;
		}
	      value << ends;
	      retval = value.str ();
	      break;
	    }
	}
    }

  if (retval)
    {
      int len = strlen (retval);
      if (len > 0)
	{
	  char *ptr = retval + len - 1;
	  while (*ptr == ' ' || *ptr == '\t')
	    ptr--;
	  *(ptr+1) = '\0';
	}
    }

  return retval;
}

// Match KEYWORD on stream IS, placing the associated value in VALUE,
// returning 1 if successful and 0 otherwise.
//
// Input should look something like:
//
//  [ \t]*keyword[ \t]*int-value.*\n

static int
extract_keyword (istream& is, char *keyword, int& value)
{
  ostrstream buf;

  int status = 0;
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      is.putback (c);
	      if (c != '\n')
		is >> value;
	      if (is)
		status = 1;
	      while (is.get (c) && c != '\n')
		; // Skip to beginning of next line;
	      break;
	    }
	}
    }
  return status;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return nonzero in GLOBAL.
//
// FILENAME is used for error messages.
//
// The data is expected to be in the following format:
//
// The input file must have a header followed by some data.
//
// All lines in the header must begin with a `#' character.
//
// The header must contain a list of keyword and value pairs with the
// keyword and value separated by a colon.
//
// Keywords must appear in the following order:
//
// # name: <name>
// # type: <type>
// # <info>
//
// Where:
//
//  <name> : a valid identifier
//
//  <type> : <typename>
//         | global <typename>
//
//  <typename> : scalar
//             | complex scalar
//             | matrix
//             | complex matrix
//             | string
//             | range
//
//  <info> : <matrix info>
//         | <string info>
//
//  <matrix info> : # rows: <integer>
//                | # columns: <integer>
//
//  <string info> : # len: <integer>
//
// Formatted ASCII data follows the header.
//
// Example:
//
//  # name: foo
//  # type: matrix
//  # rows: 2
//  # columns: 2
//    2  4
//    1  3
//
// XXX FIXME XXX -- this format is fairly rigid, and doesn't allow for
// arbitrary comments, etc.  Someone should fix that.

static char *
read_ascii_data (istream& is, const char *filename, int& global,
		 tree_constant& tc)
{
  // Read name for this entry or break on EOF.

  char *name = extract_keyword (is, "name");

  if (! name)
    return 0;

  if (! *name)
    {
      error ("load: empty name keyword found in file `%s'", filename);
      delete [] name;
      return 0;
    }
      

  if (! valid_identifier (name))
    {
      error ("load: bogus identifier `%s' found in file `%s'", name, filename);
      delete [] name;
      return 0;
    }

  // Look for type keyword.

  char *tag = extract_keyword (is, "type");

  if (tag && *tag)
    {
      char *ptr = strchr (tag, ' ');
      if (ptr)
	{
	  *ptr = '\0';
	  global = (strncmp (tag, "global", 6) == 0);
	  *ptr = ' ';
	  if (global)
	    ptr++;
	  else
	    ptr = tag;
	}
      else
	ptr = tag;

      if (strncmp (ptr, "scalar", 6) == 0)
	{
	  double tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load scalar constant");
	}
      else if (strncmp (ptr, "matrix", 6) == 0)
	{
	  int nr = 0, nc = 0;

	  if (extract_keyword (is, "rows", nr) && nr >= 0
	      && extract_keyword (is, "columns", nc) && nc >= 0)
	    {
	      if (nr > 0 && nc > 0)
		{
		  Matrix tmp (nr, nc);
		  is >> tmp;
		  if (is)
		    tc = tmp;
		  else
		    error ("load: failed to load matrix constant");
		}
	      else if (nr == 0 || nc == 0)
		tc = Matrix (nr, nc);
	      else
		panic_impossible ();
	    }
	  else
	    error ("load: failed to extract number of rows and columns");
	}
      else if (strncmp (ptr, "complex scalar", 14) == 0)
	{
	  Complex tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load complex scalar constant");
	}
      else if (strncmp (ptr, "complex matrix", 14) == 0)
	{
	  int nr = 0, nc = 0;

	  if (extract_keyword (is, "rows", nr) && nr > 0
	      && extract_keyword (is, "columns", nc) && nc > 0)
	    {
	      ComplexMatrix tmp (nr, nc);
	      is >> tmp;
	      if (is)
		tc = tmp;
	      else
		error ("load: failed to load complex matrix constant");
	    }
	  else
	    error ("load: failed to extract number of rows and columns");
	}
      else if (strncmp (ptr, "string", 6) == 0)
	{
	  int len;
	  if (extract_keyword (is, "length", len) && len > 0)
	    {
	      char *tmp = new char [len+1];
	      is.get (tmp, len+1, EOF);
	      if (is)
		tc = tmp;
	      else
		error ("load: failed to load string constant");
	    }
	  else
	    error ("load: failed to extract string length");
	}
      else if (strncmp (ptr, "range", 5) == 0)
	{
	  // # base, limit, range comment added by save().

	  skip_comments (is);
	  Range tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load range constant");
	}
      else
	error ("load: unknown constant type `%s'", tag);
    }
  else
    error ("load: failed to extract keyword specifying value type");

  delete [] tag;

  if (error_state)
    {
      error ("load: reading file %s", filename);
      return 0;
    }

  return name;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return nonzero in GLOBAL.  If SWAP
// is nonzero, swap bytes after reading.
//
// The data is expected to be in the following format:
//
// Header (one per file):
// =====================
//
//   object               type            bytes
//   ------               ----            -----
//   magic number         string             10
//
//   float format         integer             1  
//
//
// Data (one set for each item):
// ============================
//
//   object               type            bytes
//   ------               ----            -----
//   name_length          integer             4
//
//   name                 string    name_length
//
//   doc_length           integer             4
//
//   doc                  string     doc_length
//
//   global flag          integer             1
//
//   data type            integer             1
//
//   data (one of):
//
//     scalar:
//       data             real                8
//
//     complex scalar:
//       data             complex            16
//
//     matrix:
//       rows             integer             4
//       columns          integer             4
//       data             real            r*c*8
//
//     complex matrix:
//       rows             integer             4
//       columns          integer             4
//       data             complex        r*c*16
//
//     string:
//       length           int                 4
//       data             string         length
//
//     range:
//       base             real                8
//       limit            real                8
//       increment        real                8
//
// FILENAME is used for error messages.

static char *
read_binary_data (istream& is, int swap, floating_point_format fmt,
		  const char *filename, int& global,
		  tree_constant& tc, char *&doc)
{
  char tmp = 0;

  FOUR_BYTE_INT name_len = 0, doc_len = 0;
  char *name = 0;

  doc = 0;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another name should not result in an error.

  is.read (&name_len, 4);
  if (! is)
    return 0;
  if (swap)
    swap_4_bytes ((char *) &name_len);

  name = new char [name_len+1];
  name[name_len] = '\0';
  if (! is.read (name, name_len))
    goto data_read_error;

  is.read (&doc_len, 4);
  if (! is)
    goto data_read_error;
  if (swap)
    swap_4_bytes ((char *) &doc_len);

  doc = new char [doc_len+1];
  doc[doc_len] = '\0';
  if (! is.read (doc, doc_len))
    goto data_read_error;

  if (! is.read (&tmp, 1))
    goto data_read_error;
  global = tmp ? 1 : 0;

  tmp = 0;
  if (! is.read (&tmp, 1))
    goto data_read_error;

  switch (tmp)
    {
    case 1:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	double dtmp;
	read_doubles (is, &dtmp, (save_type) tmp, 1, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = dtmp;
      }
      break;

    case 2:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (&nr, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes ((char *) &nr);
	if (! is.read (&nc, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes ((char *) &nc);
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	Matrix m (nr, nc);
	double *re = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, re, (save_type) tmp, len, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 3:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	Complex ctmp;
	read_doubles (is, (double *) &ctmp, (save_type) tmp, 2, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = ctmp;
      }
      break;

    case 4:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (&nr, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes ((char *) &nr);
	if (! is.read (&nc, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes ((char *) &nc);
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	ComplexMatrix m (nr, nc);
	Complex *im = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, (double *) im, (save_type) tmp, 2*len,
		      swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 5:
      {
	int nr = tc.rows ();
	int nc = tc.columns ();
	FOUR_BYTE_INT len = nr * nc;
	if (! is.read (&len, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes ((char *) &len);
	char *s = new char [len+1];
	if (! is.read (s, len))
	  {
	    delete [] s;
	    goto data_read_error;
	  }
	s[len] = '\0';
	tc = s;
      }
      break;

    case 6:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	double bas, lim, inc;
	if (! is.read (&bas, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes ((char *) &bas);
	if (! is.read (&lim, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes ((char *) &lim);
	if (! is.read (&inc, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes ((char *) &inc);
	Range r (bas, lim, inc);
	tc = r;
      }
      break;

    default:
    data_read_error:
      error ("load: trouble reading binary file `%s'", filename);
      delete [] name;
      name = 0;
      break;
    }

  return name;
}

// Read LEN elements of data from IS in the format specified by
// PRECISION, placing the result in DATA.  If SWAP is nonzero, swap
// the bytes of each element before copying to DATA.  FLT_FMT
// specifies the format of the data if we are reading floating point
// numbers.

static void
read_mat_binary_data (istream& is, double *data, int precision,
		      int len, int swap, floating_point_format flt_fmt)
{
  switch (precision)
    {
    case 0:
      read_doubles (is, data, LS_DOUBLE, len, swap, flt_fmt);
      break;

    case 1:
      read_doubles (is, data, LS_FLOAT, len, swap, flt_fmt);
      break;

    case 2:
      read_doubles (is, data, LS_INT, len, swap, flt_fmt);
      break;

    case 3:
      read_doubles (is, data, LS_SHORT, len, swap, flt_fmt);
      break;

    case 4:
      read_doubles (is, data, LS_U_SHORT, len, swap, flt_fmt);
      break;

    case 5:
      read_doubles (is, data, LS_U_CHAR, len, swap, flt_fmt);
      break;

    default:
      break;
    }
}

static int
read_mat_file_header (istream& is, int& swap, FOUR_BYTE_INT& mopt, 
		      FOUR_BYTE_INT& nr, FOUR_BYTE_INT& nc,
		      FOUR_BYTE_INT& imag, FOUR_BYTE_INT& len,
		      int quiet = 0)
{
  swap = 0;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another mopt value should not result in an
  // error.

  is.read (&mopt, 4);
  if (! is)
    return 1;

  if (! is.read (&nr, 4))
    goto data_read_error;

  if (! is.read (&nc, 4))
    goto data_read_error;

  if (! is.read (&imag, 4))
    goto data_read_error;

  if (! is.read (&len, 4))
    goto data_read_error;

// If mopt is nonzero and the byte order is swapped, mopt will be
// bigger than we expect, so we swap bytes.
//
// If mopt is zero, it means the file was written on a little endian
// machine, and we only need to swap if we are running on a big endian
// machine.
//
// Gag me.

#if defined (WORDS_BIGENDIAN)
  if (mopt == 0)
    swap = 1;
#endif

  // mopt is signed, therefore byte swap may result in negative value.

  if (mopt > 9999 || mopt < 0)
    swap = 1;

  if (swap)
    {
      swap_4_bytes ((char *) &mopt);
      swap_4_bytes ((char *) &nr);
      swap_4_bytes ((char *) &nc);
      swap_4_bytes ((char *) &imag);
      swap_4_bytes ((char *) &len);
    }

  if (mopt > 9999 || mopt < 0 || imag > 1 || imag < 0)
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }

  return 0;

 data_read_error:
  return -1;
}

// We don't just use a cast here, because we need to be able to detect
// possible errors.

static floating_point_format
get_floating_point_format (int mach)
{
  floating_point_format flt_fmt = OCTAVE_UNKNOWN_FLT_FMT;

  switch (mach)
    {
    case 0:
      flt_fmt = OCTAVE_IEEE_LITTLE;
      break;

    case 1:
      flt_fmt = OCTAVE_IEEE_BIG;
      break;

    case 2:
      flt_fmt = OCTAVE_VAX_D;
      break;

    case 3:
      flt_fmt = OCTAVE_VAX_G;
      break;

    case 4:
      flt_fmt = OCTAVE_CRAY;
      break;

    default:
      flt_fmt = OCTAVE_UNKNOWN_FLT_FMT;
      break;
    }

  return flt_fmt;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.
//
// The data is expected to be in Matlab's .mat format, though not all
// the features of that format are supported.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

static char *
read_mat_binary_data (istream& is, const char *filename,
		      tree_constant& tc)
{
  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  Matrix re;
  floating_point_format flt_fmt = OCTAVE_UNKNOWN_FLT_FMT;
  char *name = 0;
  int swap = 0, type = 0, prec = 0, mach = 0, dlen = 0;

  FOUR_BYTE_INT mopt, nr, nc, imag, len;

  int err = read_mat_file_header (is, swap, mopt, nr, nc, imag, len);
  if (err)
    {
      if (err < 0)
	goto data_read_error;
      else
	return 0;
    }

  type = mopt % 10; // Full, sparse, etc.
  mopt /= 10;       // Eliminate first digit.
  prec = mopt % 10; // double, float, int, etc.
  mopt /= 100;      // Skip unused third digit too.
  mach = mopt % 10; // IEEE, VAX, etc.

  flt_fmt = get_floating_point_format (mach);
  if (flt_fmt == OCTAVE_UNKNOWN_FLT_FMT)
    {
      error ("load: unrecognized binary format!");
      return 0;
    }

  if (type != 0 && type != 1)
    {
      error ("load: can't read sparse matrices");
      return 0;
    }

  if (imag && type == 1)
    {
      error ("load: encountered complex matrix with string flag set!");
      return 0;
    }

  name = new char [len];
  if (! is.read (name, len))
    goto data_read_error;

  dlen = nr * nc;
  if (dlen < 0)
    goto data_read_error;

  re.resize (nr, nc);

  read_mat_binary_data (is, re.fortran_vec (), prec, dlen, swap, flt_fmt);

  if (! is || error_state)
    {
      error ("load: reading matrix data for `%s'", name);
      goto data_read_error;
    }

  if (imag)
    {
      Matrix im (nr, nc);

      read_mat_binary_data (is, im.fortran_vec (), prec, dlen, swap, flt_fmt);

      if (! is || error_state)
	{
	  error ("load: reading imaginary matrix data for `%s'", name);
	  goto data_read_error;
	}

      ComplexMatrix ctmp (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  ctmp.elem (i, j) = Complex (re.elem (i, j), im.elem (i, j));

      tc = ctmp;
    }
  else
    tc = re;

  // XXX FIXME XXX -- this needs to change once strings really work.

  if (type == 1 && nr == 1)
    tc = tc.convert_to_str ();

  return name;

 data_read_error:
  error ("load: trouble reading binary file `%s'", filename);
  delete [] name;
  return 0;
}

// Return nonzero if NAME matches one of the given globbing PATTERNS.

static int
matches_patterns (char **patterns, int num_pat, char *name)
{
  while (num_pat-- > 0)
    {
      if (fnmatch (*patterns++, name, __FNM_FLAGS) == 0)
	return 1;
    }
  return 0;
}

static int
read_binary_file_header (istream& is, int& swap,
			 floating_point_format& flt_fmt, int quiet = 0) 
{
  int magic_len = 10;
  char magic [magic_len+1];
  is.read (magic, magic_len);
  magic[magic_len] = '\0';
  if (strncmp (magic, "Octave-1-L", magic_len) == 0)
    {
#if defined (WORDS_BIGENDIAN)
      swap = 1;
#else
      swap = 0;
#endif
    }
  else if (strncmp (magic, "Octave-1-B", magic_len) == 0)
    {
#if defined (WORDS_BIGENDIAN)
      swap = 0;
#else
      swap = 1;
#endif
    }
  else
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }
	
  char tmp = 0;
  is.read (&tmp, 1);

  flt_fmt = get_floating_point_format (tmp);
  if (flt_fmt == OCTAVE_UNKNOWN_FLT_FMT)
    {
      if (! quiet)
        error ("load: unrecognized binary format!");
      return -1;
    }

  return 0;
}

static load_save_format
get_file_format (const char *fname, const char *orig_fname)
{
  load_save_format retval = LS_UNKNOWN;

  ifstream file (fname);

  if (! file)
    {
      error ("load: couldn't open input file `%s'", orig_fname);
      return retval;
    }

  int swap;
  floating_point_format flt_fmt = OCTAVE_UNKNOWN_FLT_FMT;

  if (read_binary_file_header (file, swap, flt_fmt, 1) == 0)
    retval = LS_BINARY;
  else
    {
      file.seekg (0, ios::beg);

      FOUR_BYTE_INT mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len, 1);

      if (! err)
	retval = LS_MAT_BINARY;
      else
	{
	  file.seekg (0, ios::beg);

	  char *tmp = extract_keyword (file, "name");

	  if (tmp)
	    {
	      retval = LS_ASCII;
	      delete [] tmp;
	    }
	}
    }

  file.close ();

  if (retval == LS_UNKNOWN)
    error ("load: unable to determine file format for `%s'", orig_fname);

  return retval;
}

static Octave_object
do_load (istream& stream, const char *orig_fname, int force,
	 load_save_format format, floating_point_format flt_fmt,
	 int list_only, int swap, int verbose, char **argv,
	 int argc, int nargout)
{
  Octave_object retval;

  ostrstream output_buf;
  int count = 0;
  for (;;)
    {
      int global = 0;
      tree_constant tc;

      char *name = 0;
      char *doc = 0;

      switch (format)
	{
	case LS_ASCII:
	  name = read_ascii_data (stream, orig_fname, global, tc);
	  break;

	case LS_BINARY:
	  name = read_binary_data (stream, swap, flt_fmt, orig_fname,
				   global, tc, doc);
	  break;

	case LS_MAT_BINARY:
	  name = read_mat_binary_data (stream, orig_fname, tc);
	  break;

	default:
	  gripe_unrecognized_data_fmt ("load");
	  break;
	}

      if (error_state || stream.eof () || ! name)
	{
	  delete [] name;
	  delete [] doc;
	  break;
	}
      else if (! error_state && name)
	{
	  if (tc.is_defined ())
	    {
	      if (argc == 0 || matches_patterns (argv, argc, name))
		{
		  count++;
		  if (list_only)
		    {
		      if (verbose)
			{
			  if (count == 1)
			    output_buf
			      << "type               rows   cols   name\n"
			      << "====               ====   ====   ====\n";

			  output_buf.form ("%-16s", tc.type_as_string ());
			  output_buf.form ("%7d", tc.rows ());
			  output_buf.form ("%7d", tc.columns ());
			  output_buf << "   ";
			}
		      output_buf << name << "\n";
		    }
		  else
		    {
		      install_loaded_variable (force, name, tc, global, doc);
		    }
		}
	    }
	  else
	    error ("load: unable to load variable `%s'", name);
	}
      else
	{
	  if (count == 0)
	    error ("load: are you sure `%s' is an Octave data file?",
		   orig_fname);

	  delete [] name;
	  delete [] doc;
	  break;
	}

      delete [] name;
      delete [] doc;
    }

  if (list_only && count)
    {
      if (nargout > 0)
	{
	  output_buf << ends;
	  char *msg = output_buf.str ();
	  retval = msg;
	  delete [] msg;
	}
      else
	maybe_page_output (output_buf);
    }

  return retval;
}

DEFUN_TEXT ("load", Fload, Sload, -1, 1,
  "load [-force] [-ascii] [-binary] [-mat-binary] file [pattern ...]\n\
\n\
Load variables from a file.\n\
\n\
If no argument is supplied to select a format, load tries to read the
named file as an Octave binary, then as a .mat file, and then as an
Octave text file.\n\
\n\
If the option -force is given, variables with the same names as those
found in the file will be replaced with the values read from the file.")
{
  Octave_object retval;

  DEFINE_ARGV ("load");

  argc--;
  argv++;

  int force = 0;

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = LS_UNKNOWN;

  int list_only = 0;
  int verbose = 0;

  while (argc > 0)
    {
      if (strcmp (*argv, "-force") == 0 || strcmp (*argv, "-f") == 0)
	{
	  force++;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-list") == 0 || strcmp (*argv, "-l") == 0)
	{
	  list_only = 1;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-verbose") == 0 || strcmp (*argv, "-v") == 0)
	{
	  verbose = 1;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-ascii") == 0 || strcmp (*argv, "-a") == 0)
	{
	  format = LS_ASCII;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-binary") == 0 || strcmp (*argv, "-b") == 0)
	{
	  format = LS_BINARY;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-mat-binary") == 0 || strcmp (*argv, "-m") == 0)
	{
	  format = LS_MAT_BINARY;
	  argc--;
	  argv++;
	}
      else
	break;
    }

  if (argc < 1)
    {
      error ("load: you must specify a single file to read");
      DELETE_ARGV;
      return retval;
    }

  char *orig_fname = *argv;

  floating_point_format flt_fmt = OCTAVE_UNKNOWN_FLT_FMT;

  int swap = 0;

  if (strcmp (*argv, "-") == 0)
    {
      argc--;
      argv++;

      if (format != LS_UNKNOWN)
	{
	  // XXX FIXME XXX -- if we have already seen EOF on a
	  // previous call, how do we fix up the state of cin so that
	  // we can get additional input?  I'm afraid that we can't
	  // fix this using cin only.

	  retval = do_load (cin, orig_fname, force, format, flt_fmt,
			    list_only, swap, verbose, argv, argc,
			    nargout);
	}
      else
	error ("load: must specify file format if reading from stdin");
    }
  else
    {
      static char *fname = 0;

      if (fname)
	free (fname);

      fname = tilde_expand (*argv);

      if (format == LS_UNKNOWN)
	format = get_file_format (fname, orig_fname);

      if (format != LS_UNKNOWN)
	{
	  argv++;
	  argc--;

	  unsigned mode = ios::in;
	  if (format == LS_BINARY || format == LS_MAT_BINARY)
	    mode |= ios::bin;

	  ifstream file (fname, mode);

	  if (file)
	    {
	      if (format == LS_BINARY)
		{
		  if (read_binary_file_header (file, swap, flt_fmt) < 0)
		    {
		      file.close ();
		      DELETE_ARGV;
		      return retval;
		    }
		}

	      retval = do_load (file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose,
				argv, argc, nargout);

	      file.close ();
	    }
	  else
	    error ("load: couldn't open input file `%s'", orig_fname);
	}
    }

  DELETE_ARGV;

  return retval;
}

// Return nonzero if PATTERN has any special globbing chars in it.

static int
glob_pattern_p (char *pattern)
{
  char *p = pattern;
  char c;
  int open = 0;

  while ((c = *p++) != '\0')
    {
      switch (c)
	{
	case '?':
	case '*':
	  return 1;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return 1;
	  continue;
	  
	case '\\':
	  if (*p++ == '\0')
	    return 0;

	default:
	  continue;
	}
    }

  return 0;
}

// MAX_VAL and MIN_VAL are assumed to have integral values even though
// they are stored in doubles.

static save_type
get_save_type (double max_val, double min_val)
{
  save_type st = LS_DOUBLE;

  if (max_val < 256 && min_val > -1)
    st = LS_U_CHAR;
  else if (max_val < 65536 && min_val > -1)
    st = LS_U_SHORT;
  else if (max_val < 4294967295 && min_val > -1)
    st = LS_U_INT;
  else if (max_val < 128 && min_val >= -128)
    st = LS_CHAR;
  else if (max_val < 32768 && min_val >= -32768)
    st = LS_SHORT;
  else if (max_val < 2147483648 && min_val > -2147483648)
    st = LS_INT;

  return st;
}

// Save the data from TC along with the corresponding NAME, help
// string DOC, and global flag MARK_AS_GLOBAL on stream OS in the
// binary format described above for load_binary_data.

static int
save_binary_data (ostream& os, const tree_constant& tc, char *name,
		  char *doc, int mark_as_global, int save_as_floats) 
{
  int fail = 0;

  FOUR_BYTE_INT name_len = 0;
  if (name)
    name_len = strlen (name);

  os.write (&name_len, 4);
  os.write (name, name_len);

  FOUR_BYTE_INT doc_len = 0;
  if (doc)
    doc_len = strlen (doc);

  os.write (&doc_len, 4);
  os.write (doc, doc_len);

  char tmp;

  tmp = mark_as_global;
  os.write (&tmp, 1);

  if (tc.is_real_scalar ())
    {
      tmp = 1;
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      double tmp = tc.double_value ();
      os.write (&tmp, 8);
    }
  else if (tc.is_real_matrix ())
    {
      tmp = 2;
      os.write (&tmp, 1);
      Matrix m = tc.matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (&nr, 4);
      os.write (&nc, 4);
      int len = nr * nc;
      save_type st = LS_DOUBLE;
      if (save_as_floats)
	{
	  if (too_large_for_float (m))
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    st = LS_FLOAT;
	}
      else if (len > 8192) // XXX FIXME XXX -- make this configurable.
	{
	  double max_val, min_val;
	  if (all_parts_int (m, max_val, min_val))
	    st = get_save_type (max_val, min_val);
	}
      const double *mtmp = m.data ();
      write_doubles (os, mtmp, st, len);
    }
  else if (tc.is_complex_scalar ())
    {
      tmp = 3;
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      Complex tmp = tc.complex_value ();
      os.write (&tmp, 16);
    }
  else if (tc.is_complex_matrix ())
    {
      tmp = 4;
      os.write (&tmp, 1);
      ComplexMatrix m = tc.complex_matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (&nr, 4);
      os.write (&nc, 4);
      int len = nr * nc;
      save_type st = LS_DOUBLE;
      if (save_as_floats)
	{
	  if (too_large_for_float (m))
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    st = LS_FLOAT;
	}
      else if (len > 4096) // XXX FIXME XXX -- make this configurable.
	{
	  double max_val, min_val;
	  if (all_parts_int (m, max_val, min_val))
	    st = get_save_type (max_val, min_val);
	}
      const Complex *mtmp = m.data ();
      write_doubles (os, (const double *) mtmp, st, 2*len);
    }
  else if (tc.is_string ())
    {
      tmp = 5;
      os.write (&tmp, 1);
      int nr = tc.rows ();
      int nc = tc.columns ();
      FOUR_BYTE_INT len = nr * nc;
      os.write (&len, 4);
      const char *s = tc.string_value ();
      os.write (s, len);
    }
  else if (tc.is_range ())
    {
      tmp = 6;
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      Range r = tc.range_value ();
      double bas = r.base ();
      double lim = r.limit ();
      double inc = r.inc ();
      os.write (&bas, 8);
      os.write (&lim, 8);
      os.write (&inc, 8);
    }
  else
    {
      gripe_wrong_type_arg ("save", tc);
      fail = 1;
    }

  return (os && ! fail);
}

// Save the data from TC along with the corresponding NAME on stream OS 
// in the MatLab binary format.

static int
save_mat_binary_data (ostream& os, const tree_constant& tc, char *name) 
{
  int fail = 0;

  FOUR_BYTE_INT mopt = 0;

  mopt += tc.is_string () ? 1 : 0;
  mopt += 1000 * get_floating_point_format (native_float_format);

  os.write (&mopt, 4);
  
  FOUR_BYTE_INT nr = tc.rows ();
  os.write (&nr, 4);

  FOUR_BYTE_INT nc = tc.columns ();
  os.write (&nc, 4);

  int len = nr * nc;

  FOUR_BYTE_INT imag = tc.is_complex_type () ? 1 : 0;
  os.write (&imag, 4);

  FOUR_BYTE_INT name_len = name ? strlen (name) + 1 : 0;

  os.write (&name_len, 4);
  os.write (name, name_len);

  if (tc.is_real_scalar ())
    {
      double tmp = tc.double_value ();
      os.write (&tmp, 8);
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      os.write (m.data (), 8 * len);
    }
  else if (tc.is_complex_scalar ())
    {
      Complex tmp = tc.complex_value ();
      os.write (&tmp, 16);
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real(m_cmplx);
      os.write (m.data (), 8 * len);
      m = ::imag(m_cmplx);
      os.write (m.data (), 8 * len);
    }
  else if (tc.is_string ())
    {
      begin_unwind_frame ("save_mat_binary_data");
      unwind_protect_int (user_pref.implicit_str_to_num_ok);
      user_pref.implicit_str_to_num_ok = 1;
      Matrix m = tc.matrix_value ();
      os.write (m.data (), 8 * len);
      run_unwind_frame ("save_mat_binary_data");
    }
  else if (tc.is_range ())
    {
      Range r = tc.range_value ();
      double base = r.base ();
      double inc = r.inc ();
      int nel = r.nelem ();
      for (int i = 0; i < nel; i++)
	{
	  double x = base + i * inc;
	  os.write (&x, 8);
	}
    }
  else
    {
      gripe_wrong_type_arg ("save", tc);
      fail = 1;
    }

  return (os && ! fail);
}

static void
ascii_save_type (ostream& os, char *type, int mark_as_global)
{
  if (mark_as_global)
    os << "# type: global ";
  else
    os << "# type: ";

  os << type << "\n";
}

static Matrix
strip_infnan (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix retval (nr, nc);

  int k = 0;
  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < nc; j++)
	{
	  double d = m.elem (i, j);
	  if (xisnan (d))
	    goto next_row;
	  else
	    retval.elem (k, j) = xisinf (d) ? (d > 0 ? OCT_RBV : -OCT_RBV) : d;
	}
      k++;

    next_row:
      continue;
    }

  if (k > 0)
    retval.resize (k, nc);

  return retval;
}

static ComplexMatrix
strip_infnan (const ComplexMatrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix retval (nr, nc);

  int k = 0;
  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < nc; j++)
	{
	  Complex c = m.elem (i, j);
	  if (xisnan (c))
	    goto next_row;
	  else
	    {
	      double re = real (c);
	      double im = imag (c);

	      re = xisinf (re) ? (re > 0 ? OCT_RBV : -OCT_RBV) : re;
	      im = xisinf (im) ? (im > 0 ? OCT_RBV : -OCT_RBV) : im;

	      retval.elem (k, j) = Complex (re, im);
	    }
	}
      k++;

    next_row:
      continue;
    }

  if (k > 0)
    retval.resize (k, nc);

  return retval;
}

// Save the data from TC along with the corresponding NAME, and global
// flag MARK_AS_GLOBAL on stream OS in the plain text format described
// above for load_ascii_data.  If NAME is null, the name: line is not
// generated.  PRECISION specifies the number of decimal digits to print. 
// If STRIP_NAN_AND_INF is nonzero, rows containing NaNs are deleted,
// and Infinite values are converted to +/-OCT_RBV (A Real Big Value,
// but not so big that gnuplot can't handle it when trying to compute
// axis ranges, etc.).
//
// Assumes ranges and strings cannot contain Inf or NaN values.
//
// Returns 1 for success and 0 for failure.

// XXX FIXME XXX -- should probably write the help string here too.

int
save_ascii_data (ostream& os, const tree_constant& tc,
		 char *name, int strip_nan_and_inf,
		 int mark_as_global, int precision) 
{
  int success = 1;

  if (! precision)
    precision = user_pref.save_precision;

  if (name)
    os << "# name: " << name << "\n";

  long old_precision = os.precision ();
  os.precision (precision);

  if (tc.is_real_scalar ())
    {
      ascii_save_type (os, "scalar", mark_as_global);

      double d = tc.double_value ();
      if (strip_nan_and_inf)
	{
	  if (xisnan (d))
	    {
	      error ("only value to plot is NaN");
	      success = 0;
	    }
	  else
	    {
	      d = xisinf (d) ? (d > 0 ? OCT_RBV : -OCT_RBV) : d;
	      os << d << "\n";
	    }
	}
      else
	os << d << "\n";
    }
  else if (tc.is_real_matrix ())
    {
      ascii_save_type (os, "matrix", mark_as_global);
      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      Matrix tmp = tc.matrix_value ();
      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);

      os << tmp;
    }
  else if (tc.is_complex_scalar ())
    {
      ascii_save_type (os, "complex scalar", mark_as_global);

      Complex c = tc.complex_value ();
      if (strip_nan_and_inf)
	{
	  if (xisnan (c))
	    {
	      error ("only value to plot is NaN");
	      success = 0;
	    }
	  else
	    {
	      double re = real (c);
	      double im = imag (c);

	      re = xisinf (re) ? (re > 0 ? OCT_RBV : -OCT_RBV) : re;
	      im = xisinf (im) ? (im > 0 ? OCT_RBV : -OCT_RBV) : im;

	      c = Complex (re, im);

	      os << c << "\n";
	    }
	}
      else
	os << c << "\n";
    }
  else if (tc.is_complex_matrix ())
    {
      ascii_save_type (os, "complex matrix", mark_as_global);
      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      ComplexMatrix tmp = tc.complex_matrix_value ();
      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);

      os << tmp;
    }
  else if (tc.is_string ())
    {
      ascii_save_type (os, "string", mark_as_global);
      const char *tmp = tc.string_value ();
      os << "# length: " << strlen (tmp) << "\n"
	 << tmp << "\n";
    }
  else if (tc.is_range ())
    {
      ascii_save_type (os, "range", mark_as_global);
      Range tmp = tc.range_value ();
      os << "# base, limit, increment\n"
	 << tmp.base () << " "
	 << tmp.limit () << " "
	 << tmp.inc () << "\n";
    }
  else
    {
      gripe_wrong_type_arg ("save", tc);
      success = 0;
    }

  os.precision (old_precision);

  return (os && success);
}

// Save the info from sr on stream os in the format specified by fmt.

static void
do_save (ostream& os, symbol_record *sr, load_save_format fmt,
	 int save_as_floats)
{
  if (! sr->is_variable ())
    {
      error ("save: can only save variables, not functions");
      return;
    }

  char *name = sr->name ();
  char *help = sr->help ();
  int global = sr->is_linked_to_global ();
  tree_constant tc = *((tree_constant *) sr->def ());

  if (! name || ! tc.is_defined ())
    return;

  switch (fmt)
    {
    case LS_ASCII:
      save_ascii_data (os, tc, name, 0, global);
      break;

    case LS_BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case LS_MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

    default:
      gripe_unrecognized_data_fmt ("save");
      break;
    }
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.  If SAVE_BUILTINS is nonzero, also save
// builtin variables with names that match PATTERN.

static int
save_vars (ostream& os, char *pattern, int save_builtins,
	   load_save_format fmt, int save_as_floats)
{
  int count;

  symbol_record **vars = curr_sym_tab->glob
    (count, pattern, symbol_def::USER_VARIABLE, SYMTAB_ALL_SCOPES);

  int saved = count;

  int i;

  for (i = 0; i < count; i++)
    {
      do_save (os, vars[i], fmt, save_as_floats);

      if (error_state)
	break;
    }

  delete [] vars;

  if (! error_state && save_builtins)
    {
      symbol_record **vars = global_sym_tab->glob
	(count, pattern, symbol_def::BUILTIN_VARIABLE, SYMTAB_ALL_SCOPES);

      saved += count;

      for (i = 0; i < count; i++)
	{
	  do_save (os, vars[i], fmt, save_as_floats);

	  if (error_state)
	    break;
	}

      delete [] vars;
    }

  return saved;
}

static load_save_format
get_default_save_format (void)
{
  load_save_format retval = LS_ASCII;

  char *fmt = user_pref.default_save_format;

  if (strcasecmp (fmt, "binary") == 0)
    retval = LS_BINARY;
  else if (strcasecmp (fmt, "mat-binary") == 0
	   || strcasecmp (fmt, "mat_binary") == 0)
    retval = LS_MAT_BINARY;
      
  return retval;
}

static void
write_binary_header (ostream& stream, load_save_format format)
{
  if (format == LS_BINARY)
    {
#if defined (WORDS_BIGENDIAN)
      stream << "Octave-1-B";
#else
      stream << "Octave-1-L";
#endif

      char tmp = (char) native_float_format;
      stream.write (&tmp, 1);
    }
}

static void
save_vars (char **argv, int argc, ostream& os, int save_builtins,
	   load_save_format fmt, int save_as_floats)
{
  write_binary_header (os, fmt);

  if (argc == 0)
    {
      save_vars (os, "*", save_builtins, fmt, save_as_floats);
    }
  else
    {
      while (argc-- > 0)
	{
	  if (! save_vars (os, *argv, save_builtins, fmt, save_as_floats))
	    {
	      warning ("save: no such variable `%s'", *argv);
	    }

	  argv++;
	}
    }
}

void
save_user_variables (void)
{
  // XXX FIXME XXX -- should choose better file name?

  const char *fname = "octave-core";

  message (0, "attempting to save variables to `%s'...", fname);

  load_save_format format = get_default_save_format ();

  unsigned mode = ios::out|ios::trunc;
  if (format == LS_BINARY || format == LS_MAT_BINARY)
    mode |= ios::bin;

  ofstream file (fname, mode);

  if (file)
    {
      save_vars (0, 0, file, 0, format, 0);
      message (0, "save to `%s' complete", fname);
    }
  else
    warning ("unable to open `%s' for writing...", fname);
}

DEFUN_TEXT ("save", Fsave, Ssave, -1, 1,
  "save [-ascii] [-binary] [-float-binary] [-mat-binary] \n\
     [-save-builtins] file [pattern ...]\n\
\n\
save variables in a file")
{
  Octave_object retval;

  DEFINE_ARGV ("save");

  argc--;
  argv++;

  // Here is where we would get the default save format if it were
  // stored in a user preference variable.

  int save_builtins = 0;

  int save_as_floats = 0;

  load_save_format format = get_default_save_format ();

  while (argc > 0)
    {
      if (strcmp (*argv, "-ascii") == 0 || strcmp (*argv, "-a") == 0)
	{
	  format = LS_ASCII;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-binary") == 0 || strcmp (*argv, "-b") == 0)
	{
	  format = LS_BINARY;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-mat-binary") == 0 || strcmp (*argv, "-m") == 0)
	{
	  format = LS_MAT_BINARY;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-float-binary") == 0
	       || strcmp (*argv, "-f") == 0)
	{
	  format = LS_BINARY;
	  save_as_floats = 1;
	  argc--;
	  argv++;
	}
      else if (strcmp (*argv, "-save-builtins") == 0)
	{
	  save_builtins = 1;
	  argc--;
	  argv++;
	}
      else
	break;
    }

  if (argc < 1)
    {
      print_usage ("save");
      DELETE_ARGV;
      return retval;
    }

  if (save_as_floats && format == LS_ASCII)
    {
      error ("save: cannot specify both -ascii and -float-binary");
      DELETE_ARGV;
      return retval;
    }

  if (strcmp (*argv, "-") == 0)
    {
      argc--;
      argv++;

      // XXX FIXME XXX -- should things intended for the screen end up
      // in a tree_constant (string)?

      ostrstream buf;

      save_vars (argv, argc, buf, save_builtins, format,
		 save_as_floats);

      maybe_page_output (buf);
    }
  else if (argc == 1 && glob_pattern_p (*argv)) // Guard against things
    {						// like `save a*',
      print_usage ("save");			// which are probably
      DELETE_ARGV;				// mistakes...
      return retval;
    }
  else
    {
      static char *fname = 0;

      if (fname)
	free (fname);

      fname = tilde_expand (*argv);

      argc--;
      argv++;

      unsigned mode = ios::out|ios::trunc;
      if (format == LS_BINARY || format == LS_MAT_BINARY)
	mode |= ios::bin;

      ofstream file (fname, mode);

      if (file)
	{
	  save_vars (argv, argc, file, save_builtins, format,
		     save_as_floats);
	}
      else
	{
	  error ("save: couldn't open output file `%s'", *argv);
	  DELETE_ARGV;
	  return retval;
	}
    }

  DELETE_ARGV;

  return retval;
}

// Maybe this should be a static function in tree-plot.cc?

// If TC is matrix, save it on stream OS in a format useful for
// making a 3-dimensional plot with gnuplot.  If PARAMETRIC is
// nonzero, assume a parametric 3-dimensional plot will be generated.

int
save_three_d (ostream& os, const tree_constant& tc, int parametric)
{
  int fail = 0;

  int nr = tc.rows ();
  int nc = tc.columns ();

  if (tc.is_real_matrix ())
    {
      os << "# 3D data...\n"
	 << "# type: matrix\n"
	 << "# total rows: " << nr << "\n"
	 << "# total columns: " << nc << "\n";

      if (parametric)
	{
	  int extras = nc % 3;
	  if (extras)
	    warning ("ignoring last %d columns", extras);

	  Matrix tmp = tc.matrix_value ();
	  tmp = strip_infnan (tmp);
	  nr = tmp.rows ();

	  for (int i = 0; i < nc-extras; i += 3)
	    {
	      os << tmp.extract (0, i, nr-1, i+2);
	      if (i+3 < nc-extras)
		os << "\n";
	    }
	}
      else
	{
	  Matrix tmp = tc.matrix_value ();
	  tmp = strip_infnan (tmp);
	  nr = tmp.rows ();

	  for (int i = 0; i < nc; i++)
	    {
	      os << tmp.extract (0, i, nr-1, i);
	      if (i+1 < nc)
		os << "\n";
	    }
	}
    }
  else
    {
      ::error ("for now, I can only save real matrices in 3D format");
      fail = 1;
    }

  return (os && ! fail);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
