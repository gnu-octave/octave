// data-conv.cc                                           -*- C++ -*-
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

#include <iostream.h>

#include "byte-swap.h"
#include "data-conv.h"
#include "lo-error.h"

#define swap_1_bytes(x, y)

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

static void
gripe_unrecognized_float_fmt (void)
{
  (*current_liboctave_error_handler)
    ("unrecognized floating point format requested");
}

static void
gripe_data_conversion (const char *from, const char *to)
{
  (*current_liboctave_error_handler)
    ("unable to convert from %s to %s format", from, to);
}

// But first, some data conversion routines.

// Currently, we only handle conversions for the IEEE types.  To fix
// that, make more of the following routines work.

// XXX FIXME XXX -- assumes sizeof (Complex) == 8
// XXX FIXME XXX -- assumes sizeof (double) == 8
// XXX FIXME XXX -- assumes sizeof (float) == 4

static void
IEEE_big_double_to_IEEE_little_double (double *d, int len)
{
  swap_8_bytes ((char *) d, len);
}

static void
VAX_D_double_to_IEEE_little_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "IEEE little endian format");
}

static void
VAX_G_double_to_IEEE_little_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "IEEE little endian format");
}

static void
Cray_to_IEEE_little_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "IEEE little endian format");
}

static void
IEEE_big_float_to_IEEE_little_float (float *d, int len)
{
  swap_4_bytes ((char *) d, len);
}

static void
VAX_D_float_to_IEEE_little_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "IEEE little endian format");
}

static void
VAX_G_float_to_IEEE_little_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "IEEE little endian format");
}

static void
Cray_to_IEEE_little_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "IEEE little endian format");
}

static void
IEEE_little_double_to_IEEE_big_double (double *d, int len)
{
  swap_8_bytes ((char *) d, len);
}

static void
VAX_D_double_to_IEEE_big_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "IEEE big endian format");
}

static void
VAX_G_double_to_IEEE_big_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "IEEE big endian format");
}

static void
Cray_to_IEEE_big_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "IEEE big endian format");
}

static void
IEEE_little_float_to_IEEE_big_float (float *d, int len)
{
  swap_4_bytes ((char *) d, len);
}

static void
VAX_D_float_to_IEEE_big_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "IEEE big endian format");
}

static void
VAX_G_float_to_IEEE_big_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "IEEE big endian format");
}

static void
Cray_to_IEEE_big_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "IEEE big endian format");
}

static void
IEEE_little_double_to_VAX_D_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE little endian", "VAX D");
}

static void
IEEE_big_double_to_VAX_D_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE big endian", "VAX D");
}

static void
VAX_G_double_to_VAX_D_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "VAX D");
}

static void
Cray_to_VAX_D_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "VAX D");
}

static void
IEEE_little_float_to_VAX_D_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE little endian", "VAX D");
}

static void
IEEE_big_float_to_VAX_D_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE big endian", "VAX D");
}

static void
VAX_G_float_to_VAX_D_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "VAX D");
}

static void
Cray_to_VAX_D_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("Cray", "VAX D");
}

static void
IEEE_little_double_to_VAX_G_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE little endian", "VAX G");
}

static void
IEEE_big_double_to_VAX_G_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE big endian", "VAX G");
}

static void
VAX_D_double_to_VAX_G_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "VAX G");
}

static void
Cray_to_VAX_G_double (double * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "VAX G");
}

static void
IEEE_little_float_to_VAX_G_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE little endian", "VAX G");
}

static void
IEEE_big_float_to_VAX_G_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("IEEE big endian", "VAX G");
}

static void
VAX_D_float_to_VAX_G_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX D float", "VAX G");
}

static void
Cray_to_VAX_G_float (float * /* d */, int /* len */)
{
  gripe_data_conversion ("VAX G float", "VAX G");
}

void
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
      (*current_liboctave_error_handler)
	("impossible state reached in file `%s' at line %d",
	 __FILE__, __LINE__);
      break;
    }
}

void
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
      (*current_liboctave_error_handler)
	("impossible state reached in file `%s' at line %d",
	 __FILE__, __LINE__);
      break;
    }
}

void
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

void
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
      (*current_liboctave_error_handler)
	("unrecognized data format requested");
      break;
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
