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

#include <cctype>

#include <iostream.h>

#include "byte-swap.h"
#include "data-conv.h"
#include "lo-error.h"

oct_data_conv::data_type
oct_data_conv::string_to_data_type (const string& str)
{
  data_type retval = dt_unknown;

    // XXX FIXME XXX -- finish implementing this.

  // XXX FIXME XXX -- before checking s, need to strip spaces and downcase.

  int n = str.length ();

  int k = 0;

  string s (n, ' ');

  for (int i = 0; i < n; i++)
    if (! isspace (str[i]))
      s[k++] = tolower (str[i]);

  s.resize (k);

  if (s == "char" || s == "char*1" || s == "integer*1" || s == "int8")
    retval = dt_char;
  else if (s == "schar" || s == "signedchar")
    retval = dt_schar;
  else if (s == "uchar" || s == "unsignedchar")
    retval = dt_uchar;
  else if (s == "short")
    retval = dt_short;
  else if (s == "ushort" || s == "unsignedshort")
    retval = dt_ushort;
  else if (s == "int")
    retval = dt_int;
  else if (s == "uint" || s == "unsignedint")
    retval = dt_uint;
  else if (s == "long")
    retval = dt_long;
  else if (s == "ulong" || s == "unsignedlong")
    retval = dt_ulong;
  else if (s == "float" || s == "float32" || s == "real*4")
    retval = dt_float;
  else if (s == "double" || s == "float64" || s == "real*8")
    retval = dt_double;
  else if (s == "int16" || s == "integer*2")
    {
      if (sizeof (short) == 2)
	retval = dt_short;
      else if (sizeof (int) == 2)
	retval = dt_int;
      else
	(*current_liboctave_error_handler)
	  ("unable to find matching native data type for %s", s.c_str ());
    }
  else if (s == "int32" || s == "integer*4")
    {
      if (sizeof (int) == 4)
	retval = dt_int;
      else if (sizeof (long) == 4)
	retval = dt_long;
      else
	(*current_liboctave_error_handler)
	  ("unable to find matching native data type for %s", s.c_str ());
    }
  else
    (*current_liboctave_error_handler) ("invalid data type specified");

  return retval;
}

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
			     oct_mach_info::float_format fmt)
{
  switch (oct_mach_info::native_float_format ())
    {
    case oct_mach_info::ieee_little_endian:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_double_to_IEEE_little_double (data, len);
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_double_to_IEEE_little_double (data, len);
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_double_to_IEEE_little_double (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_IEEE_little_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::ieee_big_endian:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_double_to_IEEE_big_double (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_double_to_IEEE_big_double (data, len);
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_double_to_IEEE_big_double (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_IEEE_big_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::vax_d:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_double_to_VAX_D_double (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_double_to_VAX_D_double (data, len);
	  break;

	case oct_mach_info::vax_d:
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_double_to_VAX_D_double (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_VAX_D_double (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::vax_g:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_double_to_VAX_G_double (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_double_to_VAX_G_double (data, len);
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_double_to_VAX_G_double (data, len);
	  break;

	case oct_mach_info::vax_g:
	  break;

	case oct_mach_info::cray:
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
			    oct_mach_info::float_format fmt)
{
  switch (oct_mach_info::native_float_format ())
    {
    case oct_mach_info::ieee_little_endian:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_float_to_IEEE_little_float (data, len);
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_float_to_IEEE_little_float (data, len);
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_float_to_IEEE_little_float (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_IEEE_little_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::ieee_big_endian:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_float_to_IEEE_big_float (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_float_to_IEEE_big_float (data, len);
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_float_to_IEEE_big_float (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_IEEE_big_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::vax_d:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_float_to_VAX_D_float (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_float_to_VAX_D_float (data, len);
	  break;

	case oct_mach_info::vax_d:
	  break;

	case oct_mach_info::vax_g:
	  VAX_G_float_to_VAX_D_float (data, len);
	  break;

	case oct_mach_info::cray:
	  Cray_to_VAX_D_float (data, len);
	  break;

	default:
	  gripe_unrecognized_float_fmt ();
	  break;
	}
      break;

    case oct_mach_info::vax_g:
      switch (fmt)
	{
	case oct_mach_info::ieee_little_endian:
	  IEEE_little_float_to_VAX_G_float (data, len);
	  break;

	case oct_mach_info::ieee_big_endian:
	  IEEE_big_float_to_VAX_G_float (data, len);
	  break;

	case oct_mach_info::vax_d:
	  VAX_D_float_to_VAX_G_float (data, len);
	  break;

	case oct_mach_info::vax_g:
	  break;

	case oct_mach_info::cray:
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
	      int swap, oct_mach_info::float_format fmt)
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
;;; End: ***
*/
