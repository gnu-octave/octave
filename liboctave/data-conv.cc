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

#include <cctype>

#include <iostream>

#include "byte-swap.h"
#include "data-conv.h"
#include "lo-error.h"

#define FIND_SIZED_INT_TYPE(VAL, BITS, TQ, Q) \
  do \
    { \
      int sz = BITS / CHAR_BIT; \
      if (sizeof (TQ char) == sz) \
	VAL = oct_data_conv::dt_ ## Q ## char; \
      else if (sizeof (TQ short) == sz) \
	VAL = oct_data_conv::dt_ ## Q ## short; \
      else if (sizeof (TQ int) == sz) \
	VAL = oct_data_conv::dt_ ## Q ## int; \
      else if (sizeof (TQ long) == sz) \
	VAL = oct_data_conv::dt_ ## Q ## long; \
      else \
        VAL = oct_data_conv::dt_unknown; \
    } \
  while (0)

#define FIND_SIZED_FLOAT_TYPE(VAL, BITS) \
  do \
    { \
      int sz = BITS / CHAR_BIT; \
      if (sizeof (float) == sz) \
	VAL = oct_data_conv::dt_float; \
      else if (sizeof (double) == sz) \
	VAL = oct_data_conv::dt_double; \
      else \
        VAL = oct_data_conv::dt_unknown; \
    } \
  while (0)

// I'm not sure it is worth the trouble, but let's use a lookup table
// for the types that are supposed to be a specific number of bits
// wide.  Given the macros above, this should work as long as CHAR_BIT
// is a multiple of 8 and there are types with the right sizes.
//
// The sized data type lookup table has the following format:
//
//                            bits
//                    +----+----+----+----+
//                    |  8 | 16 | 32 | 64 |
//                    +----+----+----+----+
//     signed integer |    |    |    |    |
//                    +----+----+----+----+
//   unsigned integer |    |    |    |    |
//                    +----+----+----+----+
//     floating point |    |    |    |    |
//                    +----+----+----+----+
//
// So, the 0,3 element is supposed to contain the oct_data_conv enum
// value corresponding to the correct native data type for a signed
// 32-bit integer.

static void
init_sized_type_lookup_table (oct_data_conv::data_type table[3][4])
{
  int bits = 8;

  for (int i = 0; i < 4; i++)
    {
      FIND_SIZED_INT_TYPE (table[0][i], bits, , );

      FIND_SIZED_INT_TYPE (table[1][i], bits, unsigned, u);

      FIND_SIZED_FLOAT_TYPE (table[2][i], bits);

      bits *= 2;
    }
}

oct_data_conv::data_type
oct_data_conv::string_to_data_type (const std::string& str)
{
  data_type retval = dt_unknown;

  static bool initialized = false;

  static data_type sized_type_table[3][4];

  if (! initialized)
    {
      init_sized_type_lookup_table (sized_type_table);

      initialized = true;
    }

  // XXX FIXME XXX -- finish implementing this.

  int n = str.length ();

  int k = 0;

  std::string s (n, ' ');

  for (int i = 0; i < n; i++)
    if (! isspace (str[i]))
      s[k++] = tolower (str[i]);

  s.resize (k);

  if (s == "char")
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
  else if (s == "float")
    retval = dt_float;
  else if (s == "double")
    retval = dt_double;
  else if (s == "int8" || s == "char*1" || s == "integer*1")
    retval = sized_type_table[0][0];
  else if (s == "int16" || s == "integer*2")
    retval = sized_type_table[0][1];
  else if (s == "int32" || s == "integer*4")
    retval = sized_type_table[0][2];
  else if (s == "int64" || s == "integer*8")
    retval = sized_type_table[0][3];
  else if (s == "uint8")
    retval = sized_type_table[1][0];
  else if (s == "uint16")
    retval = sized_type_table[1][1];
  else if (s == "uint32")
    retval = sized_type_table[1][2];
  else if (s == "uint64")
    retval = sized_type_table[1][3];
  else if (s == "float32" || s == "real*4")
    retval = sized_type_table[2][2];
  else if (s == "float64" || s == "real*8")
    retval = sized_type_table[2][3];
  else
    (*current_liboctave_error_handler) ("invalid data type specified");

  if (retval == dt_unknown)
    (*current_liboctave_error_handler)
      ("unable to find matching native data type for %s", s.c_str ());

  return retval;
}

#define swap_1_bytes(x, y)

#define LS_DO_READ(TYPE, swap, data, size, len, stream) \
  do \
    { \
      if (len > 0) \
	{ \
	  volatile TYPE *ptr = X_CAST (volatile TYPE *, data); \
	  stream.read (X_CAST (char *, ptr), size * len); \
	  if (swap) \
	    swap_ ## size ## _bytes (ptr, len); \
	  TYPE tmp = ptr[0]; \
	  for (int i = len - 1; i > 0; i--) \
	    data[i] = ptr[i]; \
	  data[0] = tmp; \
	} \
    } \
  while (0)

// Have to use copy here to avoid writing over data accessed via
// Matrix::data().

#define LS_DO_WRITE(TYPE, data, size, len, stream) \
  do \
    { \
      if (len > 0) \
	{ \
	  char tmp_type = static_cast<char> (type); \
	  stream.write (&tmp_type, 1); \
	  TYPE *ptr = new TYPE [len]; \
	  for (int i = 0; i < len; i++) \
	    ptr[i] = X_CAST (TYPE, data[i]); \
	  stream.write (X_CAST (char *, ptr), size * len); \
	  delete [] ptr ; \
	} \
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
  swap_8_bytes (d, len);
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
  swap_4_bytes (d, len);
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
  swap_8_bytes (d, len);
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
  swap_4_bytes (d, len);
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
read_doubles (std::istream& is, double *data, save_type type, int len,
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
	volatile float *ptr = X_CAST (float *, data);
	is.read (X_CAST (char *, data), 4 * len);
	do_float_format_conversion (X_CAST (float *, data), len, fmt);
	float tmp = ptr[0];
	for (int i = len - 1; i > 0; i--)
	  data[i] = ptr[i];
	data[0] = tmp;
      }
      break;

    case LS_DOUBLE: // No conversion necessary.
      is.read (X_CAST (char *, data), 8 * len);
      do_double_format_conversion (data, len, fmt);
      break;

    default:
      is.clear (std::ios::failbit|is.rdstate ());
      break;
    }
}

void
write_doubles (std::ostream& os, const double *data, save_type type, int len)
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

    case LS_DOUBLE: // No conversion necessary.
      {
	char tmp_type = X_CAST (char, type);
	os.write (&tmp_type, 1);
	os.write (X_CAST (char *, data), 8 * len);
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
