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

#if !defined (octave_data_conv_h)
#define octave_data_conv_h 1

#include <climits>

#include "float-fmt.h"

// Not all of the following are currently used.

#if CHAR_BIT != 8
#error "CHAR_BIT is not 8!"
#endif

#if SIZEOF_SHORT == 2
#define TWO_BYTE_INT short
#elif SIZEOF_INT == 2
#define TWO_BYTE_INT int
#else
#error "No 2 byte integer type found!"
#endif

#if SIZEOF_INT == 4
#define FOUR_BYTE_INT int
#elif SIZEOF_LONG == 4
#define FOUR_BYTE_INT long
#else
#error "No 4 byte integer type found!"
#endif

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

extern void
do_double_format_conversion (double *data, int len, floating_point_format fmt);

extern void
do_float_format_conversion (float *data, int len, floating_point_format fmt);

extern void
read_doubles (istream& is, double *data, save_type type, int len,
	      int swap, floating_point_format fmt);
extern void
write_doubles (ostream& os, const double *data, save_type type, int len);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
