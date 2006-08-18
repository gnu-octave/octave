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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_load_save_h)
#define octave_load_save_h 1

#include <iostream>

#include <string>

class octave_value;

enum load_save_format
  {
    LS_ASCII,
    LS_BINARY,
    LS_MAT_ASCII,
    LS_MAT_ASCII_LONG,
    LS_MAT_BINARY,
    LS_MAT5_BINARY,
    LS_MAT7_BINARY,
#ifdef HAVE_HDF5
    LS_HDF5,
#endif /* HAVE_HDF5 */
    LS_UNKNOWN
  };

extern bool
save_ascii_data_for_plotting (std::ostream& os, const octave_value& t,
			      const std::string& name = std::string ());

extern bool
save_three_d (std::ostream& os, const octave_value& t,
	      bool parametric = false);

extern void dump_octave_core (void);

extern int
read_binary_file_header (std::istream& is, bool& swap,
			 oct_mach_info::float_format& flt_fmt,
			 bool quiet = false);

extern octave_value
do_load (std::istream& stream, const std::string& orig_fname, bool force,
	 load_save_format format, oct_mach_info::float_format flt_fmt,
	 bool list_only, bool swap, bool verbose,
	 const string_vector& argv, int argv_idx, int argc, int nargout);

extern void
do_save (std::ostream& os, symbol_record *sr, load_save_format fmt,
	 bool save_as_floats, bool& infnan_warned);

extern void
write_header (std::ostream& os, load_save_format format);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
