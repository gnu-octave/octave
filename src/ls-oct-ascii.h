/*

Copyright (C) 2003 John W. Eaton

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

#if !defined (octave_ls_oct_ascii_h)
#define octave_ls_oct_ascii_h 1

#include <cfloat>

#include <string>

#include "str-vec.h"

// Flag for cell elements
#define CELL_ELT_TAG "<cell-element>"

// Used when converting Inf to something that gnuplot can read.

#ifndef OCT_RBV
#define OCT_RBV DBL_MAX / 100.0
#endif

extern std::string
extract_keyword (std::istream& is, const char *keyword, 
		 const bool next_only = false);

extern  bool
extract_keyword (std::istream& is, const char *keyword, int& value,
		 const bool next_only = false);

extern  bool
extract_keyword (std::istream& is, const string_vector& keywords,
		 std::string& keyword, int& value,
		 const bool next_only = false);

extern std::string
read_ascii_data (std::istream& is, const std::string& filename, bool& global,
		 octave_value& tc, int count);

extern bool
save_ascii_data (std::ostream& os, const octave_value& val_arg,
		 const std::string& name, bool& infnan_warned,
		 bool strip_nan_and_inf, bool mark_as_global,
		 int precision);

extern bool
save_ascii_data_for_plotting (std::ostream& os, const octave_value& t,
			      const std::string& name);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

