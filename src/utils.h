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

#if !defined (octave_utils_h)
#define octave_utils_h 1

#include <cstdarg>

#include <iostream>
#include <string>

#include "lo-utils.h"

class octave_value;
class octave_value_list;
class string_vector;

extern std::string search_path_for_file (const std::string&, const std::string&);
extern std::string file_in_path (const std::string&, const std::string&);
extern std::string fcn_file_in_path (const std::string&);
extern std::string oct_file_in_path (const std::string&);

extern void jump_to_top_level (void) GCC_ATTR_NORETURN;

extern int almost_match (const std::string& std, const std::string& s,
			 int min_match_len = 1, int case_sens = 1);

extern int
keyword_almost_match (const char * const *std, int *min_len,
		      const std::string& s, int min_toks_to_match,
		      int max_toks);

extern int empty_arg (const char *name, int nr, int nc);

extern const char *undo_string_escape (char c);

extern std::string do_string_escapes (const std::string& s);

extern std::string undo_string_escapes (const std::string& s);

extern int check_preference (const std::string& var);

extern void
get_dimensions (const octave_value& a, const octave_value& b,
		const char *warn_for, int& nr, int& nc);

extern void
get_dimensions (const octave_value& a,
		const char *warn_for, int& nr, int& nc);

extern int
octave_format (std::ostream& os, const char *fmt, ...);

extern int
octave_vformat (std::ostream& os, const char *fmt, va_list args);

extern "C" void octave_usleep (unsigned int useconds);

extern "C" int octave_strcasecmp (const char *s1, const char *s2);

extern "C" int octave_strncasecmp (const char *s1, const char *s2, size_t n);

extern "C" char *octave_snprintf (size_t n, const char *fmt, ...);

extern "C" char *octave_vsnprintf (size_t n, const char *fmt, va_list args);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
