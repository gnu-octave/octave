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

class istream;
class ostrstream;

#include <string>

#include "lo-utils.h"

class octave_value_list;
class string_vector;

extern string search_path_for_file (const string&, const string&);
extern string file_in_path (const string&, const string&);
extern string fcn_file_in_path (const string&);
extern string oct_file_in_path (const string&);

extern "C" void jump_to_top_level (void) GCC_ATTR_NORETURN;

extern int almost_match (const string& std, const string& s,
			 int min_match_len = 1, int case_sens = 1);

extern int
keyword_almost_match (const char * const *std, int *min_len,
		      const string& s, int min_toks_to_match,
		      int max_toks);

extern int empty_arg (const char *name, int nr, int nc);

extern const char *undo_string_escape (char c);

extern string do_string_escapes (const string& s);

extern string undo_string_escapes (const string& s);

extern int check_preference (const string& var);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
