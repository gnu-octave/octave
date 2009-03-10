/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
              2003, 2005, 2006, 2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_utils_h)
#define octave_utils_h 1

#include <cstdarg>

#include <iosfwd>
#include <string>

#include "dMatrix.h"
#include "lo-utils.h"

class octave_value;
class octave_value_list;
class string_vector;

extern OCTINTERP_API bool valid_identifier (const char *s);
extern OCTINTERP_API bool valid_identifier (const std::string& s);

extern OCTINTERP_API bool
same_file (const std::string& f, const std::string& g);

extern OCTINTERP_API int almost_match (const std::string& std,
				       const std::string& s,
				       int min_match_len = 1,
				       int case_sens = 1);

extern OCTINTERP_API int
keyword_almost_match (const char * const *std, int *min_len,
		      const std::string& s, int min_toks_to_match,
		      int max_toks);

extern OCTINTERP_API int empty_arg (const char *name, octave_idx_type nr,
				    octave_idx_type nc);

extern OCTINTERP_API std::string
search_path_for_file (const std::string&, const string_vector&);

extern OCTINTERP_API string_vector
search_path_for_all_files (const std::string&, const string_vector&);

extern OCTINTERP_API std::string
file_in_path (const std::string&, const std::string&);

extern OCTINTERP_API std::string contents_file_in_path (const std::string&);

extern OCTINTERP_API std::string fcn_file_in_path (const std::string&);
extern OCTINTERP_API std::string oct_file_in_path (const std::string&);
extern OCTINTERP_API std::string mex_file_in_path (const std::string&);

extern OCTINTERP_API std::string do_string_escapes (const std::string& s);

extern OCTINTERP_API const char *undo_string_escape (char c);

extern OCTINTERP_API std::string undo_string_escapes (const std::string& s);

extern OCTINTERP_API void
check_dimensions (dim_vector& dim, const char *warnfor);

extern OCTINTERP_API void
get_dimensions (const octave_value& a, const char *warn_for,
                dim_vector& dim);

extern OCTINTERP_API void
get_dimensions (const octave_value& a, const octave_value& b,
		const char *warn_for, octave_idx_type& nr,
		octave_idx_type& nc);

extern OCTINTERP_API void
get_dimensions (const octave_value& a,const char *warn_for,
		octave_idx_type& nr, octave_idx_type& nc);

extern OCTINTERP_API Matrix
identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API FloatMatrix
float_identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API int
octave_format (std::ostream& os, const char *fmt, ...);

extern OCTINTERP_API int
octave_vformat (std::ostream& os, const char *fmt, va_list args);

extern OCTINTERP_API char *octave_vsnprintf (const char *fmt, va_list args);

extern OCTINTERP_API char *octave_snprintf (const char *fmt, ...);

extern OCTINTERP_API void octave_sleep (double seconds);

extern "C" OCTINTERP_API void octave_sleep (unsigned int seconds);

extern "C" OCTINTERP_API void octave_usleep (unsigned int useconds);

extern "C" OCTINTERP_API int
octave_raw_vsnprintf (char *buf, size_t n, const char *fmt, va_list args);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
