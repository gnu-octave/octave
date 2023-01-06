////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_utils_h)
#define octave_utils_h 1

#include "octave-config.h"

#include <cstdarg>

#include <iosfwd>
#include <string>
#include <list>

#include "dMatrix.h"
#include "lo-utils.h"

class octave_value;
class octave_value_list;
class string_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API bool valid_identifier (const char *s);
extern OCTINTERP_API bool valid_identifier (const std::string& s);

//! Helper class for `make_valid_name` function calls.
//!
//! Extracting options separately for multiple (e.g. 1000+) function calls
//! avoids expensive repetitive parsing of the very same options.

class
OCTINTERP_API
make_valid_name_options
{
public:

  //! Default options for `make_valid_name` function calls.
  //!
  //! Calling the constructor without arguments is equivalent to:
  //!
  //! @code{.cc}
  //! make_valid_name_options (ovl ("ReplacementStyle", "underscore",
  //!                               "Prefix", "x"));
  //! @endcode

  make_valid_name_options () = default;

  //! Extract attribute-value-pairs from an octave_value_list of strings.
  //!
  //! If attributes occur multiple times, the rightmost pair is chosen.
  //!
  //! @code{.cc}
  //! make_valid_name_options (ovl ("ReplacementStyle", "hex", ...));
  //! @endcode

  make_valid_name_options (const octave_value_list& args);

  //! @return ReplacementStyle, see `help matlab.lang.makeValidName`.

  const std::string&
  get_replacement_style () const { return m_replacement_style; }

  //! @return Prefix, see `help matlab.lang.makeValidName`.

  const std::string& get_prefix () const { return m_prefix; }

private:

  std::string m_replacement_style{"underscore"};
  std::string m_prefix{"x"};
};

//! Modify @p str to be a valid variable name.
//!
//! @param str input string
//! @param options see also `help matlab.lang.makeValidName`.
//!
//! @return true, if @p str was modified.

extern OCTINTERP_API bool
make_valid_name (std::string& str, const make_valid_name_options& options);

extern OCTINTERP_API bool
same_file (const std::string& f, const std::string& g);

extern OCTINTERP_API int almost_match (const std::string& std,
                                       const std::string& s,
                                       int min_match_len = 1,
                                       int case_sens = 1);

extern OCTINTERP_API int
keyword_almost_match (const char *const *std, int *min_len,
                      const std::string& s, int min_toks_to_match,
                      int max_toks);

extern OCTINTERP_API std::string
search_path_for_file (const std::string&, const string_vector&);

extern OCTINTERP_API string_vector
search_path_for_all_files (const std::string&, const string_vector&);

extern OCTINTERP_API std::string
file_in_path (const std::string&, const std::string&);

extern OCTINTERP_API std::string
find_data_file_in_load_path  (const std::string& fcn,
                              const std::string& file,
                              bool require_regular_file = false);

extern OCTINTERP_API std::string contents_file_in_path (const std::string&);

extern OCTINTERP_API std::string fcn_file_in_path (const std::string&);

extern OCTINTERP_API std::string do_string_escapes (const std::string& s);

extern OCTINTERP_API const char * undo_string_escape (char c);

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
get_dimensions (const octave_value& a, const char *warn_for,
                octave_idx_type& nr, octave_idx_type& nc);

extern OCTINTERP_API octave_idx_type
dims_to_numel (const dim_vector& dims, const octave_value_list& idx);

extern OCTINTERP_API Matrix
identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API FloatMatrix
float_identity_matrix (octave_idx_type nr, octave_idx_type nc);

extern OCTINTERP_API std::size_t
format (std::ostream& os, const char *fmt, ...);

extern OCTINTERP_API std::size_t
vformat (std::ostream& os, const char *fmt, va_list args);

extern OCTINTERP_API std::string
vasprintf (const char *fmt, va_list args);

extern OCTINTERP_API std::string asprintf (const char *fmt, ...);

extern OCTINTERP_API void sleep (double seconds,
                                 bool do_graphics_events = false);

extern OCTINTERP_API
octave_value_list
do_simple_cellfun (octave_value_list (*fcn) (const octave_value_list&, int),
                   const char *fcn_name, const octave_value_list& args,
                   int nargout);

extern OCTINTERP_API
octave_value
do_simple_cellfun (octave_value_list (*fcn) (const octave_value_list&, int),
                   const char *fcn_name, const octave_value_list& args);

OCTAVE_END_NAMESPACE(octave)

#endif
