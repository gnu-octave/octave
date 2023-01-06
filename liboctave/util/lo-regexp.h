////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#if ! defined (octave_lo_regexp_h)
#define octave_lo_regexp_h 1

#include "octave-config.h"

#include <list>
#include <sstream>
#include <string>

#include "Array.h"
#include "Matrix.h"
#include "base-list.h"
#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
OCTAVE_API
regexp
{
public:

  class opts;
  class match_data;

  regexp (const std::string& pat = "",
          const regexp::opts& opt = regexp::opts (),
          const std::string& w = "regexp")
    : m_pattern (pat), m_options (opt), m_code (nullptr), m_named_pats (),
      m_names (0), m_named_idx (), m_who (w)
  {
    compile_internal ();
  }

  regexp (const regexp&) = default;

  regexp& operator = (const regexp& rx) = default;

  ~regexp (void) { free (); }

  void compile (const std::string& pat,
                const regexp::opts& opt = regexp::opts ())
  {
    m_pattern = pat;
    m_options = opt;
    compile_internal ();
  }

  match_data match (const std::string& buffer) const;

  bool is_match (const std::string& buffer) const;

  Array<bool> is_match (const string_vector& buffer) const;

  std::string replace (const std::string& buffer,
                       const std::string& replacement) const;

  static match_data
  match (const std::string& pat, const std::string& buffer,
         const regexp::opts& opt = regexp::opts (),
         const std::string& who = "regexp")
  {
    regexp rx (pat, opt, who);

    return rx.match (buffer);
  }

  static bool
  is_match (const std::string& pat, const std::string& buffer,
            const regexp::opts& opt = regexp::opts (),
            const std::string& who = "regexp")
  {
    regexp rx (pat, opt, who);

    return rx.is_match (buffer);
  }

  static Array<bool>
  is_match (const std::string& pat, const string_vector& buffer,
            const regexp::opts& opt = regexp::opts (),
            const std::string& who = "regexp")
  {
    regexp rx (pat, opt, who);

    return rx.is_match (buffer);
  }

  static std::string
  replace (const std::string& pat, const std::string& buffer,
           const std::string& replacement,
           const regexp::opts& opt = regexp::opts (),
           const std::string& who = "regexp")
  {
    regexp rx (pat, opt, who);

    return rx.replace (buffer, replacement);
  }

  class opts
  {
  public:

    opts (void)
      : m_case_insensitive (false), m_dotexceptnewline (false),
        m_emptymatch (false), m_freespacing (false), m_lineanchors (false),
        m_once (false) { }

    opts (const opts&) = default;

    opts& operator = (const opts&) = default;

    ~opts (void) = default;

    void case_insensitive (bool val) { m_case_insensitive = val; }
    void dotexceptnewline (bool val) { m_dotexceptnewline = val; }
    void emptymatch (bool val) { m_emptymatch = val; }
    void freespacing (bool val) { m_freespacing = val; }
    void lineanchors (bool val) { m_lineanchors = val; }
    void once (bool val) { m_once = val; }

    bool case_insensitive (void) const { return m_case_insensitive; }
    bool dotexceptnewline (void) const { return m_dotexceptnewline; }
    bool emptymatch (void) const { return m_emptymatch; }
    bool freespacing (void) const { return m_freespacing; }
    bool lineanchors (void) const { return m_lineanchors; }
    bool once (void) const { return m_once; }

  private:

    bool m_case_insensitive;
    bool m_dotexceptnewline;
    bool m_emptymatch;
    bool m_freespacing;
    bool m_lineanchors;
    bool m_once;
  };

  class match_element
  {
  public:

    match_element (const string_vector& nt, const string_vector& t,
                   const std::string& ms, const Matrix& te,
                   double s, double e)
      : m_match_string (ms), m_named_tokens (nt), m_tokens (t),
        m_token_extents (te), m_start (s), m_end (e)
    { }

    match_element (const match_element&) = default;

    match_element& operator = (const match_element&) = default;

    std::string match_string (void) const { return m_match_string; }
    string_vector named_tokens (void) const { return m_named_tokens; }
    string_vector tokens (void) const { return m_tokens; }
    Matrix token_extents (void) const { return m_token_extents; }
    double start (void) const { return m_start; }
    double end (void) const { return m_end; }

  private:

    std::string m_match_string;
    string_vector m_named_tokens;
    string_vector m_tokens;
    Matrix m_token_extents;

    // FIXME: Are these values declared as double because that's what
    // Octave interpreter functions will store?  Should they be int or
    // size_t instead?
    double m_start;
    double m_end;
  };

  class match_data : public base_list<match_element>
  {
  public:

    match_data (void)
      : base_list<match_element> (), m_named_pats ()
    { }

    match_data (const std::list<match_element>& l, const string_vector& np)
      : base_list<match_element> (l), m_named_pats (np)
    { }

    match_data (const match_data&) = default;

    match_data& operator = (const match_data&) = default;

    ~match_data (void) = default;

    string_vector named_patterns (void) const { return m_named_pats; }

  private:

    string_vector m_named_pats;
  };

private:

  // The pattern we've been asked to match.
  std::string m_pattern;

  opts m_options;

  // Internal data describing the regular expression.
  void *m_code;

  string_vector m_named_pats;
  int m_names;
  Array<int> m_named_idx;
  std::string m_who;

  void free (void);

  void compile_internal (void);
};

OCTAVE_END_NAMESPACE(octave)

#endif
