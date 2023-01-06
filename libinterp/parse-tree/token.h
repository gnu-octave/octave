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

#if ! defined (octave_token_h)
#define octave_token_h 1

#include "octave-config.h"

#include <string>

#include "filepos.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class token
{
public:

  enum token_type
  {
    generic_token,
    keyword_token,
    string_token,
    numeric_token,
    ettype_token,
    scls_name_token,
  };

  enum end_tok_type
  {
    simple_end,
    arguments_end,
    classdef_end,
    enumeration_end,
    events_end,
    for_end,
    function_end,
    if_end,
    methods_end,
    parfor_end,
    properties_end,
    switch_end,
    try_catch_end,
    unwind_protect_end,
    spmd_end,
    while_end,
  };

  token (int tv, const filepos& beg_pos, const filepos& end_pos);

  token (int tv, bool is_keyword, const filepos& beg_pos
         , const filepos& end_pos);

  token (int tv, const char *s, const filepos& beg_pos,
         const filepos& end_pos);

  token (int tv, const std::string& s, const filepos& beg_pos,
         const filepos& end_pos);

  token (int tv, const octave_value& val, const std::string& s,
         const filepos& beg_pos, const filepos& end_pos);

  token (int tv, end_tok_type t, const filepos& beg_pos,
         const filepos& end_pos);

  token (int tv, const std::string& mth, const std::string& cls,
         const filepos& beg_pos, const filepos& end_pos);

  // No copying!

  token (const token&) = delete;

  token& operator = (const token&) = delete;

  ~token (void);

  void mark_may_be_command (void) { m_maybe_cmd = true; }
  bool may_be_command (void) const { return m_maybe_cmd; }

  void mark_trailing_space (void) { m_tspc = true; }
  bool space_follows_token (void) const { return m_tspc; }

  int token_value (void) const { return m_tok_val; }
  bool token_value_is (int tv) const { return tv == m_tok_val; }

  filepos beg_pos (void) const { return m_beg_pos; }
  filepos end_pos (void) const { return m_end_pos; }

  void beg_pos (const filepos& pos) { m_beg_pos = pos; }
  void end_pos (const filepos& pos) { m_end_pos = pos; }

  // These will probably be removed.
  int line (void) const { return m_beg_pos.line (); }
  int column (void) const { return m_beg_pos.column (); }

  bool iskeyword (void) const
  {
    return m_type_tag == keyword_token || m_type_tag == ettype_token;
  }

  bool isstring (void) const { return m_type_tag == string_token; }

  std::string text (void) const;
  octave_value number (void) const;
  token_type ttype (void) const;
  end_tok_type ettype (void) const;

  std::string superclass_method_name (void) const;
  std::string superclass_class_name (void) const;

  std::string text_rep (void) const;

private:

  bool m_maybe_cmd;

  bool m_tspc;

  filepos m_beg_pos;
  filepos m_end_pos;

  int m_tok_val;

  token_type m_type_tag;

  union tok_info
  {
    tok_info (void) { }

    tok_info (const char *s) : m_str (new std::string (s)) { }

    tok_info (const std::string& str) : m_str (new std::string (str)) { }

    tok_info (const octave_value& num) : m_num (new octave_value (num)) { }

    tok_info (end_tok_type et) : m_et (et) { }

    tok_info (const std::string& meth, const std::string& cls)
      : m_superclass_info (new superclass_info (meth, cls))
    { }

    tok_info (const tok_info&) = delete;

    tok_info& operator = (const tok_info&) = delete;

    ~tok_info (void) { }

    std::string *m_str;

    octave_value *m_num;

    end_tok_type m_et;

    struct superclass_info
    {
    public:
      superclass_info (const std::string& meth, const std::string& cls)
        : m_method_name (meth), m_class_name (cls)
      { }

      superclass_info (void) = delete;

      superclass_info (const superclass_info&) = delete;

      superclass_info& operator = (const superclass_info&) = delete;

      ~superclass_info (void) = default;

      //--------

      // The name of the method to call.  This is the text before the
      // "@" and may be of the form "object.method".
      std::string m_method_name;

      // The name of the superclass.  This is the text after the "@"
      // and may be of the form "object.method".
      std::string m_class_name;
    };

    superclass_info *m_superclass_info;
  };

  tok_info m_tok_info;

  std::string m_orig_text;
};

OCTAVE_END_NAMESPACE(octave)

#endif
