////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2024 The Octave Project Developers
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
#include <variant>

#include "comment-list.h"
#include "error.h"
#include "filepos.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class token
{
public:

  enum token_type
  {
    invalid_token,
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

public:

  token ()
    : m_type_tag (invalid_token)
  { }

  token (int id, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_leading_comments (lst)
  { }

  token (int id, bool is_kw, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (is_kw ? keyword_token : generic_token), m_leading_comments (lst)
  { }

  token (int id, const char *s, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (string_token), m_tok_info (s), m_leading_comments (lst)
  { }

  token (int id, const std::string& s, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (string_token), m_tok_info (s), m_leading_comments (lst)
  { }

  token (int id, const octave_value& val, const std::string& s, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (numeric_token), m_tok_info (val), m_orig_text (s), m_leading_comments (lst)
  { }

  token (int id, end_tok_type t, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (ettype_token), m_tok_info (t), m_leading_comments (lst)
  { }

  token (int id, const std::string& meth, const std::string& cls, const filepos& beg_pos, const filepos& end_pos, const comment_list& lst = comment_list ())
    : m_beg_pos (beg_pos), m_end_pos (end_pos), m_tok_id (id), m_type_tag (scls_name_token), m_tok_info (meth, cls), m_leading_comments (lst)
  { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (token)

  void mark_may_be_command () { m_maybe_cmd = true; }
  bool may_be_command () const { return m_maybe_cmd; }

  void mark_trailing_space () { m_tspc = true; }
  bool space_follows_token () const { return m_tspc; }

  int token_id () const { return m_tok_id; }

  bool token_is (int id) const { return m_tok_id == id; }
  bool token_is (const token *tok) const
  {
    return tok ? token_is (tok->token_id ()) : false;
  }

  filepos beg_pos () const { return m_beg_pos; }
  filepos end_pos () const { return m_end_pos; }

  void beg_pos (const filepos& pos) { m_beg_pos = pos; }
  void end_pos (const filepos& pos) { m_end_pos = pos; }

  comment_list leading_comments () const { return m_leading_comments; }
  comment_list trailing_comments () const { return m_trailing_comments; }

  void leading_comments (const comment_list& lst) { m_leading_comments = lst; }
  void trailing_comments (const comment_list& lst) { m_trailing_comments = lst; }

  // These will probably be removed.
  int line () const { return m_beg_pos.line (); }
  int column () const { return m_beg_pos.column (); }

  bool iskeyword () const
  {
    return m_type_tag == keyword_token || m_type_tag == ettype_token;
  }

  bool isstring () const { return m_type_tag == string_token; }

  std::string text () const { return m_tok_info.text (); }
  octave_value number () const { return m_tok_info.number (); }
  token_type ttype () const { return m_type_tag; }
  end_tok_type ettype () const { return m_tok_info.ettype (); }

  std::string superclass_method_name () const { return m_tok_info.superclass_method_name (); }
  std::string superclass_class_name () const { return m_tok_info.superclass_class_name (); }

  std::string text_rep () const { return m_orig_text; }

private:

  bool m_maybe_cmd {false};

  bool m_tspc {false};

  filepos m_beg_pos;
  filepos m_end_pos;

  int m_tok_id {-1};

  token_type m_type_tag {generic_token};

  class superclass_info
  {
  public:

    superclass_info (const std::string& meth, const std::string& cls)
      : m_method_name (meth), m_class_name (cls)
    { }

    OCTAVE_DEFAULT_COPY_MOVE_DELETE (superclass_info)

    std::string method_name () const { return m_method_name; }

    std::string class_name () const { return m_class_name; }

  private:

    //--------

    // The name of the method to call.  This is the text before the
    // "@" and may be of the form "object.method".
    std::string m_method_name;

    // The name of the superclass.  This is the text after the "@"
    // and may be of the form "object.method".
    std::string m_class_name;
  };

  class tok_info
  {
  public:

    tok_info (const char *s) : m_value (std::string (s)) { }

    tok_info (const std::string& str) : m_value (str) { }

    tok_info (const octave_value& num) : m_value (octave_value (num)) { }

    tok_info (end_tok_type et) : m_value (et) { }

    tok_info (const std::string& meth, const std::string& cls) : m_value (superclass_info (meth, cls)) { }

    OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (tok_info)

    std::string text () const
    {
      panic_unless (std::holds_alternative<std::string> (m_value));
      return std::get<std::string> (m_value);
    }

    octave_value number () const
    {
      panic_unless (std::holds_alternative<octave_value> (m_value));
      return std::get<octave_value> (m_value);
    }

    token::end_tok_type ettype () const
    {
      panic_unless (std::holds_alternative<end_tok_type> (m_value));
      return std::get<end_tok_type> (m_value);
    }

    std::string
    superclass_method_name () const
    {
      panic_unless (std::holds_alternative<superclass_info> (m_value));
      return std::get<superclass_info> (m_value).method_name ();
    }

    std::string
    superclass_class_name () const
    {
      panic_unless (std::holds_alternative<superclass_info> (m_value));
      return std::get<superclass_info> (m_value).class_name ();
    }

  private:

    std::variant<std::string, octave_value, end_tok_type, superclass_info> m_value;
  };

  tok_info m_tok_info;

  std::string m_orig_text;

  // Comments that appear prior to the token.
  comment_list m_leading_comments;

  // Comments that appear after the token.  This list is only used to
  // hold comments that appear after the last token in a file.
  comment_list m_trailing_comments;
};

OCTAVE_END_NAMESPACE(octave)

#endif
