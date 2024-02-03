////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2000-2024 The Octave Project Developers
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

#if ! defined (octave_comment_list_h)
#define octave_comment_list_h 1

#include "octave-config.h"

#include <string>

#include "base-list.h"

OCTAVE_BEGIN_NAMESPACE(octave)

extern std::string get_comment_text ();

extern char * get_comment_text_c_str ();

extern void save_comment_text (const std::string& text);

class comment_elt
{
public:

  enum comment_type
  {
    unknown,
    block,
    full_line,
    end_of_line,
    doc_string,
    copyright
  };

  comment_elt (const std::string& s = "", comment_type t = unknown, bool uses_hash_char = false)
    : m_text (s), m_type (t), m_uses_hash_char (uses_hash_char) { }

  comment_elt (const comment_elt& oc)
    : m_text (oc.m_text), m_type (oc.m_type), m_uses_hash_char (oc.m_uses_hash_char)
  { }

  comment_elt& operator = (const comment_elt& oc)
  {
    if (this != &oc)
      {
        m_text = oc.m_text;
        m_type = oc.m_type;
        m_uses_hash_char = oc.m_uses_hash_char;
      }

    return *this;
  }

  bool empty () const { return m_text.empty (); }

  std::string text () const { return m_text; }

  comment_type type () const { return m_type; }

  bool is_block () const { return m_type == block; }
  bool is_full_line () const { return m_type == full_line; }
  bool is_end_of_line () const { return m_type == end_of_line; }
  bool is_doc_string () const { return m_type == doc_string; }
  bool is_copyright () const { return m_type == copyright; }
  bool uses_hash_char () const { return m_uses_hash_char; }

  void reset ()
  {
    m_text = "";
    m_type = unknown;
    m_uses_hash_char = false;
  }

  ~comment_elt () = default;

private:

  // The text of the comment.
  std::string m_text;

  // The type of comment.
  comment_type m_type;

  // TRUE means a line comment uses '#' or a block comment used at least
  // one '#' delimiter.
  bool m_uses_hash_char;
};

class comment_list : public base_list<comment_elt>
{
public:

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (comment_list)

  void append (const comment_elt& elt)
  { base_list<comment_elt>::append (elt); }

  void append (const std::string& s,
               comment_elt::comment_type t = comment_elt::unknown,
               bool uses_hash_char = false)
  { append (comment_elt (s, t, uses_hash_char)); }

  comment_list * dup () const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
