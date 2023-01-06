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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>

#include "error.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

token::token (int tv, const filepos& beg_pos, const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (generic_token),
    m_tok_info (), m_orig_text ()
{ }

token::token (int tv, bool is_kw, const filepos& beg_pos,
              const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv),
    m_type_tag (is_kw ? keyword_token : generic_token), m_tok_info (),
    m_orig_text ()
{ }

token::token (int tv, const char *s, const filepos& beg_pos,
              const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (string_token),
    m_tok_info (s), m_orig_text ()
{ }

token::token (int tv, const std::string& s, const filepos& beg_pos,
              const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (string_token),
    m_tok_info (s), m_orig_text ()
{ }

token::token (int tv, const octave_value& val, const std::string& s,
              const filepos& beg_pos, const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (numeric_token),
    m_tok_info (val), m_orig_text (s)
{ }

token::token (int tv, end_tok_type t, const filepos& beg_pos,
              const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (ettype_token),
    m_tok_info (t), m_orig_text ()
{ }

token::token (int tv, const std::string& meth, const std::string& cls,
              const filepos& beg_pos, const filepos& end_pos)
  : m_maybe_cmd (false), m_tspc (false), m_beg_pos (beg_pos),
    m_end_pos (end_pos), m_tok_val (tv), m_type_tag (scls_name_token),
    m_tok_info (meth, cls), m_orig_text ()
{ }

token::~token (void)
{
  if (m_type_tag == string_token)
    delete m_tok_info.m_str;
  else if (m_type_tag == numeric_token)
    delete m_tok_info.m_num;
  else if (m_type_tag == scls_name_token)
    delete m_tok_info.m_superclass_info;
}

std::string
token::text (void) const
{
  panic_if (m_type_tag != string_token);
  return *m_tok_info.m_str;
}

octave_value
token::number (void) const
{
  panic_if (m_type_tag != numeric_token);
  return *m_tok_info.m_num;
}

token::token_type
token::ttype (void) const
{
  return m_type_tag;
}

token::end_tok_type
token::ettype (void) const
{
  panic_if (m_type_tag != ettype_token);
  return m_tok_info.m_et;
}

std::string
token::superclass_method_name (void) const
{
  panic_if (m_type_tag != scls_name_token);
  return m_tok_info.m_superclass_info->m_method_name;
}

std::string
token::superclass_class_name (void) const
{
  panic_if (m_type_tag != scls_name_token);
  return m_tok_info.m_superclass_info->m_class_name;
}

std::string
token::text_rep (void) const
{
  return m_orig_text;
}

OCTAVE_END_NAMESPACE(octave)
