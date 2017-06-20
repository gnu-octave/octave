/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>

#include "symtab.h"
#include "token.h"

namespace octave
{
  token::token (int tv, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (generic_token), m_tok_info (),
      m_orig_text ()
  { }

  token::token (int tv, bool is_kw, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (is_kw ? keyword_token : generic_token),
      m_tok_info (), m_orig_text ()
  { }

  token::token (int tv, const char *s, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (string_token), m_tok_info (s),
      m_orig_text ()
  { }

  token::token (int tv, const std::string& s, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (string_token), m_tok_info (s),
      m_orig_text ()
  { }

  token::token (int tv, double d, const std::string& s, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (double_token), m_tok_info (d),
      m_orig_text (s)
  { }

  token::token (int tv, end_tok_type t, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (ettype_token), m_tok_info (t),
      m_orig_text ()
  { }

  token::token (int tv, const symbol_table::symbol_record& sr, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (sym_rec_token), m_tok_info (sr),
      m_orig_text ()
  { }

  token::token (int tv, const std::string& method_nm,
                const std::string& class_nm, int l, int c)
    : m_maybe_cmd (false), m_tspc (false), m_line_num (l), m_column_num (c),
      m_tok_val (tv), m_type_tag (scls_name_token),
      m_tok_info (method_nm, class_nm), m_orig_text ()
  { }

  token::~token (void)
  {
    if (m_type_tag == string_token)
      delete m_tok_info.m_str;

    if (m_type_tag == sym_rec_token)
      delete m_tok_info.m_sr;

    if (m_type_tag == scls_name_token)
      delete m_tok_info.m_superclass_info;
  }

  std::string
  token::text (void) const
  {
    assert (m_type_tag == string_token);
    return *m_tok_info.m_str;
  }

  std::string
  token::symbol_name (void) const
  {
    assert (m_type_tag == sym_rec_token);
    return m_tok_info.m_sr->name ();
  }

  double
  token::number (void) const
  {
    assert (m_type_tag == double_token);
    return m_tok_info.m_num;
  }

  token::token_type
  token::ttype (void) const
  {
    return m_type_tag;
  }

  token::end_tok_type
  token::ettype (void) const
  {
    assert (m_type_tag == ettype_token);
    return m_tok_info.m_et;
  }

  symbol_table::symbol_record
  token::sym_rec (void) const
  {
    assert (m_type_tag == sym_rec_token);
    return *m_tok_info.m_sr;
  }

  std::string
  token::superclass_method_name (void) const
  {
    assert (m_type_tag == scls_name_token);
    return m_tok_info.m_superclass_info->m_method_nm;
  }

  std::string
  token::superclass_class_name (void) const
  {
    assert (m_type_tag == scls_name_token);
    return m_tok_info.m_superclass_info->m_class_nm;
  }

  std::string
  token::text_rep (void) const
  {
    return m_orig_text;
  }
}
