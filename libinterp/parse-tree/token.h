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

#if ! defined (octave_token_h)
#define octave_token_h 1

#include "octave-config.h"

#include <string>

#include "symtab.h"

namespace octave
{
  class
  token
  {
  public:

    enum token_type
      {
        generic_token,
        keyword_token,
        string_token,
        double_token,
        ettype_token,
        sym_rec_token,
        scls_name_token,
      };

    enum end_tok_type
      {
        simple_end,
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
        while_end,
      };

    token (int tv, int l = -1, int c = -1);
    token (int tv, bool is_keyword, int l = -1, int c = -1);
    token (int tv, const char *s, int l = -1, int c = -1);
    token (int tv, const std::string& s, int l = -1, int c = -1);
    token (int tv, double d, const std::string& s = "",
           int l = -1, int c = -1);
    token (int tv, end_tok_type t, int l = -1, int c = -1);
    token (int tv, symbol_table::symbol_record *s, int l = -1, int c = -1);
    token (int tv, const std::string& mth, const std::string& cls,
           int l = -1, int c = -1);

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

    int line (void) const { return m_line_num; }
    int column (void) const { return m_column_num; }

    bool is_keyword (void) const
    {
      return m_type_tag == keyword_token || m_type_tag == ettype_token;
    }

    bool is_symbol (void) const
    {
      return m_type_tag == sym_rec_token;
    }

    std::string text (void) const;
    std::string symbol_name (void) const;
    double number (void) const;
    token_type ttype (void) const;
    end_tok_type ettype (void) const;
    symbol_table::symbol_record *sym_rec (void) const;

    std::string superclass_method_name (void) const;
    std::string superclass_class_name (void) const;

    std::string text_rep (void) const;

  private:

    bool m_maybe_cmd;

    bool m_tspc;

    int m_line_num;

    int m_column_num;

    int m_tok_val;

    token_type m_type_tag;

    union tok_info
    {
      tok_info (void) { }

      tok_info (const char *s) : m_str (new std::string (s)) { }

      tok_info (const std::string& str) : m_str (new std::string (str)) { }

      tok_info (double num) : m_num (num) { }

      tok_info (end_tok_type et) : m_et (et) { }

      tok_info (symbol_table::symbol_record *sr) : m_sr (sr) { }

      tok_info (const std::string& method_nm, const std::string& class_nm)
        : m_superclass_info (new superclass_info (method_nm, class_nm))
      { }

      tok_info (const tok_info&) = delete;

      tok_info& operator = (const tok_info&) = delete;

      ~tok_info (void) { }

      std::string *m_str;

      double m_num;

      end_tok_type m_et;

      symbol_table::symbol_record *m_sr;

      struct superclass_info
      {
        superclass_info (void) = delete;

        superclass_info (const std::string& method_nm,
                         const std::string& class_nm)
          : m_method_nm (method_nm), m_class_nm (class_nm)
        { }

        superclass_info (const superclass_info&) = delete;

        superclass_info& operator = (const superclass_info&) = delete;

        ~superclass_info (void) = default;

        std::string m_method_nm;
        std::string m_class_nm;
      };

      superclass_info *m_superclass_info;
    };

    tok_info m_tok_info;

    std::string m_orig_text;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::token' instead")
typedef octave::token token;

#endif

#endif
