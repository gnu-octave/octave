/*

Copyright (C) 2019 The Octave Project Developers

See the file COPYRIGHT.md in the top-level directory of this distribution
or <https://octave.org/COPYRIGHT.html/>.


This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_syminfo_accumulator_h)
#define octave_syminfo_accumulator_h 1

#include "octave-config.h"

#include <string>

#include "oct-map.h"
#include "ov.h"
#include "stack-frame.h"
#include "syminfo.h"
#include "symrec.h"

namespace octave
{
  class symbol_info_accumulator : public stack_frame_walker
  {
  public:

    symbol_info_accumulator (const std::string& pattern,
                             bool have_regexp = false)
      : stack_frame_walker (), m_patterns (pattern), m_match_all (false),
        m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
        m_found_names ()
    { }

    symbol_info_accumulator (const string_vector& patterns,
                             bool have_regexp = false)
      : stack_frame_walker (), m_patterns (patterns), m_match_all (false),
        m_first_only (false), m_have_regexp (have_regexp), m_sym_inf_list (),
        m_found_names ()
    { }

    symbol_info_accumulator (bool match_all = true, bool first_only = true)
      : stack_frame_walker (), m_patterns (), m_match_all (match_all),
        m_first_only (first_only), m_have_regexp (false),
        m_sym_inf_list (), m_found_names ()
    { }

    symbol_info_accumulator (const symbol_info_accumulator&) = delete;

    symbol_info_accumulator& operator = (const symbol_info_accumulator&) = delete;

    ~symbol_info_accumulator (void) = default;

    bool is_empty  (void) const
    {
      for (const auto& nm_sil : m_sym_inf_list)
        {
          const symbol_info_list& lst = nm_sil.second;

          if (! lst.empty ())
            return false;
        }

      return true;
    }

    std::list<std::string> names (void) const
    {
      std::list<std::string> retval;

      for (const auto& nm_sil : m_sym_inf_list)
        {
          const symbol_info_list& lst = nm_sil.second;

          std::list<std::string> nm_list = lst.names ();

          for (const auto& nm : nm_list)
            retval.push_back (nm);
        }

      return retval;
    }

    symbol_info_list symbol_info (void) const
    {
      symbol_info_list retval;

      for (const auto& nm_sil : m_sym_inf_list)
        {
          const symbol_info_list& lst = nm_sil.second;

          for (const auto& syminf : lst)
            retval.push_back (syminf);
        }

      return retval;
    }

    octave_map map_value (void) const
    {
      octave_map retval;

      // FIXME: is there a better way to concatenate structures?

      size_t n_frames = m_sym_inf_list.size ();

      OCTAVE_LOCAL_BUFFER (octave_map, map_list, n_frames);

      size_t j = 0;
      for (const auto& nm_sil : m_sym_inf_list)
        {
          std::string scope_name = nm_sil.first;
          const symbol_info_list& lst = nm_sil.second;

          map_list[j] = lst.map_value (scope_name, n_frames-j);

          j++;
        }

      return octave_map::cat (-1, n_frames, map_list);
    }

    void display (std::ostream& os, const std::string& format) const
    {
      for (const auto& nm_sil : m_sym_inf_list)
        {
          os << "\nvariables in scope: " << nm_sil.first << "\n\n";

          const symbol_info_list& lst = nm_sil.second;

          lst.display (os, format);
        }
    }

    void visit_compiled_fcn_stack_frame (compiled_fcn_stack_frame& frame)
    {
      // This one follows static link always.  Hmm, should the access
      // link for a compiled_fcn_stack_frame be the same as the static
      // link?

      stack_frame *slink = frame.static_link ();

      if (slink)
        slink->accept (*this);
    }

    void visit_script_stack_frame (script_stack_frame& frame)
    {
      stack_frame *alink = frame.access_link ();

      if (alink)
        alink->accept (*this);
    }

    void visit_user_fcn_stack_frame (user_fcn_stack_frame& frame)
    {
      append_list (frame);

      stack_frame *alink = frame.access_link ();

      if (alink)
        alink->accept (*this);
    }

    void visit_scope_stack_frame (scope_stack_frame& frame)
    {
      append_list (frame);

      stack_frame *alink = frame.access_link ();

      if (alink)
        alink->accept (*this);
    }

  private:

    typedef std::pair<std::string, symbol_info_list> syminf_list_elt;

    // FIXME: the following is too complex and duplicates too much
    // code.  Maybe it should be split up so we have separate classes
    // that do each job that is needed?

    std::list<symbol_record>
    filter (stack_frame& frame, const std::list<symbol_record>& symbols)
    {
      std::list<symbol_record> new_symbols;

      if (m_match_all)
        {
          for (const auto& sym : symbols)
            {
              if (frame.is_defined (sym))
                {
                  std::string name = sym.name ();

                  if (m_first_only
                      && m_found_names.find (name) != m_found_names.end ())
                    continue;

                  m_found_names.insert (name);

                  new_symbols.push_back (sym);
                }
            }
        }
      else if (m_have_regexp)
        {
          octave_idx_type npatterns = m_patterns.numel ();

          for (octave_idx_type j = 0; j < npatterns; j++)
            {
              std::string pattern = m_patterns[j];

              regexp pat (pattern);

              for (const auto& sym : symbols)
                {
                  std::string name = sym.name ();

                  if (pat.is_match (name) && frame.is_defined (sym))
                    {
                      if (m_first_only
                          && m_found_names.find (name) != m_found_names.end ())
                        continue;

                      m_found_names.insert (name);

                      new_symbols.push_back (sym);
                    }
                }
            }
        }
      else
        {
          octave_idx_type npatterns = m_patterns.numel ();

          for (octave_idx_type j = 0; j < npatterns; j++)
            {
              std::string pattern = m_patterns[j];

              glob_match pat (pattern);

              for (const auto& sym : symbols)
                {
                  std::string name = sym.name ();

                  if (pat.match (name) && frame.is_defined (sym))
                    {
                      if (m_first_only
                          && m_found_names.find (name) == m_found_names.end ())
                        continue;

                      m_found_names.insert (name);

                      new_symbols.push_back (sym);
                    }
                }
            }
        }

      return new_symbols;
    }

    void append_list (stack_frame& frame)
    {
      symbol_scope scope = frame.get_scope ();

      std::list<symbol_record> symbols = scope.symbol_list ();

      if (m_match_all || ! m_patterns.empty ())
        symbols = filter (frame, symbols);

      symbol_info_list syminf_list = frame.make_symbol_info_list (symbols);

      m_sym_inf_list.push_back (syminf_list_elt (scope.name (), syminf_list));
    }

    string_vector m_patterns;

    bool m_match_all;
    bool m_first_only;
    bool m_have_regexp;

    std::list<std::pair<std::string, symbol_info_list>> m_sym_inf_list;

    std::set<std::string> m_found_names;
  };
}

#endif
