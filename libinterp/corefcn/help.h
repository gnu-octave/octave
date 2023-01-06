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

#if ! defined (octave_help_h)
#define octave_help_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

class string_vector;

class octave_value;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class help_system
{
public:

  help_system (interpreter& interp)
    : m_interpreter (interp),
      m_built_in_docstrings_file (init_built_in_docstrings_file ()),
      m_doc_cache_file (init_doc_cache_file ()),
      m_info_file (init_info_file ()),
      m_info_program (init_info_program ()),
      m_makeinfo_program ("makeinfo"),
      m_suppress_verbose_help_message (false),
      m_texi_macros_file (init_texi_macros_file ())
  { }

  octave_value
  built_in_docstrings_file (const octave_value_list& args, int nargout);

  std::string
  built_in_docstrings_file (void) const { return m_built_in_docstrings_file; }

  std::string built_in_docstrings_file (const std::string& file)
  {
    return set (m_built_in_docstrings_file, file);
  }

  octave_value doc_cache_file (const octave_value_list& args, int nargout);

  std::string doc_cache_file (void) const { return m_doc_cache_file; }

  std::string doc_cache_file (const std::string& file)
  {
    return set (m_doc_cache_file, file);
  }

  octave_value info_file (const octave_value_list& args, int nargout);

  std::string info_file (void) const { return m_info_file; }

  std::string info_file (const std::string& file)
  {
    return set (m_info_file, file);
  }

  octave_value info_program (const octave_value_list& args, int nargout);

  std::string info_program (void) const { return m_info_program; }

  std::string info_program (const std::string& file)
  {
    return set (m_info_program, file);
  }

  octave_value makeinfo_program (const octave_value_list& args, int nargout);

  std::string makeinfo_program (void) const { return m_makeinfo_program; }

  std::string makeinfo_program (const std::string& file)
  {
    return set (m_makeinfo_program, file);
  }

  octave_value
  suppress_verbose_help_message (const octave_value_list& args, int nargout);

  bool suppress_verbose_help_message (void) const
  {
    return m_suppress_verbose_help_message;
  }

  bool suppress_verbose_help_message (bool flag)
  {
    return set (m_suppress_verbose_help_message, flag);
  }

  octave_value texi_macros_file (const octave_value_list& args, int nargout);

  std::string texi_macros_file (void) const { return m_texi_macros_file; }

  std::string texi_macros_file (const std::string& file)
  {
    return set (m_texi_macros_file, file);
  }

  std::string raw_help (const std::string&, bool&) const;

  std::string which (const std::string& name) const;
  std::string which (const std::string& name, std::string& type) const;

  string_vector make_name_list (void) const;

  void get_help_text (const std::string& name, std::string& text,
                      std::string& format) const;

  void get_help_text_from_file (const std::string& fname, std::string& text,
                                std::string& format) const;

private:

  interpreter& m_interpreter;

  // Name of the file containing doc strings for built-in functions.
  // (--built-in-docstrings-file file)
  std::string m_built_in_docstrings_file;

  // Name of the doc cache file specified on the command line.
  // (--doc-cache-file file)
  std::string m_doc_cache_file;

  // Name of the info file specified on command line.
  // (--info-file file)
  std::string m_info_file;

  // Name of the info reader we'd like to use.
  // (--info-program program)
  std::string m_info_program;

  // Name of the makeinfo program to run.
  std::string m_makeinfo_program;

  // If TRUE, don't print additional help message in help and usage
  // functions.
  bool m_suppress_verbose_help_message;

  // Name of the file containing local Texinfo macros that are prepended
  // to doc strings before processing.
  // (--texi-macros-file)
  std::string m_texi_macros_file;

  static std::string init_built_in_docstrings_file (void);

  static std::string init_doc_cache_file (void);

  static std::string init_info_file (void);

  static std::string init_info_program (void);

  static std::string init_texi_macros_file (void);

  template <typename T>
  T set (T& var, const T& new_val)
  {
    T old_val = var;
    var = new_val;
    return old_val;
  }

  string_vector local_functions (void) const;

  bool raw_help_from_symbol_table (const std::string& nm,
                                   std::string& h, std::string& w,
                                   bool& symbol_found) const;

  bool raw_help_from_file (const std::string& nm,
                           std::string& h, std::string& file,
                           bool& symbol_found) const;

  bool raw_help_from_docstrings_file (const std::string& nm, std::string& h,
                                      bool& symbol_found) const;
};

extern string_vector make_name_list (void);

OCTAVE_END_NAMESPACE(octave)

#endif
