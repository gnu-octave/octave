////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#if ! defined (octave_syminfo_h)
#define octave_syminfo_h 1

#include "octave-config.h"

#include <string>
#include <iosfwd>

#include "base-list.h"

#include "ov.h"

class octave_map;

OCTAVE_BEGIN_NAMESPACE(octave)

struct whos_parameter
{
  char command;
  char modifier;
  int parameter_length;
  int first_parameter_length;
  int balance;
  std::string text;
  std::string line;
};

class
OCTINTERP_API
symbol_info
{
public:

  symbol_info (const std::string& name, const octave_value& value,
               bool is_formal, bool is_global, bool is_persistent)
    : m_name (name), m_value (value), m_is_complex (value.iscomplex ()),
      m_is_sparse (value.issparse ()), m_is_formal (is_formal),
      m_is_global (is_global), m_is_persistent (is_persistent)
  { }

  std::string name (void) const { return m_name; }

  octave_value value (void) const { return m_value; }

  bool is_complex (void) const { return m_is_complex; }

  bool is_sparse (void) const { return m_is_sparse; }

  bool is_formal (void) const { return m_is_formal; }

  bool is_global (void) const { return m_is_global; }

  bool is_persistent (void) const { return m_is_persistent; }

  void display_line (std::ostream& os,
                     const std::list<whos_parameter>& params) const;
private:

  std::string m_name;
  octave_value m_value;
  bool m_is_complex;
  bool m_is_sparse;
  bool m_is_formal;
  bool m_is_global;
  bool m_is_persistent;
};

class
OCTINTERP_API
symbol_info_list : public base_list<symbol_info>
{
public:

  symbol_info_list (void) = default;

  symbol_info_list (const symbol_info_list&) = default;

  symbol_info_list& operator = (const symbol_info_list&) = default;

  ~symbol_info_list (void) = default;

  octave_value varval (const std::string& name) const;

  std::list<std::string> names (void) const;

  octave_map map_value (const std::string& caller_function_name,
                        int nesting_level) const;

  // Print a line of information for a given symbol.
  void print_descriptor (std::ostream& os,
                         const std::list<whos_parameter> params) const;

  void display (std::ostream& os, const std::string& format) const;

  // Parse FORMAT, and return a parameter list,
  // containing all information needed to print the given
  // attributes of the symbols.
  std::list<whos_parameter>
  parse_whos_line_format (const std::string& format) const;
};

OCTAVE_END_NAMESPACE(octave)

#endif
