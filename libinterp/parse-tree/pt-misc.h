////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_pt_misc_h)
#define octave_pt_misc_h 1

#include "octave-config.h"

#include "base-list.h"
#include "pt-decl.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class symbol_scope;
class tree_identifier;
class tree_index_expression;

// Parameter lists.  Used to hold the list of input and output
// parameters in a function definition.  Elements are identifiers
// only.

class tree_parameter_list : public base_list<tree_decl_elt *>
{
public:

  enum in_or_out
  {
    in = 1,
    out = 2
  };

  tree_parameter_list (in_or_out io)
    : m_in_or_out (io), m_marked_for_varargs (0)
  { }

  tree_parameter_list (in_or_out io, tree_decl_elt *t)
    : m_in_or_out (io), m_marked_for_varargs (0)
  {
    append (t);
  }

  tree_parameter_list (in_or_out io, tree_identifier *id)
    : m_in_or_out (io), m_marked_for_varargs (0)
  {
    append (new tree_decl_elt (id));
  }

  // No copying!

  tree_parameter_list (const tree_parameter_list&) = delete;

  tree_parameter_list& operator = (const tree_parameter_list&) = delete;

  ~tree_parameter_list (void);

  void mark_as_formal_parameters (void);

  void mark_varargs (void) { m_marked_for_varargs = 1; }

  void mark_varargs_only (void) { m_marked_for_varargs = -1; }

  bool takes_varargs (void) const { return m_marked_for_varargs != 0; }

  bool varargs_only (void) { return (m_marked_for_varargs < 0); }

  bool is_input_list (void) const { return m_in_or_out == in; }

  bool is_output_list (void) const { return m_in_or_out == out; }

  std::list<std::string> variable_names (void) const;

  std::string varargs_symbol_name (void) const
  {
    return m_in_or_out == in ? "varargin" : "varargout";
  }

  tree_parameter_list * dup (symbol_scope& scope) const;

  void accept (tree_walker& tw)
  {
    tw.visit_parameter_list (*this);
  }

private:

  in_or_out m_in_or_out;

  // 1: takes varargs
  // -1: takes varargs only
  // 0: does not take varargs.
  int m_marked_for_varargs;
};

OCTAVE_END_NAMESPACE(octave)

#endif
