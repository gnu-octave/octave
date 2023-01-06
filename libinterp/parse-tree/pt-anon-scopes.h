////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if !defined (octave_pt_anon_scopes_h)
#define octave_pt_anon_scopes_h 1

#include <set>
#include <string>

#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// In possibly nested definitions of anonymous functions, collect
// their scopes and the symbol records therein.

class
tree_anon_scopes : public tree_walker
{
public:

  tree_anon_scopes (void) = delete;

  tree_anon_scopes (tree_anon_fcn_handle& anon_fh);

  // No copying!

  tree_anon_scopes (const tree_anon_scopes&) = delete;

  tree_anon_scopes& operator = (const tree_anon_scopes&) = delete;

  ~tree_anon_scopes (void) = default;

  std::set<std::string> fcn_parameters (void) const { return m_params; }

  std::set<std::string> free_variables (void) const { return m_vars; }

  // The following methods, though public, don't belong to the
  // intended user interface of this class.

  void visit_anon_fcn_handle (tree_anon_fcn_handle&);

  void visit_identifier (tree_identifier&);

  void visit_parameter_list (tree_parameter_list&);

  void visit_statement (tree_statement&);

  void visit_statement_list (tree_statement_list&);

private:

  // Variable names that are function parameters.
  std::set<std::string> m_params;

  // Other variable names.
  std::set<std::string> m_vars;
};

OCTAVE_END_NAMESPACE(octave)

#endif
