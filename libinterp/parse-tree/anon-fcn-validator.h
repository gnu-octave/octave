////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#if ! defined (octave_anon_fcn_validator_h)
#define octave_anon_fcn_validator_h 1

#include "octave-config.h"

#include <string>

#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_expression;
class tree_parameter_list;

// How to check the semantics of the code that the parse trees represent.

class anon_fcn_validator : public tree_walker
{
public:

  anon_fcn_validator (tree_parameter_list *, tree_expression *expr);

  // No copying!

  anon_fcn_validator (const anon_fcn_validator&) = delete;

  anon_fcn_validator& operator = (const anon_fcn_validator&) = delete;

  ~anon_fcn_validator (void) = default;

  void visit_postfix_expression (tree_postfix_expression&);

  void visit_prefix_expression (tree_prefix_expression&);

  void visit_multi_assignment (tree_multi_assignment&);

  void visit_simple_assignment (tree_simple_assignment&);

  bool ok (void) const { return m_ok; }

  int line (void) const { return m_line; }
  int column (void) const { return m_column; }

  std::string message (void) const { return m_message; }

private:

  bool m_ok;
  int m_line;
  int m_column;
  std::string m_message;

  void error (tree_expression& expr);
};

OCTAVE_END_NAMESPACE(octave)

#endif
