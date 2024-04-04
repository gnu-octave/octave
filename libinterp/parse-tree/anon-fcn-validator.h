////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2024 The Octave Project Developers
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

#include "filepos.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_expression;
class tree_parameter_list;

// How to check the semantics of the code that the parse trees represent.

class anon_fcn_validator : public tree_walker
{
public:

  anon_fcn_validator (tree_parameter_list *, tree_expression *expr);

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (anon_fcn_validator)

  ~anon_fcn_validator () = default;

  void visit_postfix_expression (tree_postfix_expression&);

  void visit_prefix_expression (tree_prefix_expression&);

  void visit_multi_assignment (tree_multi_assignment&);

  void visit_simple_assignment (tree_simple_assignment&);

  bool ok () const { return m_ok; }

  filepos beg_pos () const { return m_beg_pos; }
  filepos end_pos () const { return m_end_pos; }

  std::string message () const { return m_message; }

private:

  bool m_ok {true};
  filepos m_beg_pos;
  filepos m_end_pos;
  std::string m_message;

  void error (tree_expression& expr);
};

OCTAVE_END_NAMESPACE(octave)

#endif
