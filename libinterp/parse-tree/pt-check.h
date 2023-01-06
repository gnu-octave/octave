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

#if ! defined (octave_pt_check_h)
#define octave_pt_check_h 1

#include "octave-config.h"

#include <string>

#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_decl_command;

// How to check the semantics of the code that the parse trees represent.

class tree_checker : public tree_walker
{
public:

  tree_checker (void)
    : m_do_lvalue_check (false), m_file_name () { }

  // No copying!

  tree_checker (const tree_checker&) = delete;

  tree_checker& operator = (const tree_checker&) = delete;

  ~tree_checker (void) = default;

  void visit_argument_list (tree_argument_list&);

  void visit_simple_for_command (tree_simple_for_command&);

  void visit_complex_for_command (tree_complex_for_command&);

  void visit_multi_assignment (tree_multi_assignment&);

  void visit_simple_assignment (tree_simple_assignment&);

  void visit_try_catch_command (tree_try_catch_command&);

private:

  bool m_do_lvalue_check;

  std::string m_file_name;

  OCTAVE_NORETURN void errmsg (const std::string& msg, int line);
};

OCTAVE_END_NAMESPACE(octave)

#endif
