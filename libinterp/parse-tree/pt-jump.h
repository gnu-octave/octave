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

#if ! defined (octave_pt_jump_h)
#define octave_pt_jump_h 1

#include "octave-config.h"

#include "pt-cmd.h"
#include "pt-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Break.

class tree_break_command : public tree_command
{
public:

  tree_break_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  // No copying!

  tree_break_command (const tree_break_command&) = delete;

  tree_break_command& operator = (const tree_break_command&) = delete;

  ~tree_break_command (void) = default;

  void accept (tree_walker& tw)
  {
    tw.visit_break_command (*this);
  }
};

// Continue.

class tree_continue_command : public tree_command
{
public:

  tree_continue_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  // No copying!

  tree_continue_command (const tree_continue_command&) = delete;

  tree_continue_command& operator = (const tree_continue_command&) = delete;

  ~tree_continue_command (void) = default;

  void accept (tree_walker& tw)
  {
    tw.visit_continue_command (*this);
  }
};

// Return.

class tree_return_command : public tree_command
{
public:

  tree_return_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  // No copying!

  tree_return_command (const tree_return_command&) = delete;

  tree_return_command& operator = (const tree_return_command&) = delete;

  ~tree_return_command (void) = default;

  void accept (tree_walker& tw)
  {
    tw.visit_return_command (*this);
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
