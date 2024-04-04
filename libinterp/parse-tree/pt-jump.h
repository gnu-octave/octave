////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

// Base class for jump commands

class tree_jump_command : public tree_command
{
public:

  tree_jump_command (const token& tok) : m_token (tok) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_jump_command)

  ~tree_jump_command () = default;

  filepos beg_pos () const { return m_token.beg_pos (); }
  filepos end_pos () const { return m_token.end_pos (); }

protected:

  token m_token;
};

// Break.

class tree_break_command : public tree_jump_command
{
public:

  tree_break_command (const token& tok) : tree_jump_command (tok) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_break_command)

  ~tree_break_command () = default;

  void accept (tree_walker& tw)
  {
    tw.visit_break_command (*this);
  }
};

// Continue.

class tree_continue_command : public tree_jump_command
{
public:

  tree_continue_command (const token& tok) : tree_jump_command (tok) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_continue_command)

  ~tree_continue_command () = default;

  void accept (tree_walker& tw)
  {
    tw.visit_continue_command (*this);
  }
};

// Return.

class tree_return_command : public tree_jump_command
{
public:

  tree_return_command (const token& tok) : tree_jump_command (tok) { }

  OCTAVE_DISABLE_COPY_MOVE (tree_return_command)

  ~tree_return_command () = default;

  void accept (tree_walker& tw)
  {
    tw.visit_return_command (*this);
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
