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

#if ! defined (octave_pt_h)
#define octave_pt_h 1

#include "octave-config.h"

#include <string>

#include <iosfwd>

class octave_function;

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;
class tree_walker;

// Base class for the parse tree.

class tree
{
public:

  tree (int l = -1, int c = -1)
    : m_line_num (l), m_column_num (c), m_bp_cond (nullptr)
  { }

  // No copying!

  tree (const tree&) = delete;

  tree& operator = (const tree&) = delete;

  virtual ~tree (void) = default;

  virtual int line (void) const { return m_line_num; }

  virtual int column (void) const { return m_column_num; }

  void line (int l) { m_line_num = l; }

  void column (int c) { m_column_num = c; }

  void set_location (int l, int c)
  {
    m_line_num = l;
    m_column_num = c;
  }

  virtual void set_breakpoint (const std::string& condition)
  {
    if (m_bp_cond)
      *m_bp_cond = condition;
    else
      m_bp_cond = new std::string (condition);
  }

  virtual void delete_breakpoint (void)
  {
    if (m_bp_cond)
      {
        delete m_bp_cond;

        m_bp_cond = nullptr;
      }
  }

  bool meets_bp_condition (tree_evaluator& tw) const;

  bool is_breakpoint (void) const
  {
    return m_bp_cond;
  }

  bool is_active_breakpoint (tree_evaluator& tw) const
  {
    return m_bp_cond && meets_bp_condition (tw);
  }

  // breakpoint condition, or "0" (i.e., "false") if no breakpoint.
  // To distinguish "0" from a disabled breakpoint, test "is_breakpoint" too.
  const std::string bp_cond (void) const
  {
    return m_bp_cond ? *m_bp_cond : "0";
  }

  std::string str_print_code (void);

  virtual void accept (tree_walker& tw) = 0;

private:

  // The input line and column where we found the text that was
  // eventually converted to this tree node.
  int m_line_num;
  int m_column_num;

  // NULL if no breakpoint, or a breakpoint condition if there is one.
  std::string *m_bp_cond;
};

OCTAVE_END_NAMESPACE(octave)

#endif
