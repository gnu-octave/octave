////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2024 The Octave Project Developers
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

#if ! defined (octave_pt_cmd_h)
#define octave_pt_cmd_h 1

#include "octave-config.h"

#include <string>

#include "comment-list.h"
#include "ov-fcn.h"
#include "pt.h"
#include "pt-bp.h"
#include "pt-walk.h"
#include "panic.h"
#include "token.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// A base class for commands.

class tree_command : public tree
{
public:

  tree_command () = default;

  OCTAVE_DISABLE_COPY_MOVE (tree_command)

  virtual ~tree_command () = default;

  virtual void update_end_pos (const filepos&) { panic_impossible (); }
};

// No-op.

class tree_no_op_command : public tree_command
{
public:

  tree_no_op_command (const std::string& cmd, bool eof, const token& tok)
    : m_eof (eof), m_tok (tok), m_orig_cmd (cmd)
  { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_no_op_command)

  ~tree_no_op_command () = default;

  filepos beg_pos () const { return m_tok.beg_pos (); }
  filepos end_pos () const { return m_tok.end_pos (); }

  void update_end_pos (const filepos& pos)
  {
    if (is_end_of_fcn_or_script () || is_end_of_file ())
      m_tok.end_pos (pos);
    else
      panic_impossible ();
  }

  comment_list leading_comments () const { return m_tok.leading_comments (); }

  void attach_trailing_comments (const comment_list& lst)
  {
    m_tok.trailing_comments (lst);
  }

  comment_list trailing_comments () const
  {
    return m_tok.trailing_comments ();
  }

  void accept (tree_walker& tw)
  {
    tw.visit_no_op_command (*this);
  }

  bool is_end_of_fcn_or_script () const
  {
    return (m_orig_cmd == "endfunction" || m_orig_cmd == "endscript");
  }

  bool is_end_of_file () const { return m_eof; }

  std::string original_command () { return m_orig_cmd; }

private:

  bool m_eof;

  // If defined, may be END token or EOF.
  token m_tok;

  std::string m_orig_cmd;
};

// Function definition.

class tree_function_def : public tree_command
{
public:

  tree_function_def (octave_function *f) : m_fcn (f) { }

  OCTAVE_DISABLE_CONSTRUCT_COPY_MOVE (tree_function_def)

  ~tree_function_def () = default;

  filepos beg_pos () const
  {
    octave_function *f = m_fcn.function_value ();
    return f->beg_pos ();
  }

  filepos end_pos () const
  {
    octave_function *f = m_fcn.function_value ();
    return f->end_pos ();
  }

  void accept (tree_walker& tw)
  {
    tw.visit_function_def (*this);
  }

  octave_value function () { return m_fcn; }

private:

  octave_value m_fcn;

  tree_function_def (const octave_value& v) : m_fcn (v) { }
};

OCTAVE_END_NAMESPACE(octave)

#endif
