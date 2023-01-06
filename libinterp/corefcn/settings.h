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

#if ! defined (octave_settings_h)
#define octave_settings_h 1

#include "octave-config.h"

#include <cstddef>

class octave_value;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

// Most settings for the interpreter are stored in the classes which
// they affect (intput_system, output_system, load_path, etc.  Some
// don't really fit anywhere else.  For example, there is no single
// lexer or parser object, so we store settings for those things
// here.

class settings
{
public:

  settings (void);

  settings (const settings&) = delete;

  settings& operator = (const settings&) = delete;

  ~settings (void) = default;

  octave_value display_tokens (const octave_value_list& args, int nargout);

  bool display_tokens (void) const { return m_display_tokens; }

  bool display_tokens (bool flag)
  {
    bool val = m_display_tokens;
    m_display_tokens = flag;
    return val;
  }

  // Read only.
  std::size_t token_count (void) const { return m_token_count; }

  void increment_token_count (void) { ++m_token_count; }

  octave_value lexer_debug_flag (const octave_value_list& args, int nargout);

  bool lexer_debug_flag (void) const { return m_lexer_debug_flag; }

  bool lexer_debug_flag (bool flag)
  {
    bool val = m_lexer_debug_flag;
    m_lexer_debug_flag = flag;
    return val;
  }

private:

  // Display tokens as they are processed, for debugging.
  bool m_display_tokens = false;

  // Number of tokens processed since interpreter startup.
  std::size_t m_token_count = 0;

  // Internal variable for lexer debugging state.
  bool m_lexer_debug_flag = false;
};

OCTAVE_END_NAMESPACE(octave)

#endif
