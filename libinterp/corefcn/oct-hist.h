////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_oct_hist_h)
#define octave_oct_hist_h 1

#include "octave-config.h"

#include <string>

#include "cmd-hist.h"

#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class OCTINTERP_API history_system
{
public:

  history_system (interpreter& interp);

  history_system (const history_system&) = delete;

  history_system& operator = (const history_system&) = delete;

  ~history_system (void) = default;

  void initialize (bool read_history_file = false);

  void write_timestamp (void);

  octave_value input_from_tmp_file (const octave_value_list& args,
                                    int nargout);

  bool input_from_tmp_file (void) const
  {
    return m_input_from_tmp_file;
  }

  bool input_from_tmp_file (bool flag)
  {
    return set (m_input_from_tmp_file, flag);
  }

  octave_value timestamp_format_string (const octave_value_list& args,
                                        int nargout);

  std::string timestamp_format_string (void) const
  {
    return m_timestamp_format_string;
  }

  std::string timestamp_format_string (const std::string& file)
  {
    return set (m_timestamp_format_string, file);
  }

  string_vector
  do_history (const octave_value_list& args = octave_value_list (),
              int nargout = 0);

  void do_edit_history (const octave_value_list& args = octave_value_list ());

  void do_run_history (const octave_value_list& args = octave_value_list ());

private:

  interpreter& m_interpreter;

  // TRUE means input is coming from temporary history file.
  bool m_input_from_tmp_file;

  // The format of the timestamp marker written to the history file when
  // Octave exits.
  std::string m_timestamp_format_string;

  static std::string default_file (void);

  static int default_size (void);

  static std::string default_timestamp_format (void);

  template <typename T>
  T set (T& var, const T& new_val)
  {
    T old_val = var;
    var = new_val;
    return old_val;
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
