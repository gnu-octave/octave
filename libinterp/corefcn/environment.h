////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if ! defined (octave_environment_h)
#define octave_environment_h 1

#include "octave-config.h"

#include <string>

class octave_value;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class environment
{
public:

  environment (void)
    : m_editor (init_editor ()),
      m_exec_path (init_exec_path ()),
      m_image_path (init_image_path ())
  { }

  octave_value editor (const octave_value_list& args, int nargout);

  std::string editor (void) const { return m_editor; }

  std::string editor (const std::string& ed)
  {
    return set (m_editor, ed);
  }

  octave_value exec_path (const octave_value_list& args, int nargout);

  std::string exec_path (void) const { return m_exec_path; }

  std::string exec_path (const std::string& path);

  octave_value image_path (const octave_value_list& args, int nargout);

  std::string image_path (void) const { return m_image_path; }

  std::string image_path (const std::string& path)
  {
    return set (m_image_path, path);
  }

private:

  std::string m_editor;

  std::string m_exec_path;

  std::string m_image_path;

  static std::string init_editor (void);

  static std::string init_exec_path (void);

  static std::string init_image_path (void);

  std::string set (std::string& var, const std::string& new_val)
  {
    std::string old_val = var;
    var = new_val;
    return old_val;
  }
};

OCTAVE_END_NAMESPACE(octave)

#endif
