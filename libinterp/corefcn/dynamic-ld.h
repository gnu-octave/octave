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

#if ! defined (octave_dynamic_ld_h)
#define octave_dynamic_ld_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "oct-shlib.h"

class octave_function;

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class
dynamic_loader
{
private:

  class
  shlibs_list
  {
  public:

    typedef std::list<dynamic_library>::iterator iterator;
    typedef std::list<dynamic_library>::const_iterator const_iterator;

    shlibs_list (void) : m_lib_list () { }

    // No copying!

    shlibs_list (const shlibs_list&) = delete;

    shlibs_list& operator = (const shlibs_list&) = delete;

    ~shlibs_list (void) = default;

    void append (const dynamic_library& shl);

    std::list<std::string> remove (dynamic_library& shl);

    dynamic_library find_file (const std::string& file_name) const;

    void display (void) const;

  private:

    // List of libraries we have loaded.
    std::list<dynamic_library> m_lib_list;
  };


public:

  dynamic_loader (interpreter& interp)
    : m_interpreter (interp), m_loaded_shlibs (), m_doing_load (false)
  { }

  // No copying!

  dynamic_loader (const dynamic_loader&) = delete;

  dynamic_loader& operator = (const dynamic_loader&) = delete;

  virtual ~dynamic_loader (void) = default;

  octave_function *
  load_oct (const std::string& fcn_name,
            const std::string& file_name = "",
            bool relative = false);

  octave_function *
  load_mex (const std::string& fcn_name,
            const std::string& file_name = "",
            bool relative = false);

  bool remove_oct (const std::string& fcn_name,
                   dynamic_library& shl);

  bool remove_mex (const std::string& fcn_name,
                   dynamic_library& shl);

private:

  void clear_function (const std::string& fcn_name);

  void clear (dynamic_library& oct_file);

  interpreter& m_interpreter;

  shlibs_list m_loaded_shlibs;

  bool m_doing_load;

  static std::string name_mangler (const std::string& name);

  static std::string name_uscore_mangler (const std::string& name);

  static std::string mex_mangler (const std::string& name);

  static std::string mex_uscore_mangler (const std::string& name);

  static std::string mex_f77_mangler (const std::string& name);

  static void * try_load_mex (dynamic_library& mex_file,
                             const std::string& fcn_name, bool& have_fmex);
};

OCTAVE_END_NAMESPACE(octave)

#endif
