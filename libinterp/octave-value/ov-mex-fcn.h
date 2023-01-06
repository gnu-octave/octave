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

#if ! defined (octave_ov_mex_fcn_h)
#define octave_ov_mex_fcn_h 1

#include "octave-config.h"

#include <string>

#include "oct-shlib.h"

#include "ov-fcn.h"
#include "ov-builtin.h"
#include "ov-typeinfo.h"

class octave_value;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

OCTAVE_END_NAMESPACE(octave)

// Dynamically-linked functions.

class
octave_mex_function : public octave_function
{
public:

  octave_mex_function (void)
    : m_mex_fcn_ptr (nullptr), m_exit_fcn_ptr (nullptr), m_sh_lib (),
      m_time_checked (), m_interleaved (false), m_is_fmex (false),
      m_is_system_fcn_file (false)
  { }

  octave_mex_function (void *fptr, bool interleaved, bool fmex,
                       const octave::dynamic_library& shl,
                       const std::string& nm = "");

  // No copying!

  octave_mex_function (const octave_mex_function& fcn) = delete;

  octave_mex_function& operator = (const octave_mex_function& fcn) = delete;

  ~octave_mex_function (void);

  octave_function * function_value (bool = false) { return this; }

  const octave_function * function_value (bool = false) const { return this; }

  void mark_fcn_file_up_to_date (const octave::sys::time& t)
  {
    m_time_checked = t;
  }

  std::string fcn_file_name (void) const;

  octave::sys::time time_parsed (void) const;

  octave::sys::time time_checked (void) const { return m_time_checked; }

  bool is_system_fcn_file (void) const { return m_is_system_fcn_file; }

  bool is_builtin_function (void) const { return false; }

  bool is_mex_function (void) const { return true; }

  bool use_interleaved_complex (void) const { return m_interleaved; }

  octave_value_list
  execute (octave::tree_evaluator& tw, int nargout = 0,
           const octave_value_list& args = octave_value_list ());

  void atexit (void (*fcn) (void)) { m_exit_fcn_ptr = fcn; }

  octave::dynamic_library get_shlib (void) const { return m_sh_lib; }

  void * mex_fcn_ptr (void) const { return m_mex_fcn_ptr; }

  bool is_fmex (void) const { return m_is_fmex; }

private:

  void *m_mex_fcn_ptr;

  void (*m_exit_fcn_ptr) (void);

  octave::dynamic_library m_sh_lib;

  // The time the file was last checked to see if it needs to be
  // parsed again.
  octave::sys::time m_time_checked;

  bool m_interleaved;

  bool m_is_fmex;

  // True if this function came from a file that is considered to be a
  // system function.  This affects whether we check the time stamp
  // on the file to see if it has changed.
  bool m_is_system_fcn_file;

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
