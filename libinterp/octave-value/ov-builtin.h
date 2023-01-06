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

#if ! defined (octave_ov_builtin_h)
#define octave_ov_builtin_h 1

#include "octave-config.h"

#include <list>
#include <set>
#include <string>

#include "ov-fcn.h"
#include "ov-typeinfo.h"

class octave_value;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;
class interpreter;

OCTAVE_END_NAMESPACE(octave)

// Builtin functions.

class
OCTINTERP_API
octave_builtin : public octave_function
{
public:

  octave_builtin (void)
    : octave_function (), m_fcn (nullptr), m_meth (nullptr), m_file ()
  { }

  typedef octave_value_list (*meth) (octave::interpreter&,
                                     const octave_value_list&, int);

  typedef octave_value_list (*fcn) (const octave_value_list&, int);

  octave_builtin (fcn ff, const std::string& nm = "",
                  const std::string& ds = "")
    : octave_function (nm, ds), m_fcn (ff), m_meth (nullptr), m_file ()
  { }

  octave_builtin (meth mm, const std::string& nm = "",
                  const std::string& ds = "")
    : octave_function (nm, ds), m_fcn (nullptr), m_meth (mm), m_file ()
  { }

  octave_builtin (fcn ff, const std::string& nm, const std::string& fnm,
                  const std::string& ds)
    : octave_function (nm, ds), m_fcn (ff), m_meth (nullptr), m_file (fnm)
  { }

  octave_builtin (meth mm, const std::string& nm, const std::string& fnm,
                  const std::string& ds)
    : octave_function (nm, ds), m_fcn (nullptr), m_meth (mm), m_file (fnm)
  { }

  // No copying!

  octave_builtin (const octave_builtin& ob) = delete;

  octave_builtin& operator = (const octave_builtin& ob) = delete;

  ~octave_builtin (void) = default;

  std::string src_file_name (void) const { return m_file; }

  octave_function * function_value (bool = false) { return this; }

  bool is_builtin_function (void) const { return true; }

  octave_value_list
  execute (octave::tree_evaluator& tw, int nargout = 0,
           const octave_value_list& args = octave_value_list ());

  fcn function (void) const;

  meth method (void) const;

  void push_dispatch_class (const std::string& dispatch_type);

  bool handles_dispatch_class (const std::string& dispatch_type) const;

protected:

  // A pointer to the actual function.
  fcn m_fcn;
  meth m_meth;

  // The name of the file where this function was defined.
  std::string m_file;

  // The types this function has been declared to handle (if any).
  std::set<std::string> m_dispatch_classes;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
