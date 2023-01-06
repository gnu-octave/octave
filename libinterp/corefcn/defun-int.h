////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_defun_int_h)
#define octave_defun_int_h 1

#include "octave-config.h"

#include <string>

#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "version.h"

class octave_value;

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

extern OCTINTERP_API void print_usage (void);

extern OCTINTERP_API void print_usage (const std::string&);

extern OCTINTERP_API void check_version (const std::string& version,
                                         const std::string& fcn);

extern OCTINTERP_API void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
                      const dynamic_library& shl, const std::string& doc,
                      bool relative = false);

extern OCTINTERP_API void
install_dld_function (octave_dld_function::meth m, const std::string& name,
                      const dynamic_library& shl, const std::string& doc,
                      bool relative = false);

extern OCTINTERP_API void
install_mex_function (void *fptr, bool fmex, const std::string& name,
                      const dynamic_library& shl, bool relative = false);

// Gets the shlib of the currently executing DLD function, if any.
extern OCTINTERP_API dynamic_library get_current_shlib (void);

OCTAVE_END_NAMESPACE(octave)

// Some of these functions are widely used, so maybe we should avoid
// deprecating them for now?

inline void print_usage (void)
{
  octave::print_usage ();
}

inline void print_usage (const std::string& name)
{
  octave::print_usage (name);
}

inline void
check_version (const std::string& version, const std::string& fcn)
{
  octave::check_version (version, fcn);
}

inline void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
                      const octave::dynamic_library& shl,
                      const std::string& doc, bool relative = false)
{
  octave::install_dld_function (f, name, shl, doc, relative);
}

inline void
install_dld_function (octave_dld_function::meth m, const std::string& name,
                      const octave::dynamic_library& shl,
                      const std::string& doc, bool relative = false)
{
  octave::install_dld_function (m, name, shl, doc, relative);
}

inline void
install_mex_function (void *fptr, bool fmex, const std::string& name,
                      const octave::dynamic_library& shl,
                      bool relative = false)
{
  octave::install_mex_function (fptr, fmex, name, shl, relative);
}

// Gets the shlib of the currently executing DLD function, if any.
inline octave::dynamic_library get_current_shlib (void)
{
  return octave::get_current_shlib ();
}

#define FORWARD_DECLARE_FUNX(name)              \
  extern OCTAVE_EXPORT octave_value_list        \
  name (const octave_value_list&, int)

#define FORWARD_DECLARE_METHODX(name)                           \
  extern OCTAVE_EXPORT octave_value_list                        \
  name (octave::interpreter&, const octave_value_list&, int)

#define FORWARD_DECLARE_FUN(name)               \
  FORWARD_DECLARE_FUNX (F ## name)

#define FORWARD_DECLARE_METHOD(name)            \
  FORWARD_DECLARE_METHODX (F ## name)

#define DECLARE_FUNX(name, args_name, nargout_name)             \
  OCTAVE_EXPORT octave_value_list                               \
  name (const octave_value_list& args_name, int nargout_name)

#define DECLARE_METHODX(name, interp_name, args_name, nargout_name)     \
  OCTAVE_EXPORT octave_value_list                                       \
  name (octave::interpreter& interp_name,                               \
        const octave_value_list& args_name, int nargout_name)

#define DECLARE_FUN(name, args_name, nargout_name)      \
  DECLARE_FUNX (F ## name, args_name, nargout_name)

#define DECLARE_METHOD(name, interp_name, args_name, nargout_name)      \
  DECLARE_METHODX (F ## name, interp_name, args_name, nargout_name)

#define FORWARD_DECLARE_STATIC_FUNX(name)       \
  static octave_value_list                      \
  name (const octave_value_list&, int)

#define FORWARD_DECLARE_STATIC_METHODX(name)                    \
  static octave_value_list                                      \
  name (octave::interpreter&, const octave_value_list&, int)

#define FORWARD_DECLARE_STATIC_FUN(name)        \
  FORWARD_DECLARE_STATIC_FUNX (F ## name)

#define FORWARD_DECLARE_STATIC_METHOD(name)     \
  FORWARD_DECLARE_STATIC_METHODX (F ## name)

#define DECLARE_STATIC_FUNX(name, args_name, nargout_name)      \
  static octave_value_list                                      \
  name (const octave_value_list& args_name, int nargout_name)

#define DECLARE_STATIC_METHODX(name, interp_name, args_name, nargout_name) \
  static octave_value_list                                              \
  name (octave::interpreter& interp_name,                               \
        const octave_value_list& args_name, int nargout_name)

#define DECLARE_STATIC_FUN(name, args_name, nargout_name)       \
  DECLARE_STATIC_FUNX (F ## name, args_name, nargout_name)

#define DECLARE_STATIC_METHOD(name, interp_name, args_name, nargout_name) \
  DECLARE_STATIC_METHODX (F ## name, interp_name, args_name, nargout_name)

// Define the code that will be used to insert the new function into
// the symbol table.  We look for this name instead of the actual
// function so that we can easily install the doc std::string too.

typedef bool (*octave_dld_fcn_installer) (const octave::dynamic_library&, bool relative);

typedef octave_function *
  (*octave_dld_fcn_getter) (const octave::dynamic_library&, bool relative);

#if defined (OCTAVE_SOURCE)
#  define DEFINE_FUN_INSTALLER_FUN(name, doc)                           \
  DEFINE_FUNX_INSTALLER_FUN(#name, F ## name, G ## name, "external-doc")
#else
#  define DEFINE_FUN_INSTALLER_FUN(name, doc)                   \
  DEFINE_FUNX_INSTALLER_FUN(#name, F ## name, G ## name, doc)
#endif

#define DEFINE_FUNX_INSTALLER_FUN(name, fname, gname, doc)              \
  extern "C"                                                            \
  OCTAVE_EXPORT                                                         \
  octave_function *                                                     \
  gname (const octave::dynamic_library& shl, bool relative)             \
  {                                                                     \
    check_version (OCTAVE_API_VERSION, name);                           \
                                                                        \
    octave_dld_function *fcn                                            \
      = octave_dld_function::create (fname, shl, name, doc);            \
                                                                        \
    if (relative)                                                       \
      fcn->mark_relative ();                                            \
                                                                        \
    return fcn;                                                         \
  }

#endif
