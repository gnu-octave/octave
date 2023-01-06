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

#if ! defined (octave_defun_dld_h)
#define octave_defun_dld_h 1

#include "octave-config.h"

#if defined (octave_defun_h)
#  error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

//! Macro to define an at run time dynamically loadable builtin function.
//!
//! For detailed information, see \ref Macros.
//!
//! @param name The **unquoted** name of the function that should be installed
//!             on the 'octave::symbol_table' and can be called by the
//!             interpreter.  Internally, the function name is prepended by an
//!             'F'.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this function.  If this value is
//!                  omitted, the function cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this function is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the function cannot access this number.
//! @param doc Texinfo help text (docstring) for the function.
//!
//! @see DEFMETHOD_DLD

// The order of this macro for name = foo is:
// 1. Forward declaration of Ffoo.
// 2. Definition of installation function Gfoo.
// 3. Definition of Ffoo.

#define DEFUN_DLD(name, args_name, nargout_name, doc)   \
  FORWARD_DECLARE_FUN (name);                           \
  DEFINE_FUN_INSTALLER_FUN (name, doc)                  \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFUNX_DLD(name, fname, gname, args_name, nargout_name, doc)    \
  FORWARD_DECLARE_FUNX (fname);                                         \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, gname, doc)                   \
  DECLARE_FUNX (fname, args_name, nargout_name)

//! Macro to define an at run time dynamically loadable builtin method.
//!
//! For detailed information, see \ref Macros.
//!
//! @param name The **unquoted** name of the method that should be installed
//!             on the 'octave::symbol_table' and can be called by the
//!             interpreter.  Internally, the method name is prepended by an
//!             'F'.
//! @param interp_name The name of the 'octave::interpreter' reference that can
//!                    be used by this method.  If this value is omitted,
//!                    there is no access to the interpreter and one should
//!                    use #DEFUN to define a function instead.
//! @param args_name The name of the octave_value_list variable used to pass
//!                  the argument list to this method.  If this value is
//!                  omitted, the method cannot access the argument list.
//! @param nargout_name The name of the 'int' variable used to pass the number
//!                     of output arguments this method is expected to
//!                     produce from the caller.  If this value is
//!                     omitted, the method cannot access this number.
//! @param doc Texinfo help text (docstring) for the method.
//!
//! @see DEFUN_DLD

// The order of this macro for name = foo is again:
// 1. Forward declaration of Ffoo.
// 2. Definition of installation function Gfoo.
// 3. Definition of Ffoo.

#define DEFMETHOD_DLD(name, interp_name, args_name, nargout_name, doc)  \
  FORWARD_DECLARE_METHOD (name);                                        \
  DEFINE_FUN_INSTALLER_FUN (name, doc)                                  \
  DECLARE_METHOD (name, interp_name, args_name, nargout_name)

#define DEFMETHODX_DLD(name, fname, gname, interp_name, args_name,      \
                       nargout_name, doc)                               \
  FORWARD_DECLARE_METHODX (fname);                                      \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, gname, doc)                   \
  DECLARE_METHODX (fname, interp_name, args_name, nargout_name)

// The same as the above, but declare the functions as static.
// NOTE: These macros should not be used directly.

#define DEFUN_STATIC_DLD(name, args_name, nargout_name, doc)    \
  FORWARD_DECLARE_STATIC_FUN (name);                            \
  DEFINE_FUN_INSTALLER_FUN (name, doc)                          \
  DECLARE_STATIC_FUN (name, args_name, nargout_name)

#define DEFUNX_STATIC_DLD(name, fname, gname, args_name,        \
                          nargout_name, doc)                    \
  FORWARD_DECLARE_STATIC_FUNX (fname);                          \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, gname, doc)           \
  DECLARE_STATIC_FUNX (fname, args_name, nargout_name)

#define DEFMETHOD_STATIC_DLD(name, interp_name, args_name,              \
                             nargout_name, doc)                         \
  FORWARD_DECLARE_STATIC_METHOD (name);                                 \
  DEFINE_FUN_INSTALLER_FUN (name, doc)                                  \
  DECLARE_STATIC_METHOD (name, interp_name, args_name, nargout_name)

#define DEFMETHODX_STATIC_DLD(name, fname, gname, interp_name,          \
                              args_name, nargout_name, doc)             \
  FORWARD_DECLARE_STATIC_METHODX (fname);                               \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, gname, doc)                   \
  DECLARE_STATIC_METHODX (fname, interp_name, args_name, nargout_name)

// The oct-conf-post.h file defines OCTAVE_USE_STATIC_DEFUN to force all
// dynamically loaded interpreter functions and methods to be static.

#if defined (OCTAVE_USE_STATIC_DEFUN)
#  undef DEFUN_DLD
#  undef DEFUNX_DLD
#  undef DEFMETHOD_DLD
#  undef DEFMETHODX_DLD

#  define DEFUN_DLD DEFUN_STATIC_DLD
#  define DEFUNX_DLD DEFUNX_STATIC_DLD
#  define DEFMETHOD_DLD DEFMETHOD_STATIC_DLD
#  define DEFMETHODX_DLD DEFMETHODX_STATIC_DLD
#endif

#endif
